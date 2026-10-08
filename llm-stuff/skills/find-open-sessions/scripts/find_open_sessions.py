#!/usr/bin/env python3
"""Report live Claude Code, Codex, and Pi terminal sessions on macOS."""

from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Callable, Iterable


PROVIDERS = ("claude", "codex", "pi")
UUID_RE = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
    re.IGNORECASE,
)
PS_ROW_RE = re.compile(r"^\s*(\d+)\s+(\d+)\s+(\S+)\s+(.*)$")
START_ROW_RE = re.compile(r"^\s*(\d+)\s+(.+?)\s*$")
START_FORMATS = ("%a %b %d %H:%M:%S %Y", "%a %d %b %H:%M:%S %Y")
MISSING_TTYS = {"", "?", "??", "-"}
MIN_BIRTH_GAP = -120.0
MAX_BIRTH_GAP = 600.0
HIGH_CONFIDENCE_GAP = 90.0
AMBIGUITY_SECONDS = 5.0
COMMAND_TIMEOUT = 20.0


@dataclass(frozen=True)
class CommandResult:
    args: tuple[str, ...]
    returncode: int | None
    stdout: str
    stderr: str
    timed_out: bool = False
    error: str | None = None


@dataclass(frozen=True)
class TranscriptAdapter:
    provider: str
    root: Path
    discover: Callable[[Path, list[dict], list[str]], list[dict]]


class InspectionError(RuntimeError):
    def __init__(self, message: str, exit_code: int = 1):
        super().__init__(message)
        self.exit_code = exit_code


def run_command(args: Iterable[str], timeout: float = COMMAND_TIMEOUT) -> CommandResult:
    argv = tuple(args)
    try:
        result = subprocess.run(
            argv, capture_output=True, text=True, timeout=timeout, check=False
        )
        return CommandResult(argv, result.returncode, result.stdout, result.stderr)
    except subprocess.TimeoutExpired as exc:
        return CommandResult(
            argv,
            None,
            exc.stdout if isinstance(exc.stdout, str) else "",
            exc.stderr if isinstance(exc.stderr, str) else "",
            timed_out=True,
        )
    except OSError as exc:
        return CommandResult(argv, None, "", "", error=str(exc))


def split_argv(command: str) -> list[str]:
    try:
        return shlex.split(command)
    except ValueError:
        return command.split()


def _has_flag(argv: list[str], flag: str) -> bool:
    return any(arg == flag or arg.startswith(flag + "=") for arg in argv[1:])


def _has_pair(argv: list[str], first: str, second: str) -> bool:
    return any(a == first and b == second for a, b in zip(argv[1:], argv[2:]))


def launch_mode(provider: str, argv: list[str]) -> str:
    if provider == "claude":
        if _has_flag(argv, "--fork-session"):
            return "fork"
        if _has_flag(argv, "--resume"):
            return "resume"
        return "fresh"
    if provider == "codex":
        if len(argv) > 1 and argv[1] == "fork":
            return "fork"
        if len(argv) > 1 and argv[1] == "resume":
            return "resume"
        return "fresh"
    if _has_flag(argv, "--fork"):
        return "fork"
    if _has_flag(argv, "--continue"):
        return "continue"
    if _has_flag(argv, "--resume") or _has_flag(argv, "--session"):
        return "resume"
    return "fresh"


def classify_process(pid: int, ppid: int, tty: str, command: str) -> dict | None:
    """Return a sanitized candidate record, or None for a non-interactive process."""
    argv = split_argv(command)
    if not argv or tty in MISSING_TTYS:
        return None
    provider = os.path.basename(argv[0])
    if provider not in PROVIDERS:
        return None

    tail = argv[1:]
    if provider == "claude":
        if any(arg == "-p" for arg in tail) or any(
            _has_flag(argv, flag) for flag in ("--print", "--bg", "--background")
        ):
            return None
        if any(arg.startswith("--bg-") or arg.startswith("--background=") for arg in tail):
            return None
        if _has_pair(argv, "daemon", "run"):
            return None
    elif provider == "codex":
        if tail and tail[0] in {"exec", "review", "mcp-server", "app-server", "exec-server"}:
            return None
    elif any(arg == "-p" for arg in tail) or _has_flag(argv, "--print"):
        return None

    return {
        "provider": provider,
        "pid": pid,
        "ppid": ppid,
        "tty": tty,
        "argv": argv,
        "launch_mode": launch_mode(provider, argv),
    }


def parse_process_table(output: str) -> tuple[list[dict], dict[int, int]]:
    candidates: list[dict] = []
    parents: dict[int, int] = {}
    for line in output.splitlines():
        match = PS_ROW_RE.match(line)
        if not match:
            continue
        pid_text, ppid_text, tty, command = match.groups()
        pid, ppid = int(pid_text), int(ppid_text)
        parents[pid] = ppid
        candidate = classify_process(pid, ppid, tty, command)
        if candidate:
            candidates.append(candidate)
    return candidates, parents


def normalize_session_arg(value: str) -> str | None:
    value = value.strip().strip("'\"")
    if value.endswith(".jsonl") or "/" in value:
        value = Path(value).stem
    return value.lower() if UUID_RE.fullmatch(value) else None


def _flag_value(argv: list[str], flags: set[str]) -> str | None:
    for index, arg in enumerate(argv[1:], start=1):
        if arg in flags and index + 1 < len(argv):
            return argv[index + 1]
        for flag in flags:
            if arg.startswith(flag + "="):
                return arg.split("=", 1)[1]
    return None


def exact_session_id(provider: str, argv: list[str]) -> str | None:
    if provider == "claude":
        value = _flag_value(argv, {"--session-id"})
        if value:
            return normalize_session_arg(value)
        if not _has_flag(argv, "--fork-session"):
            value = _flag_value(argv, {"--resume"})
            return normalize_session_arg(value) if value else None
        return None
    if provider == "codex":
        if len(argv) > 2 and argv[1] == "resume":
            return normalize_session_arg(argv[2])
        return None
    value = _flag_value(argv, {"--session-id"})
    if value:
        return normalize_session_arg(value)
    if not _has_flag(argv, "--fork"):
        value = _flag_value(argv, {"--session", "--resume"})
        return normalize_session_arg(value) if value else None
    return None


def find_current_pid(
    start_pid: int, candidate_pids: set[int], parent_by_pid: dict[int, int]
) -> int | None:
    pid = start_pid
    seen: set[int] = set()
    while pid and pid not in seen:
        if pid in candidate_pids:
            return pid
        seen.add(pid)
        pid = parent_by_pid.get(pid, 0)
    return None


def parse_lsof_cwds(output: str) -> dict[int, str]:
    result: dict[int, str] = {}
    current: int | None = None
    for line in output.splitlines():
        if line.startswith("p") and line[1:].isdigit():
            current = int(line[1:])
        elif line.startswith("n") and current is not None:
            result[current] = os.path.realpath(line[1:])
    return result


def parse_start_time(value: str) -> float | None:
    for fmt in START_FORMATS:
        try:
            return datetime.strptime(value.strip(), fmt).timestamp()
        except ValueError:
            continue
    return None


def parse_start_times(output: str) -> dict[int, float]:
    result: dict[int, float] = {}
    for line in output.splitlines():
        match = START_ROW_RE.match(line)
        if not match:
            continue
        parsed = parse_start_time(match.group(2))
        if parsed is not None:
            result[int(match.group(1))] = parsed
    return result


def inspect_processes() -> tuple[list[dict], dict[int, int], list[str]]:
    result = run_command(("ps", "-axo", "pid=,ppid=,tty=,command="))
    if result.timed_out:
        raise InspectionError("Unable to inspect live processes: ps timed out.")
    if result.error:
        detail = result.error.lower()
        if "operation not permitted" in detail or "permission denied" in detail:
            raise InspectionError(
                "Unable to inspect live processes: ps was denied by the current sandbox."
            )
        raise InspectionError(f"Unable to inspect live processes: {result.error}")
    if result.returncode != 0:
        detail = result.stderr.strip().lower()
        if "operation not permitted" in detail or "permission denied" in detail:
            raise InspectionError(
                "Unable to inspect live processes: ps was denied by the current sandbox."
            )
        raise InspectionError(
            f"Unable to inspect live processes: ps exited {result.returncode}."
        )

    candidates, parents = parse_process_table(result.stdout)
    warnings: list[str] = []
    if not candidates:
        return candidates, parents, warnings

    pid_text = ",".join(str(proc["pid"]) for proc in candidates)
    lsof = run_command(("lsof", "-a", "-p", pid_text, "-d", "cwd", "-Fpn"))
    cwd_by_pid = parse_lsof_cwds(lsof.stdout)
    if lsof.timed_out:
        warnings.append("lsof timed out; process cwd values may be unavailable")
    elif lsof.error:
        warnings.append(f"lsof failed; process cwd values may be unavailable: {lsof.error}")
    elif lsof.returncode not in (0, None):
        warnings.append(
            f"lsof exited {lsof.returncode}; process cwd values may be unavailable"
        )

    starts = run_command(("ps", "-p", pid_text, "-o", "pid=", "-o", "lstart="))
    start_by_pid = parse_start_times(starts.stdout)
    if starts.timed_out:
        warnings.append("process start-time inspection timed out")
    elif starts.error:
        warnings.append(f"process start-time inspection failed: {starts.error}")
    elif starts.returncode != 0:
        warnings.append(f"process start-time ps exited {starts.returncode}")

    for proc in candidates:
        pid = proc["pid"]
        proc["cwd"] = cwd_by_pid.get(pid)
        proc["start"] = start_by_pid.get(pid)
        if proc["cwd"] is None:
            warnings.append(f"cwd unavailable for {proc['provider']} pid {pid}")
        if proc["start"] is None:
            warnings.append(f"start time unavailable for {proc['provider']} pid {pid}")
    return candidates, parents, warnings


def _read_first_json(path: Path) -> tuple[dict | None, str | None]:
    try:
        with path.open(encoding="utf-8") as handle:
            line = handle.readline()
        obj = json.loads(line)
        if not isinstance(obj, dict):
            return None, "first record is not an object"
        return obj, None
    except OSError as exc:
        return None, str(exc)
    except json.JSONDecodeError as exc:
        return None, f"malformed first record: {exc.msg}"


def _bounded_explicit_cwd(path: Path, max_lines: int = 64, max_bytes: int = 262_144) -> str | None:
    try:
        read_bytes = 0
        with path.open(encoding="utf-8") as handle:
            for index, line in enumerate(handle):
                if index >= max_lines or read_bytes >= max_bytes:
                    break
                read_bytes += len(line.encode("utf-8", errors="ignore"))
                try:
                    obj = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if not isinstance(obj, dict):
                    continue
                cwd = obj.get("cwd")
                if not cwd and isinstance(obj.get("payload"), dict):
                    cwd = obj["payload"].get("cwd")
                if isinstance(cwd, str) and cwd:
                    return os.path.realpath(cwd)
    except OSError:
        return None
    return None


def _birth_time(path: Path, provider: str, warnings: list[str]) -> float | None:
    try:
        stat_result = path.stat()
    except OSError as exc:
        warnings.append(f"unable to stat {provider} transcript {path.name}: {exc}")
        return None
    value = getattr(stat_result, "st_birthtime", None)
    if value is None:
        warnings.append(
            f"{provider} transcript birth time unavailable; using mtime defensively"
        )
        value = stat_result.st_mtime
    return float(value)


def _record(
    provider: str,
    path: Path,
    session_id: str,
    cwd: str | None,
    warnings: list[str],
) -> dict | None:
    birth = _birth_time(path, provider, warnings)
    if birth is None:
        return None
    return {
        "provider": provider,
        "session_id": session_id.lower(),
        "cwd": os.path.realpath(cwd) if cwd else None,
        "birth": birth,
        "path": str(path.resolve()),
    }


def _unique_paths(paths: Iterable[Path]) -> list[Path]:
    return list(dict.fromkeys(paths))


def discover_claude(root: Path, processes: list[dict], warnings: list[str]) -> list[dict]:
    records: list[dict] = []
    seen_paths: set[Path] = set()
    cwd_values = sorted({p["cwd"] for p in processes if p.get("cwd")})
    missing_cwds: set[str] = set()

    for cwd in cwd_values:
        project_dir = root / re.sub(r"[/\.]", "-", cwd)
        if not project_dir.is_dir():
            missing_cwds.add(cwd)
            continue
        try:
            paths = sorted(project_dir.glob("*.jsonl"))
        except OSError as exc:
            warnings.append(f"unable to scan Claude project directory: {exc}")
            continue
        for path in paths:
            if not UUID_RE.fullmatch(path.stem):
                continue
            item = _record("claude", path, path.stem, cwd, warnings)
            if item:
                records.append(item)
                seen_paths.add(path)

    exact_ids = {
        p["exact_id"] for p in processes if p.get("exact_id") and p.get("exact_id")
    }
    for session_id in sorted(exact_ids):
        try:
            paths = root.glob(f"*/{session_id}.jsonl")
            for path in paths:
                if path in seen_paths:
                    continue
                cwd = _bounded_explicit_cwd(path)
                item = _record("claude", path, session_id, cwd, warnings)
                if item:
                    records.append(item)
                    seen_paths.add(path)
        except OSError as exc:
            warnings.append(f"unable to resolve Claude session {session_id}: {exc}")

    if missing_cwds:
        recent: list[tuple[float, Path]] = []
        try:
            for path in root.glob("*/*.jsonl"):
                if path in seen_paths or not UUID_RE.fullmatch(path.stem):
                    continue
                try:
                    recent.append((path.stat().st_mtime, path))
                except OSError:
                    continue
        except OSError as exc:
            warnings.append(f"unable to perform bounded Claude cwd fallback: {exc}")
            return records
        for _, path in sorted(recent, reverse=True)[:300]:
            cwd = _bounded_explicit_cwd(path)
            if cwd not in missing_cwds:
                continue
            item = _record("claude", path, path.stem, cwd, warnings)
            if item:
                records.append(item)
                seen_paths.add(path)
    return records


def _relevant_dates(processes: list[dict]) -> set[date]:
    result: set[date] = set()
    for proc in processes:
        if proc.get("start") is None:
            continue
        local_date = datetime.fromtimestamp(proc["start"]).date()
        result.update(
            {local_date - timedelta(days=1), local_date, local_date + timedelta(days=1)}
        )
    if not result:
        today = date.today()
        result.update({today - timedelta(days=1), today, today + timedelta(days=1)})
    return result


def discover_codex(root: Path, processes: list[dict], warnings: list[str]) -> list[dict]:
    paths: list[Path] = []
    for day in sorted(_relevant_dates(processes)):
        directory = root / f"{day:%Y}" / f"{day:%m}" / f"{day:%d}"
        if directory.is_dir():
            paths.extend(sorted(directory.glob("*.jsonl")))
    exact_ids = {p["exact_id"] for p in processes if p.get("exact_id")}
    for session_id in sorted(exact_ids):
        paths.extend(root.glob(f"*/*/*/*{session_id}.jsonl"))

    records: list[dict] = []
    for path in _unique_paths(paths):
        obj, error = _read_first_json(path)
        if error:
            warnings.append(f"skipping Codex transcript {path.name}: {error}")
            continue
        payload = obj.get("payload") if obj else None
        if not isinstance(payload, dict):
            warnings.append(f"skipping Codex transcript {path.name}: missing payload")
            continue
        session_id = payload.get("id") or payload.get("session_id")
        cwd = payload.get("cwd")
        if not isinstance(session_id, str) or not UUID_RE.fullmatch(session_id):
            warnings.append(f"skipping Codex transcript {path.name}: invalid session id")
            continue
        if not isinstance(cwd, str) or not cwd:
            warnings.append(f"skipping Codex transcript {path.name}: missing cwd")
            continue
        item = _record("codex", path, session_id, cwd, warnings)
        if item:
            records.append(item)
    return records


def discover_pi(root: Path, processes: list[dict], warnings: list[str]) -> list[dict]:
    del processes
    records: list[dict] = []
    try:
        paths = sorted(root.glob("*/*.jsonl"))
    except OSError as exc:
        warnings.append(f"unable to scan Pi transcripts: {exc}")
        return records
    for path in paths:
        obj, error = _read_first_json(path)
        if error:
            warnings.append(f"skipping Pi transcript {path.name}: {error}")
            continue
        if not obj or obj.get("type") != "session":
            warnings.append(f"skipping Pi transcript {path.name}: invalid session record")
            continue
        session_id, cwd = obj.get("id"), obj.get("cwd")
        if not isinstance(session_id, str) or not UUID_RE.fullmatch(session_id):
            warnings.append(f"skipping Pi transcript {path.name}: invalid session id")
            continue
        if not isinstance(cwd, str) or not cwd:
            warnings.append(f"skipping Pi transcript {path.name}: missing cwd")
            continue
        item = _record("pi", path, session_id, cwd, warnings)
        if item:
            records.append(item)
    return records


def make_adapters() -> dict[str, TranscriptAdapter]:
    home = Path.home()
    return {
        "claude": TranscriptAdapter(
            "claude", home / ".claude" / "projects", discover_claude
        ),
        "codex": TranscriptAdapter(
            "codex", home / ".codex" / "sessions", discover_codex
        ),
        "pi": TranscriptAdapter(
            "pi", home / ".pi" / "agent" / "sessions", discover_pi
        ),
    }


def assign_birth_matches(
    processes: list[dict], records: list[dict], warnings: list[str]
) -> None:
    """Mutate process mapping fields using a deterministic one-to-one assignment."""
    used = {
        (p["provider"], p["session_id"])
        for p in processes
        if p.get("session_id") is not None
    }
    candidates_by_pid: dict[int, list[tuple[float, str, int, float]]] = {}
    edges: list[tuple[float, str, int, str, int, float]] = []

    for p_index, proc in enumerate(processes):
        if proc.get("session_id") or proc.get("cwd") is None or proc.get("start") is None:
            continue
        for r_index, record in enumerate(records):
            key = (record["provider"], record["session_id"])
            if key in used or record["provider"] != proc["provider"]:
                continue
            if record.get("cwd") != proc["cwd"]:
                continue
            gap = record["birth"] - proc["start"]
            if MIN_BIRTH_GAP <= gap <= MAX_BIRTH_GAP:
                distance = abs(gap)
                edge = (distance, record["session_id"], r_index, gap)
                candidates_by_pid.setdefault(proc["pid"], []).append(edge)
                edges.append(
                    (distance, proc["provider"], proc["pid"], record["session_id"], r_index, gap)
                )

    ambiguous_pids: set[int] = set()
    for proc in processes:
        choices = sorted(candidates_by_pid.get(proc["pid"], []))
        if len(choices) >= 2 and choices[1][0] - choices[0][0] <= AMBIGUITY_SECONDS:
            ambiguous_pids.add(proc["pid"])
            warnings.append(
                f"ambiguous birth match for {proc['provider']} pid {proc['pid']}"
            )

    assigned_pids: set[int] = set()
    for distance, provider, pid, session_id, r_index, gap in sorted(edges):
        key = (provider, session_id)
        if pid in assigned_pids or key in used:
            continue
        proc = next(item for item in processes if item["pid"] == pid)
        record = records[r_index]
        proc.update(
            {
                "session_id": session_id,
                "confidence": (
                    "high"
                    if distance <= HIGH_CONFIDENCE_GAP and pid not in ambiguous_pids
                    else "low"
                ),
                "match_method": "birth",
                "birth_gap_seconds": round(gap, 1),
                "transcript": record["path"],
            }
        )
        assigned_pids.add(pid)
        used.add(key)


def map_sessions(
    processes: list[dict], records: list[dict], warnings: list[str]
) -> list[dict]:
    by_key = {(r["provider"], r["session_id"]): r for r in records}
    for proc in processes:
        session_id = exact_session_id(proc["provider"], proc["argv"])
        proc["exact_id"] = session_id
        record = by_key.get((proc["provider"], session_id)) if session_id else None
        proc.update(
            {
                "session_id": session_id,
                "confidence": "exact" if session_id else "none",
                "match_method": "argv" if session_id else "unmapped",
                "birth_gap_seconds": None,
                "transcript": record["path"] if record else None,
            }
        )
    assign_birth_matches(processes, records, warnings)

    duplicates: dict[tuple[str, str], list[dict]] = {}
    for proc in processes:
        if proc.get("session_id"):
            duplicates.setdefault((proc["provider"], proc["session_id"]), []).append(proc)
    for (provider, session_id), rows in duplicates.items():
        if len(rows) > 1:
            warnings.append(f"duplicate live mapping for {provider} session {session_id}")
            for row in rows:
                row["confidence"] = "low"
    return processes


def _public_session(proc: dict, current_pid: int | None) -> dict:
    return {
        "provider": proc["provider"],
        "pid": proc["pid"],
        "ppid": proc["ppid"],
        "tty": proc["tty"],
        "cwd": proc.get("cwd"),
        "session_id": proc.get("session_id"),
        "confidence": proc.get("confidence", "none"),
        "match_method": proc.get("match_method", "unmapped"),
        "birth_gap_seconds": proc.get("birth_gap_seconds"),
        "transcript": proc.get("transcript"),
        "launch_mode": proc["launch_mode"],
        "is_current": proc["pid"] == current_pid,
    }


def build_output(
    processes: list[dict],
    current_pid: int | None,
    provider_filter: str,
    scope: str,
    warnings: list[str],
) -> dict:
    current = next((p for p in processes if p["pid"] == current_pid), None)
    selected = [p for p in processes if provider_filter == "all" or p["provider"] == provider_filter]
    if scope != "ALL":
        selected = [p for p in selected if p.get("cwd") == scope]
    sessions = [_public_session(proc, current_pid) for proc in selected]
    sessions.sort(key=lambda row: (row["provider"], row["cwd"] or "", row["pid"]))
    return {
        "schema_version": 1,
        "scope": scope,
        "provider_filter": provider_filter,
        "current_pid": current_pid,
        "current_provider": current["provider"] if current else None,
        "warnings": sorted(set(warnings)),
        "sessions": sessions,
    }


def collect(provider_filter: str, all_cwds: bool, explicit_cwd: str | None) -> dict:
    processes, parent_by_pid, warnings = inspect_processes()
    candidate_pids = {proc["pid"] for proc in processes}
    current_pid = find_current_pid(os.getpid(), candidate_pids, parent_by_pid)
    if current_pid is None:
        warnings.append("unable to identify a supported current agent process")

    for proc in processes:
        proc["exact_id"] = exact_session_id(proc["provider"], proc["argv"])

    adapters = make_adapters()
    requested = PROVIDERS if provider_filter == "all" else (provider_filter,)
    records: list[dict] = []
    for provider in requested:
        adapter = adapters[provider]
        if not adapter.root.is_dir():
            message = f"{provider} transcript root unavailable: {adapter.root}"
            if provider_filter == provider:
                raise InspectionError(message)
            warnings.append(message)
            continue
        provider_processes = [p for p in processes if p["provider"] == provider]
        if not provider_processes:
            continue
        try:
            records.extend(adapter.discover(adapter.root, provider_processes, warnings))
        except OSError as exc:
            message = f"unable to discover {provider} transcripts: {exc}"
            if provider_filter == provider:
                raise InspectionError(message) from exc
            warnings.append(message)

    map_sessions(processes, records, warnings)
    current = next((p for p in processes if p["pid"] == current_pid), None)
    if all_cwds:
        scope = "ALL"
    elif explicit_cwd is not None:
        scope = os.path.realpath(explicit_cwd)
    elif current and current.get("cwd"):
        scope = current["cwd"]
    else:
        scope = os.path.realpath(os.getcwd())
    return build_output(processes, current_pid, provider_filter, scope, warnings)


def print_human(data: dict) -> None:
    sessions = data["sessions"]
    other_count = sum(not row["is_current"] for row in sessions)
    print(f"Scope: {data['scope']}")
    print(f"Live sessions: {len(sessions)} (excluding this session: {other_count})")
    if data["warnings"]:
        print("Warnings:")
        for warning in data["warnings"]:
            print(f"  - {warning}")

    if not sessions:
        print("\nNo live sessions are in scope; there are no other sessions to suggest.")
    else:
        grouped: dict[tuple[str, str], list[dict]] = {}
        for row in sessions:
            grouped.setdefault((row["provider"], row["cwd"] or "(cwd unavailable)"), []).append(row)
        for (provider, cwd), rows in grouped.items():
            print(f"\n{provider} — {cwd}")
            for row in rows:
                session_id = row["session_id"] or "(unmapped)"
                confidence = (
                    f" — {row['confidence']} confidence"
                    if row["confidence"] in {"low", "none"}
                    else ""
                )
                marker = " — THIS SESSION — EXCLUDED" if row["is_current"] else ""
                print(f"  {session_id} [pid {row['pid']}]{confidence}{marker}")
        if other_count == 0:
            print("\nNo other live sessions are in scope.")

    print("\nThis command only reports. A later `sesh export <ID>` requires explicit confirmation.")


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Report live Claude Code, Codex, and Pi terminal sessions."
    )
    parser.add_argument("cwd", nargs="?", metavar="CWD")
    parser.add_argument("--provider", choices=("all",) + PROVIDERS, default="all")
    parser.add_argument("--all", action="store_true", dest="all_cwds")
    parser.add_argument("--json", action="store_true", dest="as_json")
    args = parser.parse_args(argv)
    if args.cwd is not None and args.all_cwds:
        parser.error("CWD cannot be combined with --all")
    return args


def check_platform() -> None:
    if sys.platform != "darwin":
        raise InspectionError(
            "Unsupported platform: find-open-sessions requires macOS.", exit_code=3
        )
    missing = [name for name in ("ps", "lsof") if shutil.which(name) is None]
    if missing:
        raise InspectionError(
            f"Missing required system command(s): {', '.join(missing)}", exit_code=3
        )


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        check_platform()
        data = collect(args.provider, args.all_cwds, args.cwd)
    except InspectionError as exc:
        print(str(exc), file=sys.stderr)
        return exc.exit_code
    if args.as_json:
        print(json.dumps(data, indent=2, sort_keys=False))
    else:
        print_human(data)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
