# Worked examples

Contents: [document structure](#replace-an-investigation-chronology-with-a-decision-structure) ·
[incident summary](#complete-an-incident-summary) ·
[credit-risk example](#preserve-a-demanding-credit-risk-example-without-making-it-the-default) ·
[headings](#choose-headings-by-function) ·
[paragraphs](#state-and-develop-paragraph-messages) ·
[clauses](#put-main-information-in-the-main-clause) ·
[voice](#distinguish-active-voice-from-first-person) ·
[complexity](#move-complexity-out-of-the-subjector-out-of-prose) ·
[lists](#build-lists-from-a-relationship-not-a-pile) ·
[visual text](#put-the-message-next-to-the-visual) ·
[instructions](#put-conditions-before-actions)

Use these patterns selectively. They span several domains so the method does
not imply one default audience, decision, or document genre. Examples marked
*(Doumont)* are adapted from the source book.

## Replace an investigation chronology with a decision structure

**Before** — an architecture proposal organized around the author's work:

```text
1. Introduction
2. Current architecture
3. Load tests
4. Options considered
5. Benchmark results
6. Discussion
7. Conclusion
```

The reader must traverse the investigation before learning what should change
or what tradeoff requires a decision.

**After** — motivation, claims, and recommendation first:

```text
Summary: the current queue cannot carry next year's volume; migrate before Q4
1. Why the current queue will reach its limit next year
2. What a partitioned log changes
   2.1 Sustained throughput rises from 8k to 60k events/s
   2.2 Replay becomes possible, at the cost of a three-week migration
   2.3 Two consumers must be rewritten
3. Why we recommend migrating before the Q4 freeze
Appendix A. Options considered
Appendix B. Benchmark method and full results
```

The body now follows the reader's decision. Investigation details remain
available to reviewers without standing between every reader and the message.
For a replication-oriented scientific paper, more method may belong in the
body; theorem-proof ordering does not mean hiding evidence needed for the
purpose.

## Complete an incident summary

**Before** — task and findings only:

> On 4 March we investigated a 47-minute outage of the payments API. We found
> that a deployment reduced the connection pool and that the alert fired 11
> minutes after user impact began.

The reader does not know whether the problem remains possible or what action
the report supports.

**After** — the relevant functions of a global component:

> *(Context and need)* Payments traffic now peaks at three times the level for
> which the connection pool was sized. On 4 March, a routine configuration
> deployment took the API down for 47 minutes, and no alert fired for the first
> 11 minutes. *(Task)* The on-call team reconstructed the timeline from
> deployment logs and traces and reviewed the alerting rules. *(Object)* This
> postmortem identifies the failure and asks Platform to fund two preventive
> changes before the April peak.
>
> *(Findings)* A one-line change reduced the pool from 200 connections to 20;
> review missed it because pool size is absent from the diff summary. Existing
> alerts watched CPU, which remained flat. *(Conclusion)* The outage was a
> review-surface and detection failure, not a capacity failure, and the same
> change can pass today. *(Perspectives)* Gate pool-size changes behind a load
> test and alert on connection-wait time before the April peak.

## Preserve a demanding credit-risk example without making it the default

The following example remains useful because its conclusion interprets several
findings under a real decision constraint.

**Before** — task and findings only:

> We recalibrated the SME probability-of-default model on 2019–2025 data and
> evaluated it with standard backtests. Aggregate calibration error is 4 basis
> points and Gini is 0.62.

**After** — a compact decision-oriented summary:

> Portfolio growth and a shift toward firms with shorter credit histories have
> caused the current model to overstate aggregate risk and distort pricing.
> At the Credit Risk Committee's request, we recalibrated and revalidated it on
> 2019–2025 data. Aggregate calibration error falls from 31 to 4 basis points,
> with discrimination unchanged; firms in the lowest-turnover decile remain
> underpredicted by about 15%. The recalibrated model is fit for the pricing and
> capital uses in scope provided that segment retains its overlay. Address the
> segment directly in the next redevelopment.

The conclusion does not merely repeat the two metrics; it states the permitted
use and condition.

## Choose headings by function

Document headings are map labels. Claim headings help when the claim is the
most useful destination label, but topic or action labels may be better in
other genres.

| Weak or generic     | Stronger for its function                                  |
| ------------------- | ---------------------------------------------------------- |
| Results             | Throughput triples without increasing tail latency         |
| Limitations         | Two workloads remain outside the design's scope            |
| Discussion          | Why traffic mix, not the query planner, drove the slowdown |
| User identification | Identifying yourself as a user                             |
| Project             | March Newsletter: draft article for review                 |

The first three become claims because readers navigate by findings. The
instruction becomes an action. The message subject uses a durable collection
label followed by the specific item.

## State and develop paragraph messages

**Before** — the interpretation arrives after a description *(Doumont)*:

> Figure 2 shows the evolution of the Ge content in the SiGe layer. Obviously
> there is a nearly linear decrease of the Ge content with increasing fluence.

**After**:

> The germanium content decreases linearly with increasing fluence (Figure 2).

**Before** — the opening misannounces a comparison *(Doumont)*:

> Single-use, disposable medical devices are packaged and sterilized by the
> manufacturer. Their packaging must provide protection, facilitate
> sterilization, maintain sterility, ... Reusable devices, by contrast, must be
> ...

**After**:

> Medical devices fall into two categories, disposable and reusable, with
> different sterilization requirements. Single-use devices are packaged and
> sterilized by the manufacturer. ... Reusable devices, by contrast, must be
> ...

An introductory paragraph may instead announce a theme or structure without a
true so-what. Test whether the opening performs its function, not whether it
matches one grammatical template.

### Make sentence links visible

**Parallel** — successive sentences retain the topic *(Doumont)*:

> Codes based on the Diabolo algorithm have become increasingly popular.
> Compared with traditional Demon codes, they are about twice as fast, are
> reasonably easy to implement, and can handle hybrid transforms. As a
> drawback, they require about 45% more memory.

**Serial** — a new item becomes the next subject *(Doumont)*:

> Current implementations of the Diabolo algorithm use the Angel transform.
> This transform separates the data into high and low values before generation.
> These values are then stored separately.

Mix the patterns when the reasoning requires it. Repair a subject change that
does not reflect a topic change or a new item introduced before the item that
links back.

## Put main information in the main clause

| Before                                                                      | After                                                    |
| --------------------------------------------------------------------------- | -------------------------------------------------------- |
| Figure 3 shows that the simulation worked well.                             | The simulation worked well (Figure 3).                   |
| It was observed that the rate fell at higher temperature.                   | The rate fell at higher temperature.                     |
| It is clear that the first option costs less.                               | The first option clearly costs less.                     |
| A finite-element simulation of the critical subparts was performed.         | We simulated the critical subparts with finite elements. |
| The results are summarized in Table 5. They show that option A is superior. | The results show that option A is superior (Table 5).    |
| With this method, the volume is underestimated.                             | This method underestimates the volume.                   |

To repair main information trapped in a frame, delete the frame and identify
what was lost. Restore only useful information as a subordinate clause or
parenthesis.

## Distinguish active voice from first person

| Before                                        | After                                     | Reason                                                         |
| --------------------------------------------- | ----------------------------------------- | -------------------------------------------------------------- |
| In this section, we compare the alternatives. | This section compares the alternatives.   | The section, not the authors, is the topic.                    |
| We see in Equation 4 that pressure increases. | Pressure increases (Equation 4).          | Remove the unnecessary collective `we`.                        |
| It was decided to postpone deployment.        | The release manager postponed deployment. | The decision's agent matters.                                  |
| We sampled every fifth record.                | We sampled every fifth record.            | First person accurately reports the authors' task.             |
| The samples were stored at −20 °C.            | The samples were stored at −20 °C.        | The agent is irrelevant; passive voice keeps the topic stable. |

## Move complexity out of the subject—or out of prose

**Before** — readers must retain a long subject before reaching the action:

> Reconfiguring the gateway, migrating the two legacy consumers, and replaying
> the retained event stream must be completed before cutover.

**After** — forward-reference the list:

> Three tasks must be completed before cutover: reconfigure the gateway,
> migrate the two legacy consumers, and replay the retained event stream.

When repeated prose encodes exact mappings, use a table instead:

| State   | Indicator | Operator action                     |
| ------- | --------- | ----------------------------------- |
| Ready   | Green     | Begin processing.                   |
| Waiting | Amber     | Check the upstream queue.           |
| Failed  | Red       | Stop and page the on-call engineer. |

## Build lists from a relationship, not a pile

**Before** *(Doumont)* — seven mixed items with no governing stem:

```text
- To prepare a meeting, define its purpose
- You must also prepare an agenda
- Everyone should receive this agenda
- Does everyone know who the others are?
- The chairperson should not be secretary
- Ground rules may be appropriate, too
- Always review the purpose and agenda
```

**After** — two stages with parallel actions:

```text
When preparing a meeting,
- define the purpose and agenda,
- send the agenda to all participants.

As you start the meeting,
- introduce participants,
- clarify the roles,
- establish ground rules if useful,
- review the purpose and agenda.
```

The repair is logical grouping plus parallel syntax—not merely reducing the
number of bullets.

## Put the message next to the visual

| Descriptive what                                  | Interpretive so what                                                                                                               |
| ------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Evolution of Colayol sales, 1996–2002 *(Doumont)* | Colayol sales dropped by 50% over the last two years following the 2002 recession.                                                 |
| Predicted and realized default rates by decile    | Predictions track realized rates within 5 basis points except in the lowest-turnover decile, where the model underpredicts by 15%. |

See `graphs.md` for the source's caption/legend terminology and for graph
selection.

## Put conditions before actions

| Before *(Doumont)*                                                                   | After                                                                               |
| ------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------- |
| Click OK, unless you want to modify the default values, in which case click Options. | To modify the default values, click Options. Otherwise, click OK.                   |
| Victims of a chemical spill need to be placed under the shower at once.              | If a coworker is affected by a chemical spill, place them under the shower at once. |
| The application will not work without a hardware key plugged into a USB port.        | To use the application, first plug the hardware key into a free USB port.           |
