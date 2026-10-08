# Medium-specific applications

Contents: [presentations](#oral-presentations-and-slides) ·
[instructions](#instructions-and-procedures) ·
[email](#email-and-substantial-professional-messages) ·
[web](#web-pages-and-navigable-sites) ·
[meeting reports](#meeting-reports) · [posters](#scientific-posters) ·
[code-adjacent artifacts](#pull-requests-design-discussions-and-incident-issues) ·
[chat](#answers-in-chat-or-a-terminal)

Load only the sections relevant to the requested artifact. Apply the
core principles in `../SKILL.md`; the medium changes their
implementation, not their priority.

## Oral presentations and slides

Design the talk before designing slides. A useful presentation spine is:

1.  **Opening**: optional attention getter, need, task or speaker role,
    main message, then a preview of the body;
2.  **Body**: a small hierarchy of main points and subpoints, made
    visible by transitions;
3.  **Closing**: review the body's content and structure, restate the
    conclusion, then close cleanly.

Place the preview after listeners have enough motivation and a main
message to interpret it. Preview the body, not every opening and closing
move. Use the same short labels in the preview, transitions, and review.
A review should synthesize content, not merely repeat the point labels.

For slides:

-   Use slides only when a visual channel helps; no slides are better
    than noisy or redundant slides.
-   In first approximation, develop one subpoint per slide and state its
    message as a short sentence in the title area.
-   Illustrate visually where possible. Do not show material the speaker
    will not address.
-   Ensure that a listener who cannot see the slides can follow the
    speech and a viewer who cannot hear can recover the main messages
    from the slides. Avoid competing streams of prose.
-   Use builds sparingly to reveal a genuinely complex sequence.
    Remember that animation itself unfolds sequentially and can compete
    with speech.

Plan verbal, vocal, and visual delivery together. Rehearse the
presentation in realistic conditions, including transitions, timing,
pointing, and equipment. Prepare for questions by anticipating the
audience's needs. When answering, listen fully, restate or clarify when
useful, pause to structure a brief answer, and address the whole
audience. For a simple answer, state the point, give the reason and an
example if needed, then close on the point.

## Instructions and procedures

Users read instructions to act.

-   State the objective and prerequisites before the steps.
-   Organize actions as tasks and subtasks instead of one long numbered
    chain.
-   Number actions the user performs; distinguish system responses,
    explanations, warnings, and expected results visually and verbally.
-   Put every condition or warning before the action it governs.
-   Use direct action verbs and, in English, normally the imperative.
    Account for languages and cultures in which an infinitive or more
    formal construction is more appropriate.
-   Use parallel language for parallel actions.
-   Tell users how to recognize success and what to do when the observed
    result differs.
-   Use action headings such as `Connecting the sensor`, not object
    labels such as `Sensor connection`.

A procedure's global component should orient users to the product or
task, identify what they need, and help them choose where to start. It
need not be a seven-slot report of completed work.

## Email and substantial professional messages

Treat email as written communication, not disposable chat.

-   Put people who must act in `To`; put people who need only the
    outcome or information in `Cc`.
-   Use a two-part subject: a durable global topic or collection, then
    the message-specific item
    (`March Newsletter: draft article for review`). Update the second
    part when the message changes subject.
-   Supply concise context at the beginning. Quote only the minimum
    original text needed to orient the reply.
-   State who should do what by when. Make requests explicit and polite.
-   Acknowledge a message when silence would leave the sender uncertain;
    if a full answer will be late, state when to expect it.
-   Put immediately readable content in the message. Use an attachment
    for material recipients will edit, print, or file, and make the
    message and attachment independently understandable. The message may
    serve as a compact abstract of the attachment.
-   Avoid reverse-chronological transcript chains as a substitute for
    context.

Do not mechanically force every email into all seven global-component
functions. Select the motivation, outcome, and action this audience
needs.

## Web pages and navigable sites

Reveal the site's tree and help visitors answer three questions: where
am I, where can I go, and what will I find there?

-   Provide a global map or navigation that is visible as a whole at the
    relevant scale.
-   Mark the visitor's current position.
-   Use stable destination wording between navigation, page headings,
    and links.
-   Place important navigation links outside running sentences when this
    makes destinations easier to scan.
-   Make link wording informative enough for a visitor to decide whether
    to follow it.
-   Use local maps for deeper branches rather than displaying the entire
    site hierarchy everywhere.

## Meeting reports

Report the meeting's motivation and outcome, not a transcript of
everything said.

On the first page or opening view, provide what readers need to orient
and act:

-   meeting identity, date, and---when recurring---number or
    periodicity;
-   participants and their roles when the roles explain why they were
    involved;
-   the motivation or objective, consistent with the invitation;
-   decisions reached;
-   actions stated as **who does what by when**;
-   unresolved issues or next meeting when relevant.

Express each action with a verb and name one accountable owner when
several people contribute. Put decisions and actions before discussion
highlights. Include only enough account of viewpoints and evidence to
remind attendees, inform absentees, justify the outcome, or support
later work.

## Scientific posters

Treat a poster primarily as visual support for conversation, not as a
paper enlarged onto a wall.

-   Make the main messages understandable without oral explanation, but
    use as little prose as possible.
-   Reveal a clear reading sequence across the two-dimensional layout.
-   Use message-bearing headings and figures that can be grasped from
    viewing distance.
-   Keep methods and detailed evidence subordinate to the research need,
    findings, and conclusion.
-   Ensure that the author can point to and discuss elements without
    blocking or forcing viewers through dense text.

## Pull requests, design discussions, and incident issues

This is an adaptation of Doumont's principles to common agent-produced
artifacts rather than a distinct source chapter.

-   Use the title as a selection tool: state the change or observed
    problem, not a generic category
    (`Stop retrying non-idempotent writes`, not `Retry update`).
-   State the need, chosen change, and consequence before implementation
    detail.
-   Do not narrate the sequence of files inspected or restate a diff
    that readers can already see. Explain intent, tradeoffs, rejected
    alternatives when they matter, blast radius, and what the reviewer
    should verify.
-   In an incident issue, distinguish expected behavior, observed
    behavior, and reproduction conditions. Put conditions before
    reproduction actions.
-   Name decisions, owners, and follow-up dates explicitly.

## Answers in chat or a terminal

Answer the question in the first sentence, then provide the support
needed for confidence or action. Do not narrate the search unless the
process itself is requested or materially affects uncertainty. Use
headings and lists only when they reveal a real hierarchy or comparison;
a short answer carrying one message usually needs neither.
