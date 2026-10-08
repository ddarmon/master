---
name: doumont-method
description: >-
  Draft, structure, revise, or audit professional communication using Jean-Luc
  Doumont's audience-centered method from Trees, Maps, and Theorems. Use for
  reports, memos, whitepapers, design documents, RFCs, READMEs, model
  documentation, postmortems, pull-request descriptions, presentations,
  figures, instructions, meeting reports, and substantial professional
  messages; especially when a deliverable lacks a clear claim, buries its
  conclusion, follows the author's chronology, overuses bullets, or is hard to
  navigate. Also use when the user asks for Doumont style, theorem-proof or
  message-first structure, a so-what heading or caption, a global component,
  or a context-need-task-object/findings-conclusion-perspectives abstract.
---

# Doumont method

Produce communication that lets its audience find, grasp, and act on the
message with minimal effort. Optimize what the audience gets out of the
artifact, given its purpose and the available attention, time, and
space.

Use one of two modes:

-   **Generate**: establish the communication contract, identify the
    message, design the structure, then draft and revise.
-   **Audit**: establish the same contract, diagnose the artifact
    top-down with `references/checklists.md`, and either report
    prioritized issues or revise it when the user has requested a
    revision.

The source is Jean-Luc Doumont, *Trees, Maps, and Theorems: Effective
Communication for Rational Minds* (Principiae, 2009). If a copy is
available, consult it when extending or verifying this adaptation. Preserve the
source's qualifications: numerical limits and recurring patterns are
strong defaults, not laws to enforce without regard to purpose and
audience.

This file provides the core workflow. Load only the references needed:

-   `references/checklists.md` for an audit;
-   `references/worked-examples.md` for generative before/after
    patterns;
-   `references/graphs.md` for pictures, graphs, figures, or captions;
-   `references/applications.md` for presentations, instructions, email,
    web pages, meeting reports, posters, or code-adjacent communication.

## Scale the method to the task

Automatic activation does not require a formal review. Use the full workflow
for substantial communication artifacts; for small documentation edits or
ordinary replies, apply only the relevant principles without a formal audit,
extra questions, or added ceremony. Do not load this skill solely because a
routine coding or factual task will end with a written response.

- Preserve the requested scope and voice. A local edit is not permission to
  restructure the whole document, and an informal reply need not become a
  stand-alone report. Infer the communication contract when it is clear;
  ask only when an unknown would materially change the result.
- Keep the method subordinate to the task. In coding work, use it for the
  communication artifact, not as a replacement for technical investigation
  or validation. Do not announce the framework unless that helps the user
  or they asked about it.
- Preserve pedagogical discovery. In exercises and exploratory teaching,
  give clear goals and instructions without revealing answers the learner
  is meant to predict or derive. Message-first structure is not a mandate
  to give away the solution.
- Match the actual medium. A shared document needs readable native headings,
  spacing, and links, not merely well-organized plaintext. A slide needs a
  visual argument, not a report pasted onto a canvas. Check the delivered
  form when possible; distinguish content review from rendered review.
- Treat structural advice as a means, not a checklist quota. Recommend a
  new opening, claim heading, or regrouping only when it solves a concrete
  reader problem. Preserve useful topic headings and context-dependent
  brevity rather than making every artifact more formal.

## Establish the communication contract

Answer five questions before choosing the form. In audit mode, infer
answers from the artifact and request; surface uncertainty when it could
change the review.

-   **Why**: what should the audience be able to do afterward? Prefer an
    observable or otherwise testable outcome. Understanding may be a
    necessary intermediate outcome; ask what it enables.
-   **Who**: which readers or listeners matter to the purpose? Consider
    two independent continua: specialist to nonspecialist, and primary
    (close to the present situation) to secondary (distant in time or
    context).
-   **What**: which content is necessary and sufficient for that
    audience and purpose? Select it after identifying the conclusion.
-   **When and where**: what constraints do deadline, length, medium,
    language, and setting impose?
-   **Author's role**: why is this person or group communicating this
    message?

For an audit, also establish the document's state, the requested depth
of review, and whether the user wants diagnosis, revision, or both. Do
not rewrite an author's text when the request is only to review it.

## Apply the three laws

Use these priorities in decreasing order. An apparent lower-priority
rule must yield when the audience or purpose justifies a different
choice.

1.  **Adapt to the audience.** The communicator has the purpose and
    therefore the responsibility to adapt. Structure the material along
    the audience's reasoning, supply the context secondary readers need,
    and layer technical detail so different readers can stop at
    different depths.
2.  **Maximize the signal-to-noise ratio.** Nothing is neutral: anything
    that draws attention to the form instead of helping the message is
    noise. Prefer removing noise to adding signal.
3.  **Use effective redundancy.** Give the message more than one chance
    to get across through complementary codings or times: heading and
    prose, speech and diagram, preview and review. Two simultaneous
    streams of prose compete; concise labels integrated into a diagram
    do not.

Distinguish information from a message. Information states the **what**;
a message interprets it for this audience and purpose and states the
**so what** as a complete sentence.

| Information                                               | Message                                                                                              |
| --------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| A concentration of 175 µg/m³ was observed in urban areas. | The urban concentration (175 µg/m³) is unacceptably high.                                            |
| Evolution of sales over the years                         | Sales dropped by 40% last year.                                                                      |
| The service returned 503 errors for 47 minutes.           | A configuration change caused a preventable 47-minute outage, and the same failure remains possible. |

Identify messages early. Then select the shortest sufficient support. Do
not write the history of the work and move its conclusion forward
afterward when you can design from the conclusion in the first place.

## Design a navigable argument

Organize knowledge as a tree even though the audience encounters it as a
sequence. Group items by a logic the audience can recognize, make that
logic explicit when necessary, and keep siblings comparable in scope.

Use the following as diagnostic defaults for material that must be
grasped as a whole:

-   Prefer fewer hierarchical levels than items per level.
-   Endeavor to keep written documents to three visible levels and about
    five siblings under a parent. Use a simpler hierarchy for
    presentations.
-   Treat an excess as a prompt to group, demote low-level headings to
    paragraphs or sentences, introduce parts, or provide local
    maps---not as an automatic defect. Retain a justified exception.
-   Before subdividing a meaningful branch, give a global view that
    motivates it, previews its structure, or states its main message.
    Adjacent parent and first-child headings usually signal a missing
    global paragraph.

State the theorem before the proof at every useful scale: **motivation →
message → supporting detail**. Professional audiences rarely benefit
from a detective story. Ask: if the audience retains one sentence from
this document, section, paragraph, or slide, what must it be?

Give a visible map when navigation matters. A document's initial table
of contents normally shows no more than two levels even if local maps
reveal a third. Use consistent wording in maps, headings, transitions,
and links. Tell the audience where they are and, where useful, where
they can go.

### Use the global-component model flexibly

A professional document reporting work often benefits from a stand-alone
opening that gives the whole story at low resolution. Diagnose it with
seven possible functions, arranged as motivation and outcome:

| Part   | Function     | Question                                   |
| ------ | ------------ | ------------------------------------------ |
| Before | Context      | Why now?                                   |
| Before | Need         | Why does this matter to the audience?      |
| Before | Task         | What did the author do?                    |
| Before | Object       | What does this document do?                |
| After  | Findings     | What resulted from the task?               |
| After  | Conclusion   | What do the findings mean to the audience? |
| After  | Perspectives | What next?                                 |

Need and conclusion form the central problem-solution pair. Task and
findings report the work. Context and perspectives form an optional
outer situation layer. The object may be implicit when the need and task
already make the document's purpose unmistakable. Combine functions
under a hard word limit; omit one when its communicative work is already
done or irrelevant.

Phrase a task with the relevant agent, normally in active voice and past
tense (`We tested ...`). Phrase the object with the document as subject
in the present tense (`This note compares ...`). Interpret findings in
the conclusion instead of restating them.

Repeat **global component → detail** fractally for chapters and
sections. A branch's global paragraph needs at least enough of an object
to prepare readers for the subdivision; it need not reproduce all seven
functions.

### Give each label the right job

-   A document header supports routing, filing, and selection. Make
    title and author prominent; include a date and stable identifier
    when later retrieval or loose pages make them useful. A two-part
    global-to-specific title often works well.
-   A document heading labels a meaningful branch in the map. Make it
    concise, parallel with siblings, and recognizable in
    cross-references. Use a claim when that improves orientation, but do
    not force every heading into a sentence.
-   A slide title states the slide's message as a short sentence.
-   An instructional heading names the user's action or task.
-   A figure's accompanying text states the intended interpretation. See
    `references/graphs.md` for terminology and construction.

## Draft and format the support

Make each paragraph announce its function early and develop it
coherently. Often this function is one message, stated in the first
sentence, but preserve the source's exceptions: an introductory
paragraph may orient without a true so-what, and a short abstract may
carry several messages. One paragraph per message is a useful default,
not a parsing rule.

Connect sentences by content:

-   In a **parallel link**, successive sentences share a subject or
    topic. A personal pronoun often signals the continuation.
-   In a **serial link**, what one sentence introduces becomes the next
    sentence's subject. A demonstrative such as `this` or `such` often
    signals the handoff.
-   Combine the patterns when the logic requires it. Repair an
    unmotivated subject switch or an item introduced before the item
    that links back.

Write one idea per sentence, allowing complex ideas to use a main clause
plus subordinate support. Put the main information in the main clause.
Keep subject and verb close, place short items before long ones, and
make connections explicit. Repair a long subject with a short predicate
by moving detail after a colon or to the end. When prose remains
intrinsically awkward, use a table, formula, or diagram.

Optimize words in this order: clarity, accuracy, conciseness.

-   **Clarity**: use technical terms for audiences who know them, not
    jargon. Introduce necessary terms and acronyms; use one stable name
    per concept. For non-native readers, avoid unnecessary idioms,
    cultural references, false friends, and ambiguous short words.
-   **Accuracy**: name the agent when it matters---especially for
    decisions, recommendations, beliefs, and assumptions. Prefer verbs
    to nominalizations. Active voice is not the same as first person;
    passive voice is useful when the agent does not matter or when it
    keeps the paragraph's topic in subject position.
-   **Conciseness**: optimize only after clarity and accuracy. Remove
    empty frames, ineffective redundancy, and wordy phrases; combine
    closely related sentences.

Use lists to display comparable items, not to disguise a loose chain.
Aim for five or fewer items that continue a stem clause grammatically
and use parallel syntax. Group a longer set when readers must grasp it
globally; retain a longer list when completeness, lookup, or established
order matters more. Punctuate lists consistently according to whether
their items are sentences or parts of one sentence.

Format to reveal structure, not decorate it. Rely first on proximity,
similarity, prominence, and visual sequence. Use spacing and alignment
before type and color. Make emphasis scarce enough to remain prominent.

## Revise, test, and report

Treat revision as iteration through drafting, formatting, design,
and---when necessary---planning. Fix structural problems before local
phrasing because a restructure often removes the local defects.

Use complementary tests:

-   Ask a nonspecialized, secondary reader to test clarity and context.
-   Ask a specialist to test technical accuracy.
-   Ask a proficient reader to test language correctness.
-   Set the draft aside, then read only paragraph openings to test
    whether its content and progression emerge.
-   Read only the global component to test whether it supports an
    informed decision about what to do or read next.

Treat a reader's `I found this unclear` as evidence about that reading.
Identify the precise problem before proposing replacement prose. In an
audit, report a prioritized assessment rather than every detectable
violation, distinguish errors from suggestions, and note important
strengths so the author knows what to preserve.

Watch especially for generated-output failures:

-   **Structure**: flat bullet chains, arbitrary groupings, deep heading
    trees, or subdivisions with no global view;
-   **Sequence**: restating the prompt, narrating the search
    chronologically, burying the conclusion, or promising content
    without delivering the result;
-   **Surface**: topic drift, empty framing clauses, hedged
    non-conclusions, excessive first person, and uniform emphasis.

Apply `references/checklists.md` for a complete audit and
`references/worked-examples.md` when a before/after pattern would help.
