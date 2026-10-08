# Audit rubric

Contents: [contract](#pass-0--establish-the-audit-contract) ·
[global story](#pass-1--test-the-global-story-and-sequence) ·
[tree and map](#pass-2--inspect-the-tree-and-map) ·
[prose](#pass-3--inspect-paragraphs-sentences-and-words) ·
[medium](#pass-4--inspect-lists-visuals-and-medium-specific-choices) ·
[reporting](#report-the-audit-constructively)

Use this reference in audit mode. Work from purpose and structure toward
prose and surface. Do not count violations mechanically: determine
whether each choice helps this audience reach the intended outcome.

Interpret labels as follows:

-   **Principle**: a direct consequence of audience, purpose, or
    truthful representation; deviations require a compelling reason.
-   **Default**: a reliable starting point and diagnostic threshold;
    retain an exception when it works better in context.
-   **Context**: apply only when the artifact or medium makes it
    relevant.

## Pass 0 --- Establish the audit contract

-   **Principle** --- What should the audience be able to do afterward?
-   **Principle** --- Which audience matters to that purpose? How
    specialized and how close to the current context are its members?
-   **Principle** --- What medium, length, deadline, language, or
    institutional constraints apply?
-   **Context** --- What is the artifact's state: outline, early draft,
    polished draft, or published material?
-   **Principle** --- Did the user request diagnosis, revision, or both?
    What review depth is useful now: structure, accuracy, clarity,
    correctness, or a combination?

If these answers cannot be inferred and would materially change the
audit, state the uncertainty. Do not silently assume that every artifact
is a report of completed work or that every audience is an approval
committee.

## Pass 1 --- Test the global story and sequence

For a professional document reporting work, locate the opening component
and test the functions that matter:

| Function     | Test                                                     | Status            |
| ------------ | -------------------------------------------------------- | ----------------- |
| Context      | Does a secondary reader know enough about the situation? | Conditional       |
| Need         | Does the audience see the gap, opportunity, or question? | Usually central   |
| Task         | Is the relevant agent and completed work clear?          | For work reported |
| Object       | Is the document's purpose or coverage clear?             | May be implicit   |
| Findings     | Are the main results stated at the audience's level?     | For work reported |
| Conclusion   | Are the findings interpreted for the audience?           | Usually central   |
| Perspectives | Is useful next action or remaining work clear?           | Conditional       |

Diagnose recurring incomplete forms:

-   **Promissory**: motivation and object without findings or
    conclusion;
-   **Out of the blue**: findings and conclusion without enough
    motivation;
-   **Self-centered**: task and findings without the audience's need or
    the interpretation.

Then test the sequence at each useful scale:

-   **Principle** --- Does motivation precede a message that gives the
    supporting details meaning?
-   **Default** --- Does the conclusion appear before the history of how
    the author reached it?
-   **Context** --- When chronology itself is the subject---a timeline,
    procedure, or narrative---is its ordering still optimized for the
    audience?
-   **Default** --- Does each meaningful section begin with enough of a
    global view to motivate or preview its subdivisions?

## Pass 2 --- Inspect the tree and map

Generate a heading outline and view it whole.

-   **Default** --- More than three visible heading levels or roughly
    five siblings under a parent triggers a grouping review; it is not
    an automatic defect.
-   **Default** --- A parent with one child may indicate an unnecessary
    level.
-   **Principle** --- Are siblings comparable, and is their grouping
    logic recognizable to the audience?
-   **Default** --- Do adjacent parent and first-child headings reveal a
    missing global paragraph?
-   **Default** --- Is the structure too flat to grasp or deeper than it
    needs to be? Could low-level headings become paragraphs, sentences,
    or local lists?
-   **Context** --- For a presentation, is the hierarchy simple enough
    to retain without rereading?

Inspect navigation:

-   **Context** --- Does a long or selectively read artifact provide a
    map visible in one view?
-   **Default** --- Does the initial map show only the top one or two
    levels, with local maps or an index providing more detail where
    needed?
-   **Principle** --- Can the audience tell where they are?
-   **Principle** --- Do headings, links, transitions, and maps use
    stable wording?
-   **Context** --- Do destination labels support an informed decision
    to follow a link or cross-reference?

Inspect label roles instead of requiring one style everywhere:

-   document headings: meaningful, concise, parallel map labels; claims
    where useful;
-   slide titles: short message sentences;
-   instructional headings: actions or tasks;
-   figure text: intended interpretation, understandable with the figure
    alone.

## Pass 3 --- Inspect paragraphs, sentences, and words

Read only paragraph openings. Does the document's content and
progression emerge?

-   **Default** --- Does each paragraph announce its theme or message
    early?
-   **Default** --- Is there roughly one developed message per
    paragraph, allowing introductions to orient and compact abstracts to
    carry several messages?
-   **Default** --- Do parallel and serial links make the progression
    apparent? Flag unmotivated subject switches, not every mixture of
    the two patterns.
-   **Default** --- Does a run of one-sentence paragraphs state messages
    without developing them?
-   **Principle** --- Does the opening accurately announce what the
    paragraph goes on to do?

At sentence level, check:

-   **Principle** --- Main information belongs in the main clause.
    Inspect empty frames such as `Figure N shows that`,
    `It was observed that`, and `It is clear that`.
-   **Default** --- One idea per sentence, with main and subordinate
    clauses used to prioritize a genuinely complex idea.
-   **Default** --- Keep subject and verb close; move long lists or
    parentheticals after the main structure; place short items before
    long ones.
-   **Principle** --- Make logical connections explicit enough that
    readers do not have to reconstruct them.
-   **Principle** --- Name the agent when responsibility, authority,
    belief, decision, or recommendation matters.
-   **Default** --- Prefer verbs to nominalizations and active voice
    when it improves agency or directness. Retain passive voice when the
    agent is irrelevant or the topic should remain the subject.
-   **Default** --- Use first person for the authors' tasks, decisions,
    or beliefs; question repetitive `we`, collective `we see`, and
    `In this section, we ...` when the topic can act directly.

At word level, check clarity, then accuracy, then conciseness:

-   explain necessary terms and acronyms at first use;
-   use one stable name per concept;
-   distinguish technical terms from local jargon;
-   remove empty phrases and ineffective redundancy only after meaning
    is clear;
-   account for non-native readers when relevant.

## Pass 4 --- Inspect lists, visuals, and medium-specific choices

For lists:

-   **Default** --- Five or fewer comparable items when the set must be
    grasped globally; group longer sets when useful.
-   **Principle** --- A stem clause introduces the relationship among
    the items.
-   **Principle** --- Items continue the stem grammatically and use
    parallel syntax.
-   **Context** --- A longer list is legitimate for exhaustive
    reference, lookup, established sequence, or data storage.
-   **Default** --- Punctuation and capitalization remain consistent
    with whether the items are sentences or parts of one sentence.

For formatting:

-   **Principle** --- Proximity, similarity, prominence, and visual
    sequence reveal the same structure as the words.
-   **Default** --- Spacing and alignment carry structure before type or
    color.
-   **Default** --- Headings sit nearer their own material than the
    preceding text.
-   **Default** --- Emphasis is rare enough to remain prominent.

For figures and graphs, apply `graphs.md`. For presentations,
instructions, email, web pages, meeting reports, posters, pull requests,
or similar artifacts, apply the relevant part of `applications.md`.

## Report the audit constructively

Return findings in this order unless the user requests another format:

1.  **Assessment** --- State whether the artifact can achieve its
    purpose and name the largest obstacle.
2.  **Structural changes** --- Give concrete changes to purpose/audience
    fit, global story, tree, map, and sequence.
3.  **Local changes** --- Show representative paragraph or sentence
    repairs, not an exhaustive copyedit unless requested.
4.  **What to preserve** --- Identify real strengths that should survive
    revision.

Name the defect before offering replacement text. Distinguish an error
from a preference and a required change from a suggestion. Prioritize:
an audit that buries its message in every detectable violation fails the
signal-to-noise test.
