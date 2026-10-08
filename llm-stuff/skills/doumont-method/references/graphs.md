# Pictures, graphs, and figures

Contents: [picture choice](#decide-whether-a-picture-helps) ·
[display selection](#select-a-quantitative-display-from-the-question) ·
[encoding](#balance-intuition-and-resolution) ·
[construction](#construct-the-display) ·
[interpretive text](#state-the-interpretation-with-the-figure)

Load this reference when creating or auditing a picture, quantitative
display, figure, or its accompanying text. Begin with the audience's
question and message; do not begin with a preferred chart type.

## Decide whether a picture helps

Verbal and nonverbal codings have complementary strengths. Words express
abstract concepts and negation precisely. Pictures show concrete
relations and spatial structure intuitively but remain ambiguous without
words.

-   Prefer a sober schematic or line drawing when realistic detail would
    add noise. Use a photograph when the actual appearance or atmosphere
    matters.
-   Clarify both **what** viewers are seeing and **why** they are seeing
    it.
-   Express negation visibly as well as verbally; a picture of the
    prohibited action alone can reinforce the wrong behavior.
-   Combine a visual with concise explanatory words. Diagram labels that
    can be scanned in any order complement prose; animated sequences and
    simultaneous spoken prose can compete because both unfold in time.

When representing a concept, choose deliberately along two dimensions:

-   **Pictorialness**: realistic, schematic, or abstract;
-   **Literalness**: literal, metaphorical, or conventional.

The more generic or conventional the concept, the more it needs an
explicit verbal message.

## Select a quantitative display from the question

Classify what the audience needs to see before selecting the encoding:

| Question            | Useful starting representations                                                                                         |
| ------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| Compare values      | Aligned lengths for an intuitive bounded comparison; positions for close values or intervals                            |
| Show a distribution | Dot/strip display, histogram when bins are meaningful, box plot for compact comparison, or cumulative distribution      |
| Reveal correlation  | Positions of paired values in a scatterplot; add encodings only when additional variables matter                        |
| Show evolution      | Positions along a continuous axis, usually markers and lines; use bars only for discrete amounts from a meaningful zero |

Account for the number and type of variables. Do not encode more
continuous dimensions than the data contain. Reserve tables for lookup
and exact storage; reserve figures for patterns or messages worth
seeing.

## Balance intuition and resolution

Do not use a single total ranking of encodings. Doumont treats
one-dimensional length and position as the most accurate family, with a
tradeoff:

-   **Length** is highly intuitive, especially when segments are aligned
    at one end, but its necessary zero bounds resolution.
-   **Position along a scale** can omit zero when doing so is honest and
    offers the highest resolution for close values, intervals, and
    uncertainty.
-   **Angle or slope** is less accurately judged than a direct linear
    encoding.
-   **Area** is harder to judge and should rarely replace length or
    position.
-   **Volume** is still less accurate and usually decorative.

Bars encode length. They must extend from a **meaningful zero** on a
linear scale. If zero is arbitrary, as for many temperature scales, or
the scale is logarithmic, use a position representation instead. If an
outlier makes a bar impractically long, prefer two views at different
scales; if interrupting the bar, make both the break and scale
interruption conspicuous and state the value.

Use dot charts rather than bars when close values, confidence intervals,
or other whiskers must be resolved accurately.

## Construct the display

-   Let the data dominate. Question every axis, grid line, border,
    color, and dimension.
-   Use roughly two to five intervals per displayed range as a starting
    point; prefer meaningful thresholds, peaks, or endpoints to
    arbitrary ticks.
-   Label series and points directly where practical. Avoid a detached
    symbol or color key that forces repeated eye travel.
-   Use color redundantly, not as the only carrier of meaning, and
    verify the display in grayscale.
-   Order categories by value or another meaningful logic rather than by
    an arbitrary default.
-   Show uncertainty in a representation that supports it. Position
    displays normally accommodate intervals better than one-sided
    whiskers on bars.
-   Make every graphical magnitude proportional to what it claims to
    represent.

## State the interpretation with the figure

Doumont uses a restrictive terminology:

-   **caption**: a descriptive title or noun phrase stating the what;
-   **legend**: one or more complete sentences explaining the so what;
-   **key**: a mapping from symbols or colors to data series, also
    commonly called a legend in modern usage.

Because contemporary terminology varies, preserve the functions even
when the artifact calls them something else. Prefer one short
interpretive sentence that makes the figure and its text understandable
without the surrounding body. Add a descriptive title only when the
interpretation does not already make the contents clear. Replace
detached keys with direct labels when possible.

Audit the display by asking:

1.  What audience question does it answer?
2.  What complete sentence should viewers retain?
3.  Does the chosen geometry represent the data truthfully and at the
    needed resolution?
4.  Can the figure and its interpretive text stand alone?
5.  Which ink or pixels can be removed without losing meaning?
