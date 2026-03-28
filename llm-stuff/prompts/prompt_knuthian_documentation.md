## Simple Prompt

Based on this coding session, update the comments (block, inline, and docstring)
for <foo> in the spirit of Knuthian literate programming following these
principles:

-   **Explanation of WHY not just WHAT:** Each logical chunk of code explains
    the reasoning behind design decisions
-   **Problem-Solution Structure:** Clearly articulates problems before
    presenting solutions
-   **Concrete Examples:** Shows actual HTML structures, markdown examples, and
    transformations
-   **Technical Rationale:** Explains why specific approaches were chosen over
    alternatives
-   **Safety and Robustness:** Documents how edge cases are handled
-   **Workflow Integration:** Explains how features fit into real workflows in
    the codebase

## Complex Prompt

Based on this coding session, update the comments (block, inline, and docstring)
for `<foo>` in the spirit of Donald Knuth's literate programming. Your goal is
to produce a self-contained, high-quality piece of technical literature that
explains the program not just to a compiler, but to a thoughtful human reader.

Follow these principles:

#### 1. **Structure as a Narrative (Order of Human Thought)**

-   **Top-Down Explanation:** Begin with a high-level summary in the main
    docstring, explaining the function's purpose and its place in the broader
    system. The explanation should flow like an essay.
-   **Logical, Not Chronological, Order:** Present the code and its explanation
    in the most logical order for understanding. Explain the "why" of the main
    function *before* diving into the "how" of every helper function or complex
    variable. Use block comments to create "sections" for each major logical
    step.

#### 2. **Explain the "Why," Not Just the "What"**

-   **Problem-Solution Framing:** For each significant code block, first
    articulate the specific, small problem it is meant to solve. Then, present
    the code as the solution.
-   **Technical Rationale:** Explain why this specific implementation was chosen
    over potential alternatives. For example, "We chose a dictionary for O(1)
    lookups instead of iterating through a list, which would be O(n)."

#### 3. **Use Concrete Examples and Illustrations**

-   **Show, Don't Just Tell:** In docstrings or major block comments, provide
    concrete examples of inputs and their expected outputs. If the code
    transforms data, show a small "before" and "after" snippet to help the
    reader have a mental model of what the code is doing.

#### 4. **Ensure Robustness and Clarity**

-   **Acknowledge Edge Cases:** Explicitly document how the code handles edge
    cases, invalid inputs, or potential failure modes. Explain the reasoning
    behind the error-handling strategy.
-   **Cross-Reference:** To create a cohesive "web of understanding," briefly
    reference other functions or modules that this code interacts with,
    explaining the contract or relationship between them. This helps place the
    function within the larger **Workflow Integration**.
