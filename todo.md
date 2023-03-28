# Features

- Automation of some proof actions
  - Click on it opens either:
    - A dialog with a list of checkboxes to enable/disable automation for each
      action
    - A dropdown checkbox list
  - Actions to automate (in order of priority, starting from the most desirable/
    harmless): `Unlock`, `Decompose`, `Close`, `Justify`, `Case` (then we need
    to split `Case` apart from `Unlock`)
  - Automation is run systematically after each action in Proof mode
  - One can run it manually with an additional button inside the
    dialog/dropdown (then in this scenario a dropdown is better to keep the
    current goal visible, and thus visualize the effect of automation)
  - Click actions for automated rules are not display anymore in the goal view:
    this is useful only so that the user gets immediate feedback when
    (un)checking (and to keep the state consistent for the current goal, since
    something which is automated should not be applicable manually)
  - Display rules in the dropdown with the same color as click actions?

- Add support for formulas
  - Better visual feedback emphasizing main connective and direct subformulas
    (like click-and-collect)

- Implement Fencing proof interaction with multiselection
  - Selection mode when maintaining shift key pressed
  - Mobile support:
    - Start with long press on any flower
    - Cancel by clicking on any garden, or by clicking on close button (shown
      only in selection mode)
  - When selection non-empty: apply by pressing button (otherwise disabled when
    selection is empty, and hidden outside of selection mode), or shortkey (like
    `!`)
  - Selection restricted to flowers in the same garden

- Implement Edit mode
  - The drop zones of Importing turn into `add flower` zones in positive gardens
  - positive petals can be pulled (that is, removed) by clicking on them
    (scissors cursor icon)
  - Two clipboards, one for cropped flowers and one for pulled petals
  - `add flower/petal` zones have:
    - text edit to grow atom
      - one can imagine instead a button that launches/inlines a domain-specific
        GUI to build statements/objects in a custom domain, i.e. euclidian
        geometry
    - `grow` buttons to grow a flower/petal
    - free space to:
      - paste clipboard by clicking
  - Flowers/petals created by `grow` buttons don't have polarity restrictions:
    everything can be removed, and things can be added anywhere
  - First-order:
    - `add variable` zones in all gardens, regardless of polarity
    - In a typed setting, one can imagine that it opens a custom GUI to choose a
      type (locally, or even in an online data base)
  - Add Reordering interaction for petals
    - Then to reorder a flower, one must drag from the pistil
  - Grown flowers/petals should be highlighted in blue

- Implement Navigation mode
  - Underlying data structure: *focus stack* = list of zippers
  - Global context at top of the screen = hypotheses in top of focus stack
  - Focus on flower by clicking
  - Jump to flower in global context by clicking
  - Unfocus by clicking on focused flower
  - Scale/Unscale animation?

# Brainstorming

- Name flowers

- View for partial proof term attached to flower
  - Highlight all flowers depending on a selected flower
  - Highlight all flowers used in the partial proof of a selected flower
  - Action in Edit mode for deleting unused positive flowers (with no other
    flowers depending on it)

- Curry-Howard correspondance: proof term = simply typed λ-term
  - Structural editor for functional languages with ML type system (like Elm!)
  - Should work well because of *variarity*
  - Only lacks recursive types
  - Bottom type?

- Full tree structure for undo/redo history, navigable from/identified with the
  partial proof term view

- Natural language view of proof term

- Analogy between Proof/Edit modes and Survival/Creative modes in Minecraft

- Vocal recognition to input text (typically for atoms)