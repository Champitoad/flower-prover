- Add support for formulas, and their introduction rules by click

- Implement Fencing proof interaction with multiselection
  - Selection mode when maintaining shift key pressed
  - Mobile support:
    - Start with long press on any flower
    - Cancel by clicking on any garden, or by clicking on close button (shown
      only in selection mode)
  - When selection non-empty: apply by pressing button (otherwise disabled when
    selection is empty, and hidden outside of selection mode), or shortkey (like
    `!`)

- Undo/redo history
  - Works for the entire state, thus switching freely between different UI modes
  - Buttons at the bottom of the screen + usual shortkeys

- Mode selection bar at bottom of the screen

- Implement Edit mode
  - The drop zones of Importing turn into `add flower` zones in positive gardens
  - `add petal` zones at the start, end and between petals of negative flowers
  - Negative flowers and positive petals can cropped/pulled (that is, removed)
    by clicking on them (scissors cursor icon)
  - Two clipboards, one for cropped flowers and one for pulled petals
  - `add flower/petal` zones have:
    - text edit to grow atom
      - one can imagine instead a button that launches/inlines a domain-specific
        GUI to build statements/objects in a custom domain, i.e. euclidian
        geometry
    - `grow` buttons to grow a flower/petal
    - free space to:
      - paste clipboard by clicking
      - reorder flowers by drag-and-drop
  - Flowers/petals created by `grow` buttons don't have polarity restrictions:
    everything can be removed, and things can be added anywhere
  - First-order:
    - `add variable` zones in all gardens, regardless of polarity
    - In a typed setting, one can imagine that it opens a custom GUI to choose a
      type (locally, or even in an online data base)

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

- Analogy between Proof/Edit modes and Survival/Creative modes in Minecraft