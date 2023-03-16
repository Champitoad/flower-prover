- Fix bug with Unlock/Close not triggering on click

- Use HTML5 drag-and-drop API instead of custom handling of mousedown and
  mouseup events (and try to support mobile touchscreens)

- Implement Fencing proof interaction with multiselection
  - Start with long press to support mobile
  - Apply by pressing key, or button to support mobile
  - Cancel by clicking on any garden, or by clicking on close button

- Mode selection bar at bottom of the screen

- Implement Edit mode

- Implement Navigation mode
  - Global context at top of the screen
  - Focus on flower by clicking
  - Jump to flower in global context by clicking
  - Unfocus by clicking on focused flower
  - Scale/Unscale animation?