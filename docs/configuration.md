# Configuration

To open the configuration screen:
- Press the `Esc` key. This hotkey is hardcoded. You can also use `Esc` to close the configuration screen and return to the story.
- Press the `c` key. This hotkey is configurable, but `c` is the default.
- Select **Configuration** from the command menu at the upper right corner of the screen.

# Undo/redo

To move backward through the story:
- Scroll the mouse wheel up.
- Press the `b` key (default).
- Select **Undo** from the command menu.

To move forward to your most recent place in the story:
- Scroll the mouse wheel down.
- Press the `n` key (default).
- Select **Redo** from the command menu.

Notes:
- If you undo more than one step, and then left-click or press `space` to continue, or you make a choice at a menu or image map, the redo stack is erased.
- The undo/redo stack is not saved when you save your game. It applies only to your current playing session.

# Hotkeys

These are mostly self-explanatory, but here are additional notes.

- Continue: The default key is `space`
- Quick save: There is one quick-save slot.
- Export saved games to file: This lets you export all your saved games to a file. You can then import them on another computer.

    Note: Importing saved games does not delete the existing saved games. However, before importing any saved games, it's a good idea to export your existing saved games to be safe.

- Export current game to file: This lets you export the current game state to a file. You can then re-import it later, or import it on another computer. This is useful if your browser does not support storing your saved games in IndexedDB.
- Show/hide UI: This is useful for taking screenshots.

## Debug hotkeys

These hotkeys are used for debugging a visual novel.

- Show runner queue: Story Studio uses a queue to handle transitions such as fading in backgrounds. This outputs the current state of the queue to the developer console.
- Show character data: This outputs the current states of all characters (whether they are hidden or visible, and so on) to the developer console.
- Show background data: This outputs the current state of the background (what image is displayed, and so on) to the developer console.
- Check JavaScript: See [Validating JavaScript](./commands.md#validating-javascript).
- Show state: This outputs the current state of all game data stored in JavaScript to the developer console.
