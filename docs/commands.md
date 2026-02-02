# Commands

## Fade in background

Syntax:

```
fadein <background name> <transition time>
```

Example:

```
// Fade in the background named `day` over 2 seconds.
fadein day 2.0
```

Background names and URLs are specified in [/src/0_data/bgs.txt](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/bgs.txt). For example:

```
{
  "name": "day",
  "url": "/0_assets/bg/forest/day.jpg"
}
```

## Fade out background

Syntax:

```
fadeout <transition time>
```

Example:
```
// Fade out the current background over 2 seconds.
fadeout 2.0
```

## Cross fade background

Syntax:

```
fadeto <background name> <transition time>
```

Example:

```
// Cross fade from the current background to the background named `night` over 2 seconds.
fadein night 2.0
```

## Fade in character

Syntax:

```
fadein <character short name> <sprite name> <position> <transition time>
```

Example:

```
// Fade in the character with short name `l`, using the sprite named `happy`, 25% across the screen from the left, over 1 second.
fadein l happy 25 1.0
```

Character short names, full names, and sprite URLs are specified in [/src/0_data/chars.txt](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/chars.txt). For example:

```
{
  "full_name": "Laura",
  "short_name": "l",
  "height": 80,
  "sprites": {
    "happy": "/0_assets/chars/laura/small/happy.png",
    "angry": "/0_assets/chars/laura/small/angry.png",
    "sad": "/0_assets/chars/laura/small/sad.png"
  }
}
```

## Fade out character

Syntax:

```
fadeout <character short name> <transition time>
```

Example:

```
// Fade out the character with short name `l` over 1 second.
fadeout l 1.0
```

## Cross fade character

Syntax:

```
fadeto <character short name> <sprite name> <transition time>
```

Example:

```
// Cross fade the character with short name `l` from the current sprite to the sprite named `sad` over 1 second.
fadeto l sad 1.0
```

## Move in character

Syntax:

```
movein <character short name> <sprite name> <direction> <position> <transition time>
```

`direction` must be `left` or `right`.

Example:

```
// Move the character with short name `l`, using the sprite named `happy`, in from the left of the screen, to 25% across the screen from the left, over 1 second.
movein l happy left 25 1.0
```

## Move out character

Syntax:

```
moveout <character short name> <direction> <transition time>
```

Example:

```
// Move the character with short name `l` off the right side of the screen over 1 second.
moveout l right 1.0
```

## Fade out background, all characters, and dialogue box

Syntax:

```
fadeoutall <transition time>
```

Example:

```
// Fade out the current background, all characters, and the dialogue box over 1 second.
fadeoutall 1.0
```

## Dialogue

Syntax:

```
<character short name> <dialogue>
```

After the character short name, the rest of the line is dialogue, ending with the newline.

Example:

```
l Which way to go?
t How about left?
l Okay!
```

## Play music

Syntax:

```
play <music track name>
```

Example:

```
// Play the music track named `autumn`.
play autumn
```

Music tracks are specified in [/src/0_data/music.txt](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/music.txt). For example:

```
{
  "name": "autumn",
  "url": "/0_assets/music/whispers-of-autumn-love-stocktune.mp3"
}
```

## Stop music

Syntax:

```
stopmusic
```

Stops playing the current music track.

## Menu

Syntax:

```
menu <menu name> <description>
  <menu item index> <description> [/ <conditional>]
  ...
endmenu
```

Example:

```
menu direction Which way?
// The first menu option is only available is `is_lost` is `false`.
1 left / false === window.state.player_state.is_lost
2 right
endmenu
```

Notes:
- A menu must contain at least one item that does not depend on a conditional.
- After the player chooses a menu item, the menu item index of the player's choice is assigned to a variable named after the menu. In the example, if the player chooses `left`, a variable named `direction` is created with the value `1`. You can then use this variable in JavaScript statements or branching conditionals. For example:

    ```
    if 1 === direction
    // If the player goes left, they get lost.
        js window.state.player_state.is_lost = true;
    endif
    ```

## Image map

Syntax:

```
imagemap <image map name> <background name> <transition time>
  <image map item index> <x1> <y1> <x2> <y2> [/ <conditional>]
  ...
endimagemap
hideimagemap <transition time>
```

Example:

```
// Show an image map using the background image named `overhead`. Fade in the background image over 1 second.
imagemap direction overhead 1.0
// Hotspot 1 has coordinates x1 0%, y1 0%, x2 25%, x2 25%.
1 0 0 25 25 / false === window.state.player_state.is_lost
// Hotspot 2 has coordinates x1 25%, y1 25%, x2 50%, y2 50%.
2 25 25 50 50
endimagemap
// After the player chooses, fade out the background image over 1 second.
hideimagemap 1.0
```

Notes:
- An image map must contain at least one item that does not depend on a conditional.
- As with [Menu](#menu), after the player chooses an image map item, the image map item index of the player's choice is assigned to a variable named after the image map. In the example, if the player chooses the first item, a variable named `direction` is created with the value `1`.

Background names and URLs are specified in [/src/0_data/bgs.txt](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/bgs.txt). For example:

```
{
  "name": "day",
  "url": "/0_assets/bg/forest/day.jpg"
}
```

## Jump

Syntax:

```
jump <scene name>
```

Example:

```
// Jump to the scene named day_2.
jump day_2
```

Notes:
- Scene scripts are in [/src/0_data/scenes/](https://github.com/fsharpn00b/StoryStudio/tree/main/src/0_data/scenes/).
- A scene name is simply the script filename without the `.txt` extension. For example, if you have a script with filename `day_1.txt`, the scene name is `day_1`.
- The first script to run is always [start.txt](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/scenes/start.txt). In general, you should not jump to this scene.

## Branching

Syntax:

```
if <conditional>
  <commands>
endif
```

```
if <conditional>
  <commands>
else
  <commands>
endif
```

```
if <conditional>
  <commands>
elseif <conditional>
  <commands>
else
  <commands>
endif
```

Example:

```
if true === window.state.player_state.is_lost
  l Great, we're lost in the woods.
else
  l Did you get us lost again?
  t Nope!
endif
```

Notes:
- Each conditional must be valid JavaScript.
- You can have zero, one, or many `elseif` branches.
- You can use `elif` in place of `elseif`.

## Comments

Syntax:

```
// <comment>
```

```
/*
<multi-line comment>
*/
```

## JavaScript

Syntax:

```
js <statement>
```

```
js
  <statement>
  ...
endjs
```

### Persistence

Story Studio evaluates each JavaScript statement or block independently. This means the local environment does not persist from one evaluation to the next. For example, the following code:

```
js
var x = 1;
endjs

js
console.log (x);
endjs
```

results in error: `x is not defined`.

To address this, use the `window` namespace:

```
js
window.x = 1;
endjs

js
console.log (window.x);
endjs
```

Also, when you show a [menu](#menu) or [image map](#image-map), the resulting variable is automatically added to the local environment for each JavaScript statement or block from then on.

### Validating JavaScript

To validate your JavaScript:

1. Press `Shift + J` (default key binding). This extracts all JavaScript from your scripts and exports it to a file.
    - If you save the file with a different name than the default, make sure the file name has the `.ts` extension.
2. Run:

```
npx tsc --noEmit --strict <filename>
```

To install TypeScript, run:

```
npm install typescript --save-dev
```

Notes:

- `Shift + J` also exports JavaScript conditionals in:
    - [`if` and `elseif` branches](#branching)
    - [menu](#menu) items
    - [image map](#image-map) items
- `Shift + J` includes any TypeScript files you have in [/src/0_data/ts/](https://github.com/fsharpn00b/StoryStudio/blob/main/src/0_data/ts/). These files are loaded in alphanumeric order. You can use these files to define types to check your JavaScript code for type safety, but this isn't required.
- If, after installing TypeScript, you run `npx tsc...` and get the message:

    ```
    Need to install the following packages:
    tsc@<version>
    ```

    you might have saved the exported JavaScript file to a folder outside where you installed TypeScript.

## Notifications

### Temporary notification

Syntax:

```
notify <text> endnotify
```

Example:

```
notify This is a temporary notification. endnotify
```

### Permanent notification

Syntax:

```
status <text> endstatus
```

Example:

```
status This is a permanent notification. endstatus
```

Notes:
- Permanent notifications are displayed above temporary notifications.
- Temporary notification display time can be configured in the configuration screen, which you can access with the default hotkey `c`.
