# Story Studio

Story Studio is a visual novel framework that uses [Fable](https://fable.io/).

# Features

- Adjustable dialogue reveal speed.
- Undo and redo by scrolling the mouse wheel.
- Save and load progress.
- Skip transitions (background fade in/out, dialogue reveal) by clicking the mouse.

## For authors

- Simple scripting language.
	- Fade backgrounds and characters in and out.
	- Play music.
	- You can specify dialogue with a single-letter abbrevation for the character name and without needing quotes.
	- Conditionals (If, ElseIf, Else).
	- Break your script into scenes and jump between them.
	- Menus to give players choices.

- JavaScript support.
	- You can use JavaScript in dialogue, conditionals, and menus (for instance, you can show certain menu options only if their corresponding JavaScript expressions are true).
	- This lets you store game state such as character statistics and inventory.
	- You can include TypeScript types and lint your code before you run it.

# How to Run

1. Install [.NET 9.0 or later](https://dotnet.microsoft.com/en-us/download/dotnet/9.0).

1. Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)

1. Clone the repo.

    ```bash
    git clone https://github.com/fsharpn00b/StoryStudio.git
    ```

1. Change directory into the cloned repo.

    ```bash
    cd StoryStudio
    ```

1. Restore the .NET solution.

    ```bash
    dotnet restore
    ```

1. Restore Node.js packages.

    ```bash
    npm install
    ```

1. Build and run the project.

    ```bash
    npm start
    ```

1. The previous command shows a URL such as `https://localhost:5173`. Open a browser to this URL.

# Notes

I wrote Story Studio to have a visual novel framework with better support for finding errors in my scripts at compile time instead of at run time. I previously tried and failed to find a way to do this in Ren'py, as described [https://github.com/fsharpn00b/Snippets/tree/main/renpy](here).

Development is ongoing as there are more features I want to add.

# Screenshots

[![Menu](https://i.postimg.cc/PqfVT5FQ/1.jpg)](https://postimg.cc/0z30CPvz)
[![Saved game screen](https://i.postimg.cc/QMNYhdyL/2.jpg)](https://postimg.cc/NKVkxBtb)

# Credits

Story Studio includes a tiny visual novel to demonstrate its features. It based on the "Forest Hike" demo project included with Ren'py:
https://www.freecodecamp.org/news/use-python-to-create-a-visual-novel/
https://github.com/mogrady-personal/Forest-Hike

Character sprites:
https://fuelli.itch.io/free-to-use-character-sprites

Background music:
https://stocktune.com/free-music/whispers-of-autumn-love-292013-173203
