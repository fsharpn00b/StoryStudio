# Story Studio

Story Studio is a visual novel framework written in [F#](https://fsharp.org/) that uses [Fable](https://fable.io/).

# Features

- Adjustable dialogue reveal speed.
- Undo and redo by scrolling the mouse wheel.
- Save and load progress. Export and import saved games to and from local files.
- Skip transitions (background fade in/out, dialogue reveal) by clicking the mouse.

## For authors

- Simple scripting language lets you:
	- Fade backgrounds and characters in and out.
	- Play music.
	- Specify dialogue with a single-letter abbrevation for the character name and without needing quotes. For example:
        ```
	    c This is some dialogue.
        r This is a reply.
        ```
	- Branch using conditionals (if, elseif, else).
	- Break your story into scenes and jump between them.
	- Give players choices with menus and image maps.

- JavaScript support lets you:
	- Use JavaScript in dialogue, conditionals, and menus (for instance, you can show certain menu options only if their corresponding JavaScript expressions are true).
	- Store game state such as character statistics and inventory.
	- Include TypeScript types and lint your code before you run it.

[Configuration and hotkeys](https://github.com/fsharpn00b/StoryStudio/tree/main/docs/configuration.md)

[Scripting language reference](https://github.com/fsharpn00b/StoryStudio/tree/main/docs/commands.md)

# How to Run

1. Install [.NET 9.0 or later](https://dotnet.microsoft.com/en-us/download/dotnet/9.0).

1. Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm).

1. Clone the repo.

    ```bash
    git clone https://github.com/fsharpn00b/StoryStudio.git
    ```

1. Change directory into the cloned repo.

    ```bash
    cd StoryStudio
    ```

1. Restore .NET tools.

    ```bash
	dotnet tool restore
    ```

1. Restore .NET dependencies.

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

1. The previous command shows a URL such as `https://localhost:5173`. Open a browser to this URL:

    ```
    http://localhost:5173/0_pages/index.html
	```
	
    If the port number is different than `5173`, adjust the URL accordingly.

# How to deploy a visual novel

Follow the steps in [How to run](#how-to-run), but instead of `npm start`, run:

```bash
dotnet fable <path_to_src_folder> --run npx vite build --emptyOutDir
```

or:

```bash
npm run build
```

The first command is recommended because it clears the output from previous builds.

This command creates a `dist` folder in your project root folder. The `dist` folder contains your visual novel. To serve the Web page that contains your visual novel, you'll need to include a small portable Web server that runs on the target platform. Two such Web servers are [Caddy](https://github.com/caddyserver/caddy/releases) and [nginx](https://nginx.org/download.html).

Story Studio includes a [Caddy configuration file](/src/caddy_configuration) and a [Bash script](/src/run.sh) that runs Caddy. The deployment script copies both these files to the `dist` folder. `run.sh` expects to find Caddy at `dist/caddy/caddy`, so you might need to modify it if you use a different Web server. The Caddy configuration has the Web server serve the visual novel at `https://localhost/:8080`.

The deployment script is [vite.config.js](/vite.config.js).

# Plugins

Story Studio supports plugins so you can add React components written in JavaScript to your visual novel without having to modify the F# framework.

The plugin configuration file is [plugins.txt](/src/0_data/plugins.txt). By default, it loads the [example plugin](/src/0_data/plugins/Plugin_Template.fs). The example plugin simply shows an `h1` header with the word `Test`. You can show or hide it in your visual novel script as follows:

```
js
window.Interfaces.Plugin_Template.current.show ()
endjs

js
window.Interfaces.Plugin_Template.current.hide ()
endjs
```

The example plugin is written in F#, but when you build your visual novel, Fable transforms it to `/src/0_data/plugins/Plugin_Template.fs.js`. If you prefer JavaScript, you can copy `Plugin_Template.fs.js` to a new file and use it as a template to develop other plugins. If you want to write a plugin in F#, you must add it to the [Story Studio project](/src/App.fsproj) so Fable transforms it to JavaScript.

To use your plugin in a Story Studio visual novel script, you must register it:

    ```F#
    window?Plugins?<your_plugin_name> <- <your_plugin_component_name>
    ```

    ```JavaScript
    window.Plugins.<your_plugin_name> = ((props) => createElement(<your_plugin_component_name>, props));
    ```

`<your_plugin_name>` must match the plugin name specified in [plugins.txt](/src/0_data/plugins.txt).

`<your_plugin_component_name>` must match the component name in your component code:

    ```F#
    [<ReactComponent>]
    let private My_Plugin_Component
        (props : {| expose : IRefValue<I_My_Plugin_Component> |})
        : ReactElement =
    // Component code
    ```

    ```JavaScript
    function My_Plugin_Component(props) {
    ```

Also note the CSS import statement:

    ```F#
    importSideEffects "./Plugin_Template.css"
    ```

    ```JavaScript
    import "./Plugin_Template.css";
    ```

You should change this to point to the CSS file you want to use to style your component. Or, if you encode the style directly in the component, you can remove this statement.

# Notes

I wrote Story Studio to have a visual novel framework with better support for finding errors in my scripts at compile time instead of at run time. I previously tried and failed to find a way to do this in Ren'py, as described in [https://github.com/fsharpn00b/Snippets/tree/main/renpy](here).

Development is ongoing as there are more features I want to add.

# Screenshots

[![Menu](https://i.postimg.cc/PqfVT5FQ/1.jpg)](https://postimg.cc/0z30CPvz)
[![Saved game screen](https://i.postimg.cc/QMNYhdyL/2.jpg)](https://postimg.cc/NKVkxBtb)
[![Dialogue](https://i.postimg.cc/d0WH1cbt/3.jpg)](https://postimg.cc/PLwz39fB)
[![Dialogue](https://i.postimg.cc/pd0GTMwr/4.jpg)](https://postimg.cc/sBh96qDC)
[![Configuration screen](https://i.postimg.cc/JhQhjTBJ/5.png)](https://postimg.cc/wt7HpQ2T)
[![Configuration screen](https://i.postimg.cc/pTXLCDDf/6.png)](https://postimg.cc/XZmW7B1q)

# Credits

Story Studio includes a tiny visual novel to demonstrate its features. It based on the "Forest Hike" demo project included with Ren'py:
- https://www.freecodecamp.org/news/use-python-to-create-a-visual-novel/
- https://github.com/mogrady-personal/Forest-Hike

Character sprites:
https://fuelli.itch.io/free-to-use-character-sprites

Background music:
https://stocktune.com/free-music/whispers-of-autumn-love-292013-173203
