## gnome-shell-mode - Looking Glass for Emacs

gnome-shell-mode makes it easy to interactively explore and evaluate javascript in a [Gnome Shell](https://wiki.gnome.org/Projects/GnomeShell) session. It supports file local evaluation and auto-completion when working on a loaded Gnome Shell extension.

## Installation

There's no melpa package yet, but it's quite easy to use the layer with Spacemacs.

Clone the repo and create a symlink named `gnome-shell` in the spacemacs `private` folder:
```shell
git clone https://github.com/paperwm/gnome-shell-mode.git ~/the/destination
ln -s ~/the/destination /.emacs.d/private/gnome-shell
```
Add gnome-shell to your list of Spacemacs layers:

```emacs-lisp
   dotspacemacs-configuration-layers
   '(
     ...
     gnome-shell
     ...
    )
```

Restart emacs and you're ready to go.

## Usage

Make sure you're in gnome-shell-mode (eg. by using <kbd>M-x gnome-shell-mode</kbd>). All the actions will then be under the major-mode leader key (<kbd>M-m</kbd> or <kbd>,</kbd>).

For instance <kbd>,sf</kbd> will evaluate the surrounding function and the evaluated region will pulse green or red depending on the success of the evaluation. If an error occurred, the position reported by gjs will be marked as a flycheck error.

There's two non-standard keybindings:
- <kbd>Return</kbd> will evaluate the active region (if evil is used)
- <kbd>C-Return</kbd> will evaluate the active region, or the current line if there's no region active. It will then insert the result as a comment on the next line.

### Reload

The mode supports reloading buffers with <kbd>, r</kbd>. This works by first disabling the extension, re-evaluating the whole buffer in the correct scope, and then enabling the extension again.

To get full use of this, `enable` and `disable` need to assemble and disassemble all the state in the extension. A good way to handle this is having `enable` and `disable` functions in every module, making the exension's `enable` and `disable` just call out to the module's functions.

### Documentation lookup

There's basic support for documentation lookup using <kbd>, h h</kbd>. This will prompt you with a list of known symbols matching the current word, selecting one will open the documentation of that symbol in your browser.

### Gnome Shell extension support

Auto-completion and evaluation happens in the file local scope when editing a loaded extension, or a file in the Gnome Shell source tree. When editing a file not part of an extension the system creates an ad-hoc scope for the file.

More specifically, if there's an `imports.some.path` object corresponding to the file being edited the scope of evaluation will be `imports.some.path` (or `someExtension.imports.some.path` in the case of extension code).

A small example of how this works in practice. Lets say you have a successfully loaded extension in the directory `MyExtension/`and you have some silly functions in `MyExtension/functions.js`:

```javascript
function helloWorld (hello, world) {
  return `${hello} ${world}`;
}

function printHelloWorld() {
  print(helloWorld('hello', 'world'));
}

```

Now yout want `helloWorld` to also add some exclamation marks:
```javascript
function helloWorld (hello, world) {
  return `${hello} ${world}!!!`;
}
```

After having made this change you can simply re-evaluate the function (eg. by <kbd>, s f</kbd>) and `printHelloWorld` will pick up the change.

This is done by looking up the extension object through the `uuid` from the `metadata.json` file, and then looking up the module object through the extension relative file path:
 ```javascript
let Extension = imports.misc.extensionUtils.extensions[uuid];
let module = Extension.imports.path.to.current.file;
```

Having the module object we can simply use ``eval(`with(module) { ${code} }`)`` so re-evaluated code will have the correct closure.

Reassignment relies on SpiderMonkey's built in parser. We traverse the top level statements, replacing all variable and function declarations. So eg. `function name () {}` gets translated to `module.name = function () {}` and `var foo = 'bar';` to `module.foo = 'bar';`. Having a proper parse tree means we can handle complex assignments with descructuring too (eg. 'let [foo, bar] = ...').

## Caveats

Not all methods of GObjects (g-object-introspected classes) complete before they're used the first time. This include a lot of classes you'll interact with. eg. `MetaWindow`. We're unsure how to fix this. Suggestions are welcome.

Completion doesn't work for string and function objects.

While gnome-shell-mode shouldn't cause any crashes by itself, evaluating javascript in Gnome Shell is not completely safe, some code will result in a crash. Eg. looking up a non-existing dconf/schema name will cause a crash.
