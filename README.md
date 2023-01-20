## gnome-shell-mode - Looking Glass for Emacs

gnome-shell-mode makes it easy to interactively explore and evaluate javascript in a [Gnome Shell](https://wiki.gnome.org/Projects/GnomeShell) session. It supports file local evaluation and auto-completion when working on a loaded Gnome Shell extension.

## Installation

There's no melpa package yet, but it's quite easy to install the package(s) manually.

### Doom emacs

In your `packages.el`:

```emacs-lisp
(package! gnome-shell-mode
  :recipe (:host github :repo "paperwm/gnome-shell-mode"
           :files ("local/gnome-shell-mode/*")))
(package! company-gnome-shell
  :recipe (:host github :repo "paperwm/gnome-shell-mode"
           :files ("local/company-gnome-shell/*.el")))
```

and in your `config.el`:

```emacs-lisp
(use-package! gnome-shell-mode
  :defer t
  :commands (gnome-shell-mode)
  :config
  (setq-hook! 'gnome-shell-mode-hook
    mode-name "GJS")

  (map!
   :map gnome-shell-mode-map
   :v "<return>" 'gnome-shell-send-region
   :gvni "C-<return>" 'gnome-shell-repl

   :map gnome-shell-mode-map
   :localleader
   :gnv :desc "Reload buffer" "r" 'gnome-shell-reload
   :desc "Reload session" "R" 'gnome-shell-restart
   :desc "Launch session" "l" 'gnome-shell-launch-session
   :desc "Clear output" "c" 'gnome-shell-clear-output-at-point

   (:prefix ("g" . "jump")
     :desc "Jump to definition" "g" '+lookup/definition)

   (:prefix ("s" . "eval in session")
     :desc "Eval buffer" "b" 'gnome-shell-send-buffer
     :desc "Eval function" "f" 'gnome-shell-send-proc
     :desc "Eval function" "d" 'gnome-shell-send-proc
     :desc "Eval line" "l" 'gnome-shell-send-current-line
     :desc "Eval region" "r" 'gnome-shell-send-region)

   (:prefix ("e" . "eval in session")
     :desc "Eval buffer" "b" 'gnome-shell-send-buffer
     :desc "Eval function" "f" 'gnome-shell-send-proc
     :desc "Eval function" "d" 'gnome-shell-send-proc
     :desc "Eval line" "l" 'gnome-shell-send-current-line
     :desc "Eval region" "r" 'gnome-shell-send-region)

   (:prefix ("o" . "output")
     :desc "Clear all output" "c" 'gnome-shell-clear-output
     :desc "Copy output" "y" 'gnome-shell-copy-output)

   (:prefix ("h" . "help")
     :desc "Lookup at point" "h" 'gnome-shell-look-up-function-at-point
     )
   )
  )

(use-package! company-gnome-shell
  :defer t
  :commands (company-gnome-shell)
  :init
  (set-company-backend! 'gnome-shell-mode 'company-gnome-shell))
```

### Spacemacs 

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

### Vanilla emacs

Add both `local/gnome-shell-mode` and `local/company-gnome-shell` to the `load-path`.

Then add this to `init.el`:

```emacs-lisp
    (require 'company)

    (require 'gnome-shell-mode)
    (require 'company-gnome-shell)

    ;; Most staight forward but might mess up company in other modes?
    (eval-after-load "company"
     (add-to-list 'company-backends 'company-gnome-shell))
```

See `gnome-shell-mode-pkg.el` and `company-gnome-shell.el` for list of dependencies.

NB: The rest of the readme describe the keybindings defined by the spacemacs layer. Some vanilla emacs bindings are also defined by default. See the bottom of `gnome-shell-mode.el`. 

## Initial Setup

NOTE: For older gnome versions this is not needed. But it is required for at least Gnome 43.

This is required because we install a gnome extensions (called gnome-shell-mode) that is required to provide the functionality of gnome-shell-mode.

You need to enable `unsafe_mode` in the gnome-shell. You can do this by executing the following in LookingGlass (i.e. hit <kbd>Alt+F2</kbd>, type `lg` and <kbd>Enter</kbd>):

``` javascript
global.context.unsafe_mode = true
```

Then start Emacs and use one of the functions/keybindings of gnome-shell-mode.

Afterwards that you can disable `unsafe_mode` again and gnome-shell-mode should continue to work.

If you skip this step you will see an error similar to the following in Emacs and none of the functionality of gnome-shell-mode will work:

```
dbus-call-method: D-Bus error: "org.freedesktop.DBus.Error.UnknownMethod", "Object does not exist at path “/gnome/shell/mode”"
```

## Usage

Make sure you're in gnome-shell-mode (e.g. by using <kbd>M-x gnome-shell-mode</kbd>). All the actions will then be under the major-mode leader key (<kbd>M-m</kbd> or <kbd>,</kbd>).

For instance <kbd>,sf</kbd> will evaluate the surrounding function and the evaluated region will pulse green or red depending on the success of the evaluation. If an error occurred, the position reported by gjs will be marked as a flycheck error.

There's two non-standard keybindings:
- <kbd>Return</kbd> will evaluate the active region (if evil is used), the result will be shown in the minibuffer.
- <kbd>C-Return</kbd> will evaluate the active region, or the current line if there's no region active. The result will be added in an overlay ala. magit-blame. The overlay can be cleared by <kbd>,c</kbd> or by deleting the input.

The global variable `$` contains the value of the most recent evaluation.

### Launch session

By default gnome-shell-mode connects to the live Gnome Shell session. This can be a bit risky however, especially on Wayland where restart doesn't work.

Run `M-x gnome-shell-launch-session`, (`, l` if using spacemacs), to launch and connect to a nested session, the session's log will popup in a new buffer too. When a session is already running `, l`  will simply take you to the log. To launch a clean session close the nested Gnome Shell window first.

If the nested session encounter runtime errors they will be reported as errors in the correct buffer (using flycheck).

### Reload

The mode supports reloading buffers with <kbd>, r</kbd>. This works by first disabling the extension, re-evaluating the whole buffer in the correct scope, and then enabling the extension again.

To get full use of this, `enable` and `disable` need to assemble and disassemble all the state in the extension. A good way to handle this is having `enable` and `disable` functions in every module, making the exension's `enable` and `disable` just call out to the module's functions.

### Restart

Pressing <kbd>, R</kbd> in spacemacs will disable the extension the current buffer is part of and then restart Gnome Shell. This gives the extension a change to clean up and save any state making the restart less disruptive. This can also be accessed through the interactive function `gnome-shell-restart`. Note that restart is only supported on X11.

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

Not all methods of GObjects (g-object-introspected classes) complete before they're used the first time. This include a lot of classes you'll interact with. eg. `MetaWindow`. Fixed in [gjs 1.55.1](https://gitlab.gnome.org/GNOME/gjs/commit/8e982d37e9fd9adcf9e87573d91cbffaf1e7b509)

While gnome-shell-mode shouldn't cause any crashes by itself, evaluating javascript in Gnome Shell is not completely safe, some code will result in a crash. Eg. looking up a non-existing dconf/schema name will cause a crash.
