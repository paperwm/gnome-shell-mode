## gnome-shell-mode

gnome-shell-mode makes it easy to interactively explore and evaluate javascript in a Gnome Shell session. It works through the dbus interface provided by Gnome Shell.

While gnome-shell-mode haven't crashed in a while for us
it's definitely alpha software and might still crash your Gnome Shell session, use with at your own risk.

## Installation

There's no melpa package yet, but it's quite easy to use the layer with Spacemacs.

Clone the repo and create a symlink named ~gnome-shell~ in the spacemacs ~private~ folder:
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

For instance <kbd>,sf</kbd> will evaluate the surrounding function and the evaluated region will pulse green or red depending on the success of the evaluation.

There's two non-standard keybindings:
- <kbd>Return</kbd> will evaluate the active region (if evil is used)
- <kbd>C-Return</kbd> will evaluate the active region, or the current line if there's no region active. It will then insert the result as a comment on the next line.

## Caveats

Not all methods of GObjects (g-object-introspected classes) complete before they're used the first time. This include alot of classes you'll interact with. eg. `MetaWindow`. We're unsure how to fix this. Suggestions are welcome.

Completion doesn't work for string and function objects.
