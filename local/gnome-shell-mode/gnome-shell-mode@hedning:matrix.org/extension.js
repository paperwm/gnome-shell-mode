const uuid = "gnome-shell-mode@hedning:matrix.org";
const Extension = imports.misc.extensionUtils.extensions[uuid];
const emacs = Extension.imports.emacs;

function init() {
    print('init gnome-shell-mode server')
}

function enable() {
    print('enable gnome-shell-mode server')
}
function disable() {
    print('disable gnome-shell-mode server');
}
