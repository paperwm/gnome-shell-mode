const uuid = "gnome-shell-mode@hedning:matrix.org";
const Extension = imports.misc.extensionUtils.extensions[uuid];
const Emacs = Extension.imports.emacs;
const Gio = imports.gi.Gio;
const GObject = imports.gi.GObject;

emacs = {};

emacs.verbose = true;

emacs.find_property = GObject.Object.find_property;
emacs.list_properties = GObject.Object.list_properties;

// Probably possible to extract from the error stack, but hardcode for now
// Note: will change if newEval is redefined, restart gnome-shell when making
// changes to this code for now
// in gnome-shell 3.24.3 eval lineNumber is correct!
emacs.eval_line_offset = 0; 

emacs.completion_candidates = Emacs.completion_candidates;

function init() {
    print('init gnome-shell-mode server')
}

let dbusImpl;
function enable() {
    print('enable gnome-shell-mode server')
    dbusImpl = Gio.DBusExportedObject.wrapJSObject(Emacs.EvalIface, Emacs.DbusObject);
    dbusImpl.export(Gio.DBus.session, '/gnome/shell/mode');
}
function disable() {
    print('disable gnome-shell-mode server');
    dbusImpl.unexport();
}
