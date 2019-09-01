const uuid = "gnome-shell-mode@hedning:matrix.org";
var Extension;
if (imports.misc.extensionUtils.extensions) {
    Extension = imports.misc.extensionUtils.extensions[uuid];
} else {
    Extension = imports.ui.main.extensionManager.lookup(uuid);
}
const Emacs = Extension.imports.emacs;
const Gio = imports.gi.Gio;
const GObject = imports.gi.GObject;

function init() {
    print('init gnome-shell-mode server')
    window.emacs = {};

    emacs.verbose = true;

    emacs.find_property = GObject.Object.find_property;
    emacs.list_properties = GObject.Object.list_properties;

    // Probably possible to extract from the error stack, but hardcode for now
    // Note: will change if newEval is redefined, restart gnome-shell when making
    // changes to this code for now
    // in gnome-shell 3.24.3 eval lineNumber is correct!
    emacs.eval_line_offset = 0; 
}

const EvalIface =
      '\
<node> \
<interface name="gnome.shell.mode"> \
<method name="Eval"> \
    <arg type="s" direction="in" name="script" /> \
    <arg type="s" direction="in" name="path" /> \
    <arg type="b" direction="out" name="success" /> \
    <arg type="s" direction="out" name="result" /> \
</method> \
<method name="Reload"> \
    <arg type="s" direction="in" name="code" /> \
    <arg type="s" direction="in" name="path" /> \
    <arg type="b" direction="out" name="success" /> \
    <arg type="s" direction="out" name="result" /> \
</method> \
<method name="Restart"> \
    <arg type="s" direction="in" name="path" /> \
</method> \
<method name="Complete"> \
    <arg type="s" direction="in" name="code" /> \
    <arg type="s" direction="in" name="path" /> \
    <arg type="as" direction="out" name="result" /> \
</method> \
</interface> \
</node> \
';


let dbusImpl;
function enable() {
    print('enable gnome-shell-mode server')

    let DbusObject = {
        Eval: function (code, path) {
            return Emacs.Eval(code, path);
        },
        Reload: function (code, path) {
            return Emacs.Reload(code, path);
        },
        Complete: function (code, path) {
            return Emacs.completionCandidates(code, path);
        },
        Restart: function (path) {
            Emacs.Restart(path);
        }
    };

    dbusImpl = Gio.DBusExportedObject.wrapJSObject(EvalIface, DbusObject);
    dbusImpl.export(Gio.DBus.session, '/gnome/shell/mode');
}
function disable() {
    print('disable gnome-shell-mode server');
    dbusImpl.unexport();
}
