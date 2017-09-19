// -*- mode: gnome-shell; -*-

emacs = {};

emacs.verbose = true;

GObject = imports.gi.GObject;
emacs.find_property = GObject.Object.find_property;
emacs.list_properties = GObject.Object.list_properties;

// Probably possible to extract from the error stack, but hardcode for now
// Note: will change if newEval is redefined, restart gnome-shell when making
// changes to this code for now
emacs.eval_line_offset = 147;

emacs.originalDBusEval = imports.ui.shellDBus.GnomeShell.prototype.Eval;

imports.ui.shellDBus.GnomeShell.prototype.Eval = function newEval(code) {
    let eval_result;
    let result;
    let success = true;
    try {
        eval_result = eval(code);
        result = {
            success: true,
        };

        let type = typeof(eval_result);
        if (type === "undefined") {
            result.undefined = true;
        } else if (eval_result === null) {
            result.value = null;
        } else if (type === "object" && eval_result.__metaclass__) {
            // GObjects are not serialized properly by JSON.stringify
            result.value = eval_result.toString();
        } else if (type === "function") {
            // Neither are functions
            result.value = eval_result.toString();
        } else {
            // Use JSON.stringify as a poor man's pretty printer
            result.value = JSON.stringify(eval_result);

            // Also return the actual object. Currently used by the completion
            // code to avoid parsing a pretty printed array.
            // When/if we define our own dbus service we can have a separate
            // dbus method for completion and maybe remove this line.
            result.raw_value = eval_result;
        }
    } catch(e) {
        // Note: JSON.stringify(e) doesn't reliably include all fields
        result = {
            success: false,
            value: e.message,
            stack: e.stack,
            lineNumber: e.lineNumber - emacs.eval_line_offset,
            columnNumber: e.columnNumber, 
            // e.constructor
            file: e.file
        }
        emacs.lasterr = e; // for debugging

        success = false;
    }
    return [success, JSON.stringify(result)];
};

const JsParse = imports.misc.jsParse;

let commandHeader = 'const Clutter = imports.gi.Clutter; ' +
                    'const GLib = imports.gi.GLib; ' +
                    'const GObject = imports.gi.GObject; ' +
                    'const Gio = imports.gi.Gio; ' +
                    'const Gtk = imports.gi.Gtk; ' +
                    'const Mainloop = imports.mainloop; ' +
                    'const Meta = imports.gi.Meta; ' +
                    'const Shell = imports.gi.Shell; ' +
                    'const Main = imports.ui.main; ' +
                    'const Lang = imports.lang; ' +
                    'const Tweener = imports.ui.tweener; ' +
                    /* Utility functions...we should probably be able to use these
                     * in the shell core code too. */
                    'const stage = global.stage; ';

let _getAutoCompleteGlobalKeywords = () => {
    const keywords = ['true', 'false', 'null', 'new'];
    // Don't add the private properties of window (i.e., ones starting with '_')
    const windowProperties = Object.getOwnPropertyNames(window).filter(function(a){ return a.charAt(0) != '_' });
    const headerProperties = JsParse.getDeclaredConstants(commandHeader);

    return keywords.concat(windowProperties).concat(headerProperties);
}

emacs.completion_candidates = (text) => {
    let AUTO_COMPLETE_GLOBAL_KEYWORDS = _getAutoCompleteGlobalKeywords();
    let [completions, attrHead] = JsParse.getCompletions(text, commandHeader, AUTO_COMPLETE_GLOBAL_KEYWORDS);

    let path = text.substring(0, text.length - attrHead.length - 1);
    try {
        let obj = eval(path);
        if (obj && typeof(obj) === "object") {
            // NB: emacs.list_properties.call(x) crashes gnome-shell when x is a
            //     (non-empty) string or number

            emacs.list_properties.call(obj)
            // list_properties gives names with "-" not "_"
                .forEach((x) => { completions.push(x.name.replace(/-/g, "_")) });

            if (obj.prototype) {
                let [pCompletions, _] = JsParse.getCompletions(text + ".prototype", commandHeader, AUTO_COMPLETE_GLOBAL_KEYWORDS)
                pCompletions.forEach((x) => { completions.push(x)});
            }
        }
    } catch(e) {};

    return completions.filter((x) => {return x.startsWith(attrHead); });
};

