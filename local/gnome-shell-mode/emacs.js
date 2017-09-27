// -*- mode: gnome-shell; -*-

emacs = {};

emacs.verbose = true;

GObject = imports.gi.GObject;
emacs.find_property = GObject.Object.find_property;
emacs.list_properties = GObject.Object.list_properties;

// Probably possible to extract from the error stack, but hardcode for now
// Note: will change if newEval is redefined, restart gnome-shell when making
// changes to this code for now
// in gnome-shell 3.24.3 eval lineNumber is correct!
emacs.eval_line_offset = 0; 

/// Add custom printers here indexed by constructor name
// (obj, key) => String or primitive to be serialized further to JSON
// `key` is undefined if top-level object
emacs.pretty_printers = {}

emacs.pp_helper = function pp_helper(key, obj) {
        let type = typeof(obj);
        let pretty;
        if (type === "undefined") {
            pretty = "undefined";
        } else if (obj === null) {
            pretty = "null";
        } else if (type === "object"
                   && obj.constructor !== Object
                   && obj.constructor !== Array) {
            // Note: GObjects are not serialized properly by JSON.stringify
            //       (and we want custom handling anyway)
            let constructor_name = obj.constructor.name;
            let custom_pp_fn = emacs.pretty_printers[constructor_name];
            if (custom_pp_fn) {
                pretty = custom_pp_fn(obj, key);
            } else {
                pretty = obj.toString();
            }
        } else if (type === "function") {
            // Just print the whole definition
            pretty = obj.toString();
        } else if (type === "string") {
            // Could special case string so we're sure it's easier to
            // differentiate between a string and a custom string representation
            // of an object. Eg. by surrounding it by single quotes.
            pretty = obj
        } else {
            if (obj.toString !== Object.prototype.toString) {
                // The object have a custom toString method
                pretty = obj.toString();
            } else {
                // Let JSON handle it
                pretty = obj;
            }
        }
        return pretty;
    }

emacs.pp_object = function(obj) {
    if (obj !== null && typeof(obj) === "object"
        && (obj.constructor === Object || obj.constructor === Array))
    {
        // Use JSON.stringify as a poor man's pretty printer for simple
        // composite objects
        return JSON.stringify(obj, emacs.pp_helper);
    } else if(typeof(obj) === "string") {
        // A pretty string have quotes around it to not conceal it's true nature
        return JSON.stringify(obj);
    } else {
        // Top level simple or complex constructor
        let pretty = emacs.pp_helper(undefined, obj);
        if(typeof(pretty) !== "string") {
            // Emacs expects a string, even for numbers
            pretty = JSON.stringify(pretty);
        }
        return pretty;
    }
}

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

        try {
            result.value = emacs.pp_object(eval_result)

            if(eval_result && eval_result.constructor === Array) {
                // Also return the actual object. Currently used by the
                // completion code to avoid parsing a pretty printed array.
                // When/if we define our own dbus service we can have a separate
                // dbus method for completion and maybe remove this line.
                result.raw_value = eval_result;
            }
        } catch(e) {
            throw new Error("Error during pretty printing: " + e.message);
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


function pp_test_repl() {
    "asdf"
    a={a:gobj}
    //: {"a":"[object instance proxy GType:MetaWindowX11 jsobj@0x7fa7beb07070 native@0x444eab0]"}
    gobj = global.display.focus_window;
    //: [object instance proxy GType:MetaWindowX11 jsobj@0x7fa7beb07070 native@0x444eab0]
    emacs.pp_object(undefined)
    //: "undefined"
    emacs.pp_object(null)
    //: "null"
    emacs.pp_object(1)
    "asdf"
    a={a:gobj}
    //: {"a":"[object instance proxy GType:MetaWindowX11 jsobj@0x7fa7beb07070 native@0x444eab0]"}
    emacs.pp_object("a string")
    //: "\"a string\""
    a=emacs.pp_object({key:2})
    //: "{\"key\":2}"
    a=emacs.pp_object({key:"a string"})
    //: "{\"key\":\"a string\"}"
    a=emacs.pp_object({key:[gobj]})
    //: "{\"key\":[\"[object instance proxy GType:MetaWindowX11 jsobj@0x7fa7beb07070 native@0x444eab0]\"]}"
    JSON.stringify({key:2, nested:{a:2.3}})
    //: "{\"key\":2,\"nested\":{\"a\":2.3}}"
}
