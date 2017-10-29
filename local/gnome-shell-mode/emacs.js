// -*- mode: gnome-shell; -*-

emacs = {};

emacs.verbose = true;

const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;
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

emacs.pp_rect = function(xywh) {
    let [x,y,w,h] = xywh.map( v => v.toFixed(1) );
    return `{ x: ${x}, y: ${y}, w: ${w}, h: ${h} }`
}

emacs.pretty_printers["Object"] = function(obj, key) {
    if (obj.toString !== Object.prototype.toString) {
        // The object have a custom toString method
        return obj.toString();
    } else {
        // Let JSON handle it
        return  obj;
    }
}

emacs.pretty_printers["Array"] = function(obj, key) {
    // Let JSON handle it
    return obj;
}


emacs.pretty_printers["Clutter_ActorBox"] = function(box, key) {
    let x = box.get_x()
    let y = box.get_y()
    let w = box.get_width()
    let h = box.get_height()
    return `ActorBox ${emacs.pp_rect([x,y,w,h])}`
}

emacs.pretty_printers["Meta_Rectangle"] = function(box, key) {
    return `Meta_Rectangle ${emacs.pp_rect([box.x, box.y, box.width, box.height])}`
}

emacs.pp_helper = function pp_helper(key, obj) {
        let type = typeof(obj);
        let pretty;
        if (type === "undefined") {
            pretty = "undefined";
        } else if (obj === null) {
            pretty = "null";
        } else if (type === "object") {
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
            // Let JSON handle it (Numbers, etc.)
            pretty = obj;
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

/** pathString: absolute path to a js file descending from the extension root */
function findExtensionRoot(pathString) {
    let path = Gio.file_new_for_path(pathString);
    let dir = path.get_parent();

    while (dir !== null) {
        let metadata = dir.get_child("metadata.json");
        if (metadata.query_exists(null)) {
            return dir.get_path();
        }
        dir = dir.get_parent();
    }
    return null;
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
</interface> \
</node> \
';


let DbusObject = {
    Eval: function (code, path) {
        let projectRoot = findExtensionRoot(path);

        emacs.module = {};

        if (projectRoot !== null) {
            // (projectRoot does not end with slash)
            let relPath = path.slice(projectRoot.length+1)

            let metadataFile = `${projectRoot}/metadata.json`;
            let uuid;
            if (GLib.file_test(metadataFile, GLib.FileTest.IS_REGULAR)) {
                const [success, metadata] = GLib.file_get_contents(metadataFile);
                uuid = JSON.parse(metadata.toString()).uuid;
            }

            // Set up module we're in
            if (relPath.endsWith('.js') && uuid !== undefined) {
                relPath = relPath.substring(0, relPath.length - 3);
                // We try in case the projectRoot module has syntax errors
                try {
                    let empty = {};
                    let Extension =
                        imports.misc.extensionUtils.extensions[uuid];

                    emacs.module = relPath.split('/').reduce((module, name) => {
                        if (module[name]) {
                            return module[name];
                        }
                        return empty;
                    }, Extension.imports);

                    // We're in a module and we can replace `var` with
                    // `emacs.module.` so that re-assignment works
                    if (emacs.module !== empty) {
                        code = code.replace(/^var /g, 'emacs.module.');
                        // rewrite function syntax assignment
                        code = code.replace(/^function\s+(.*)\(/,
                                            'emacs.module.$1 = function(');
                        code = code.replace(/^const /g, 'emacs.module.');
                        code = code.replace(/^let /g, 'emacs.module.');
                    }
                } catch(e) {
                    print(`Couldn't load module, will evaluate without: ${e.message}`)
                }
            }
        }

        let eval_result;
        let result;
        let success = true;
        try {
            eval_result =  (0, eval)(`with(emacs.module){ ${code} }`);
            result = {
                success: true,
            };

            try {
                result.value = emacs.pp_object(eval_result)

                if (eval_result && eval_result.constructor === Array) {
                    // Also return the actual object. Currently used by the
                    // completion code to avoid parsing a pretty printed array.
                    // When/if we define our own dbus service we can have a separate
                    // dbus method for completion and maybe remove this line.
                    if (eval_result.every(x => typeof(x) === "string")) {
                        // Other types can cause problems. eg. dbus fails if a
                        // object is cyclic.
                        result.raw_value = eval_result;
                    }
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
    }
};

emacs.dbusImpl = Gio.DBusExportedObject.wrapJSObject(EvalIface, DbusObject);
emacs.dbusImpl.export(Gio.DBus.session, '/gnome/shell/mode');

const JsParse = imports.misc.jsParse;

let _getAutoCompleteGlobalKeywords = () => {
    const keywords = ['true', 'false', 'null', 'new'];
    // Don't add the private properties of window (i.e., ones starting with '_')
    const windowProperties = Object.getOwnPropertyNames(window).filter(function(a){ return a.charAt(0) != '_' });

    return keywords.concat(windowProperties);
}

emacs.completion_candidates = (text) => {
    let AUTO_COMPLETE_GLOBAL_KEYWORDS = _getAutoCompleteGlobalKeywords();
    let [completions, attrHead] = JsParse.getCompletions(text, '', AUTO_COMPLETE_GLOBAL_KEYWORDS);

    let objectPath = text.split('.').slice(0, -1);
    let empty = {};
    let moduleObject = objectPath.reduce((object, path) => {
        if (object[path]) {
            return object[path];
        }
        return empty;
    }, emacs.module);

    for (let varname in moduleObject) {
        completions.push(varname);
    }

    let path;
    if (moduleObject === emacs.module || moduleObject === empty) {
        path = text.substring(0, text.length - attrHead.length - 1);
    } else {
        path = moduleObject;
    }

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
