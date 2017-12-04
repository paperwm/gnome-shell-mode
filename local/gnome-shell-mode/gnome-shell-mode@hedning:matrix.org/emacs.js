// -*- mode: gnome-shell; -*-

const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;
const GObject = imports.gi.GObject;

/** pathString: absolute path to a js file descending from the extension root */
function findExtensionRoot(pathString) {
    let path = Gio.file_new_for_path(pathString);
    let dir = path.get_parent();

    while (dir !== null) {
        let metadata = dir.get_child("metadata.json");
        let jsResource = dir.get_child("js-resources.gresource.xml");
        if (metadata.query_exists(null)) {
            return ['extension', dir.get_path()];
        } else if (jsResource.query_exists(null)) {
            // Indicate that we're in a the gnome-shell js file
            return ['shell', dir.get_path()];
        }
        dir = dir.get_parent();
    }
    return [null, null];
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
    <arg type="s" direction="in" name="path" /> \
    <arg type="b" direction="out" name="success" /> \
    <arg type="s" direction="out" name="result" /> \
</method> \
</interface> \
</node> \
';

/**
 * Desctructively apply lines[i].replace(regex, replacement)
 */
function replaceAll(lines, regex, replacement) {
    lines.forEach((line, i) => {
        lines[i] = lines[i].replace(regex, replacement);
    })
}

let DbusObject = {
    Eval: function (code, path) {
        try {
            // (We try in case the module has syntax errors)
            emacs.module = this.findModule(path);
        } catch(e) {
            emacs.module = {};
            print(`Couldn't load module, will evaluate without: ${e.message}`)
        }

        if (Object.keys(emacs.module).length > 0) {
            // We're in a module and we can replace `var` with
            // `emacs.module.` so that re-assignment works
            let lines = code.split('\n');

            replaceAll(lines, /^var /g, 'emacs.module.');
            // rewrite function syntax assignment
            replaceAll(lines, /^function\s+(.*)\(/g,
                              'emacs.module.$1 = function(');
            replaceAll(lines, /^const /g, 'emacs.module.');
            replaceAll(lines, /^let /g, 'emacs.module.');

            code = lines.join('\n')
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
                result.value = "Error during pretty printing: " + e.message;
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
    },

    /**
     * Reload the the path by disabling the extension, re-evaluate the code in
     * the path and enable the extension again.
     */
    Reload: function(path) {
        // Make sure that we're in an ext
        let [type, root] = findExtensionRoot(path);

        let uuid;
        if (type === 'extension') {
            let metadataFile = `${root}/metadata.json`;
            if (GLib.file_test(metadataFile, GLib.FileTest.IS_REGULAR)) {
                const [success, metadata] = GLib.file_get_contents(metadataFile);
                uuid = JSON.parse(metadata.toString()).uuid;
                if (uuid === undefined) {
                    return [false, 'Extension is missing the uuid'];
                }
            }
        } else {
            return [false, 'Not a valid extension'];
        }

        let extension = imports.misc.extensionUtils.extensions[uuid];
        let modules = extension.imports.extension.modules;
        // Disable the extension
        extension.imports.extension.disable();

        // Reload the modules
        const [success, code] = GLib.file_get_contents(path);
        const [evalSuccess, result] = this.Eval(code.toString(), path);
        // Enable the extension again
        extension.imports.extension.enable();
        return [evalSuccess, result];
    },

    findModule: function(moduleFilePath) {
        let [type, projectRoot] = findExtensionRoot(moduleFilePath);
        let empty = {};
        if (projectRoot === null || type === null) {
            return empty;
        }

        // (projectRoot does not end with slash)
        let relPath = moduleFilePath.slice(projectRoot.length+1);

        let uuid;
        if (type === 'extension') {
            let metadataFile = `${projectRoot}/metadata.json`;
            if (GLib.file_test(metadataFile, GLib.FileTest.IS_REGULAR)) {
                const [success, metadata] = GLib.file_get_contents(metadataFile);
                uuid = JSON.parse(metadata.toString()).uuid;
                if (uuid === undefined) {
                    return empty;
                }
            }
        }

        // Find the module object we're in
        if (relPath.endsWith('.js')) {
            relPath = relPath.substring(0, relPath.length - 3);
            let moduleImports;
            if (type === 'extension') {
                moduleImports =
                    imports.misc.extensionUtils.extensions[uuid].imports;
            } else if (type === 'shell') {
                moduleImports = imports;
            }
            return relPath.split('/').reduce((module, name) => {
                if (module[name]) {
                    return module[name];
                }
                return empty;
            },  moduleImports);
        } else {
            return null;
        }
    }
};

const JsParse = imports.misc.jsParse;

let _getAutoCompleteGlobalKeywords = () => {
    const keywords = ['true', 'false', 'null', 'new'];
    // Don't add the private properties of window (i.e., ones starting with '_')
    const windowProperties = Object.getOwnPropertyNames(window).filter(function(a){ return a.charAt(0) != '_' });

    return keywords.concat(windowProperties);
}

function completion_candidates(text) {
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
