// -*- mode: gnome-shell; -*-

const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;
const GObject = imports.gi.GObject;

/// Add custom printers here indexed by constructor name
// (obj, key) => String or primitive to be serialized further to JSON
// `key` is undefined if top-level object
let pretty_printers = {};

function pp_rect(xywh) {
    let [x,y,w,h] = xywh.map( v => v.toFixed(1) );
    return `{ x: ${x}, y: ${y}, w: ${w}, h: ${h} }`;
}

pretty_printers["Object"] = function(obj, key) {
    if (obj.toString !== Object.prototype.toString) {
        // The object have a custom toString method
        return obj.toString();
    } else {
        // Let JSON handle it
        return  obj;
    }
}

pretty_printers["Array"] = function(obj, key) {
    // Let JSON handle it
    return obj;
}


pretty_printers["Clutter_ActorBox"] = function(box, key) {
    let x = box.get_x();
    let y = box.get_y();
    let w = box.get_width();
    let h = box.get_height();
    return `ActorBox ${pp_rect([x,y,w,h])}`;
}

pretty_printers["Meta_Rectangle"] = function(box, key) {
    return `Meta_Rectangle ${pp_rect([box.x, box.y, box.width, box.height])}`;
}

function pp_helper(key, obj) {
    let type = typeof(obj);
    let pretty;
    if (type === "undefined") {
        pretty = "undefined";
    } else if (obj === null) {
        pretty = "null";
    } else if (type === "object") {
        let constructor_name = obj.constructor.name;
        let custom_pp_fn = pretty_printers[constructor_name];
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

function pp_object(obj) {
    if (obj !== null && typeof(obj) === "object"
        && (obj.constructor === Object || obj.constructor === Array))
    {
        // Use JSON.stringify as a poor man's pretty printer for simple
        // composite objects
        return JSON.stringify(obj, pp_helper);
    } else if(typeof(obj) === "string") {
        // A pretty string have quotes around it to not conceal it's true nature
        return JSON.stringify(obj);
    } else {
        // Top level simple or complex constructor
        let pretty = pp_helper(undefined, obj);
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
    <arg type="s" direction="in" name="code" /> \
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

function parseAndReplace(code, prefix) {
    // Let line 0 be the start so the line numbers are aligned with lines indexing
    let ast = Reflect.parse(code, {line: 0});
    let lines = code.split('\n');
    let newLines = [];
    // Loop over all toplevel statements
    for (let statement of ast.body) {
        if (statement.type === 'VariableDeclaration') {
            let replacement = '';
            for (let declaration of statement.declarations) {
                if (declaration.id.type === 'ObjectPattern') {
                    replacement += '(';
                }
                replacement += pattern(lines, declaration.id, prefix);

                if (declaration.init) {
                    // init.loc.start is often bonkers so we need to work around that
                    // In addition id.loc.end is goes to the end of the
                    // statement if it's an Identfier
                    let start;
                    if (declaration.id.type === 'Identifier') {
                        start = Object.assign({}, declaration.loc.start);
                    } else {
                        start = Object.assign({}, declaration.id.loc.end);
                    }
                    // Look for the first `=` we can find starting at a position
                    // where we know the next `=` is the init equal sign
                    let line = lines[start.line];
                    while (line.indexOf('=', start.column) === -1) {
                        start.column = 0;
                        start.line = start.line + 1;
                        line = lines[start.line];
                    }
                    start.column = line.indexOf('=', start.column);

                    replacement += span(lines, {start, end: declaration.init.loc.end});
                } else {
                    // Handle cases like 'let foo'
                    replacement += '= undefined';
                }
                if (declaration.id.type === 'ObjectPattern') {
                    replacement += ')';
                }
                replacement += ',';
            }
            replacement = replacement.replace(/,$/, ';');
            newLines.push(replacement);
        } else if (statement.type === 'FunctionDeclaration') {
            let replacement = prefix + statement.id.name + ' = function ';
            replacement += '(';
            for (let param of statement.params) {
                replacement += span(lines, param.loc) + ',';
            }
            replacement = replacement.replace(/,$/, '');
            replacement += ')';
            // For some reason the body.loc.end doesn't include } (but start includes {)
            replacement += span(lines, {start: statement.body.loc.start,
                                        end: statement.loc.end});
            newLines.push(replacement);
        } else {
            newLines.push(span(lines, statement.loc) + ';');
        }
    }
    return newLines.join('\n');
}

function span(lines, loc) {
    let start = loc.start;
    let end = loc.end;
    let slice = lines.slice(start.line, end.line + 1);
    if (start.line === end.line) {
        slice[0] = slice[0].substring(start.column, end.column);
    } else {
        slice[0] = slice[0].substring(start.column);
        slice[slice.length-1] = slice[slice.length-1].substring(0, end.column);
    }
    return slice.join('\n');
}

function pattern(lines, pattern, prefix) {
    if (!pattern)
        return '';
    switch (pattern.type) {
    case 'Identifier':
        return prefix + pattern.name;
    case 'ArrayPattern':
        return arrayPattern(lines, pattern, prefix);
    case 'ObjectPattern':
        return objectPattern(lines, pattern, prefix);
    default:
        return span(lines, pattern.loc);
    }
}

function arrayPattern(lines, arraypattern, prefix) {
    let replacement = '[';
    for (let element of arraypattern.elements) {
        replacement += pattern(lines, element, prefix);
        replacement += ',';
    }
    replacement = replacement.replace(/,$/, ']');
    return replacement;
}

function objectPattern(lines, objpattern, prefix) {
    let replacement = '{';
    for (let property of objpattern.properties) {
        replacement += property.key.name + ':';
        replacement += pattern(lines, property.value, prefix);
        replacement += ',';
    }
    replacement = replacement.replace(/,$/, '}');
    return replacement;
}

let fileScopes = {};
let DbusObject = {
    Eval: function (code, path) {
        try {
            // (We try in case the module has syntax errors)
            emacs.module = this.findModule(path);
        } catch(e) {
            emacs.module = null;
            print(`Couldn't load module, fall back to default scope: ${e.message}`)
        }

        // Create a new scope, indexed by the path
        if (emacs.module === null) {
            if (!fileScopes[path])
                fileScopes[path] = {};
            emacs.module = fileScopes[path];
        }

        let eval_result;
        let result;
        let success = true;
        try {
            code = parseAndReplace(code, 'emacs.module.');
            eval_result =  (0, eval)(`with(emacs.module){ ${code} }`);
            result = {
                success: true,
            };

            try {
                result.value = pp_object(eval_result)

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
    Reload: function(code, path) {
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
        const [evalSuccess, result] = this.Eval(code, path);
        // Enable the extension again
        extension.imports.extension.enable();
        return [evalSuccess, result];
    },

    findModule: function(moduleFilePath) {
        let [type, projectRoot] = findExtensionRoot(moduleFilePath);
        let empty = {};
        if (projectRoot === null || type === null) {
            return null;
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
                    return null;
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
