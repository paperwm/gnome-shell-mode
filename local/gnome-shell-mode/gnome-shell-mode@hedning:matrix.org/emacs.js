// -*- mode: gnome-shell; -*-

const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;
const GObject = imports.gi.GObject;
const JsParse = imports.misc.jsParse;

let verbose = false;

function verboseLog() {
    if (verbose) {
        log("gnome-shell-mode", ...arguments);
    }
}

/// Add custom printers here indexed by constructor name
// (obj, key) => String or primitive to be serialized further to JSON
// `key` is undefined if top-level object
let prettyPrinters = {};

function ppRect(xywh) {
    let [x,y,w,h] = xywh.map( v => v.toFixed(1) );
    return `{ x: ${x}, y: ${y}, w: ${w}, h: ${h} }`;
}

prettyPrinters["Object"] = function(obj, key) {
    if (obj.toString !== Object.prototype.toString) {
        // The object have a custom toString method
        return obj.toString();
    } else {
        // Let JSON handle it
        return  obj;
    }
}

prettyPrinters["Array"] = function(obj, key) {
    // Let JSON handle it
    return obj;
}


prettyPrinters["Clutter_ActorBox"] = function(box, key) {
    let x = box.get_x();
    let y = box.get_y();
    let w = box.get_width();
    let h = box.get_height();
    return `ActorBox ${ppRect([x,y,w,h])}`;
}

prettyPrinters["Meta_Rectangle"] = function(box, key) {
    return `Meta_Rectangle ${ppRect([box.x, box.y, box.width, box.height])}`;

}

function ppObject(obj, key) {
    let customPPFn;
    if (hasConstuctor(obj)) {
        customPPFn = prettyPrinters[obj.constructor.name];
    }
    if (customPPFn) {
        return customPPFn(obj, key);
    } else {
        return obj.toString();
    }
}


function ppHelper(root) {
    let seen = new Map();
    seen.set(root, "<Self>")
    function cycleDetectingPP(key, obj) {

        if(key === "") {
            // obj is the "root object":
            //   JSON.stringify(X, helper)
            //   When key is "", obj is X
            // Always recurse in this case.
            return obj
        }

        let type = typeof(obj);
        let pretty;
        if (type === "undefined") {
            pretty = "undefined";
        } else if (obj === null) {
            pretty = "null";
        } else if (type === "object") {
            let prettyMaybe = ppObject(obj);

            if(typeof(prettyMaybe) === 'object') {
                // We allow pretty printers to return a object instead of a
                // string to pretty print recursively.
                // Normally its only the plain object printer that rely in this.
                if (seen.get(obj)) {
                    pretty = seen.get(obj);
                } else {
                    seen.set(prettyMaybe, prettyMaybe.toString());
                    // In the rare case prettyMaybe !== obj :
                    seen.set(obj, prettyMaybe.toString());

                    // Recursively pretty print.
                    // Use a separate stringify call (as opposed to simply
                    // returning the object) so we can cache the result and use
                    // it if we see the object again.
                    //
                    // Note that we can't return the _string_ from the recursive
                    // stringify call as the parent stringify would escape it.
                    // By convert back to a plain object we get the wanted
                    // effect!
                    let prettyTree =
                        eval(`(${JSON.stringify(prettyMaybe, cycleDetectingPP)})`);
                    seen.set(obj, prettyTree)

                    pretty = prettyTree;
                }
            } else {
                pretty = prettyMaybe;
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

    return cycleDetectingPP;
}

function hasConstuctor(obj, exactConstructor) {
    // gjs "imports objects" fails when evaluation .constructor so we need this
    // special check
    try {
        if(!exactConstructor) {
            // Check if there is _any_ constructor at all
            return !!obj.constructor
        } else {
            return exactConstructor === obj.constructor;
        }
    } catch(e) {
        return false;
    }
}

function prettyPrint(obj) {
    if (obj !== null && typeof(obj) === "object"
        && (hasConstuctor(obj, Object) || hasConstuctor(obj, Array)))
    {
        // Use JSON.stringify as a poor man's pretty printer for simple
        // composite objects
        return JSON.stringify(obj, ppHelper(obj));
    } else if(typeof(obj) === "string") {
        // A pretty string have quotes around it to not conceal it's true nature
        return `"${obj.replace(/"/g, '\\"')}"`;
    } else {
        // Top level simple or complex constructor
        let pretty = ppHelper(obj)(undefined, obj);
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

/**
 * Desctructively apply lines[i].replace(regex, replacement)
 */
function replaceAll(lines, regex, replacement) {
    lines.forEach((line, i) => {
        lines[i] = lines[i].replace(regex, replacement);
    })
}

/**
 * Go through the ast, building replacements for top level declarations.
 * For example `let foo = bar` gets translated to `<{prefix}>foo=bar`.
 */
function parseAndReplace(code, prefix) {
    // Let line 0 be the start so the line numbers are aligned with lines indexing
    let ast = Reflect.parse(code, {line: 0});
    let lines = code.split('\n');
    let statements = [];
    let sourceMap = [];
    let linebreaks = 0;
    // Loop over all toplevel statements
    let length = ast.body.length;
    for (let i = 0; i < length; i++) {
        let statement = ast.body[i];
        let newStatement;
        switch (statement.type) {
        case 'VariableDeclaration':
            newStatement = variableDeclaration(lines, statement, prefix);
            break;
        case  'ClassStatement':
        case 'FunctionDeclaration':
            newStatement = prefix + statement.id.name + ' = ';
            newStatement += getStatement(lines, statement);
            break;
        default:
            newStatement = getStatement(lines, statement);
        }
        // Always add a semicolon to the built statement for safety
        newStatement += ';\n';

        sourceMap.push(
            {source: statement.loc.start.line,
             sink: statements.length + linebreaks});
        linebreaks += Math.max(0, newStatement.split('\n').length - 1);
        statements.push(newStatement);
    }
    return [statements.join('\n'), sourceMap];
}

// Gnome Shell 3.30 removed global.screen, so we're using it here as a way to
// check if we're dealing with spdiermonkey 60 or not.
var getStatement = global.screen ?
    getStatementMoz52 :
    getStatementMoz60;

// Spidermonkey 60 fixes most of the weird loc problems
function getStatementMoz60(lines, statement) {
    switch (statement.type) {

    case 'FunctionExpression':
    case 'FunctionDeclaration':
        var g = statement.generator ? '*' : '';
        var a = statement.async ? 'async ' : '';
        return `${a}function${g} ${span(lines, statement.loc)}`;

    case  'ArrowFunctionExpression':
        // statement.loc produces things like `= () => {}`
        var params = statement.params
            .map(p => getStatement(lines, p)).toString();
        var body = getStatement(lines, statement.body);
        return `(${params}) => ${body}`;

    default:
        return span(lines, statement.loc);
    }
}

/**
   Workarounds for the somewhat weird locs Reflect.parse returns in spidermonkey
   52, mostly due to block statements not including their closing bracket.
*/
function getStatementMoz52(lines, statement) {
    switch (statement.type) {
    case  'BlockStatement':
        // Block AST nodes omits the ending '}'
        var block = `${span(lines, statement.loc)} }`;
        if (statement.body[-1].type === 'BlockStatement') {
            // Omission of ending '}' "nests" (but only one level..)
            return block + " }"
        }
        return block;

    case 'IfStatement':
        var test = getStatement(lines, statement.test);
        var consquence = getStatement(lines, statement.consequent);
        var alternate = statement.alternate !== null ?
            `else ${getStatement(lines, statement.alternate)}` : "";
        return `if (${test}) ${consquence} ${alternate}`;

    case 'WithStatement':
        var object = getStatement(lines, statement.object);
        var body = getStatement(lines, statement.body);
        return `with (${object}) ${body}`;

    case 'WhileStatement':
        var test = getStatement(lines, statement.test);
        var body = getStatement(lines, statement.body);
        return `while (${test}) ${body}`;

    case 'ForStatement':
        var test = statement.test !== null ?
            getStatement(lines, statement.test) : "";
        var init = statement.init ?
            getStatement(lines, statement.init) : "";
        var update = statement.update ?
            getStatement(lines, statement.update) : "";
        var body = getStatement(lines, statement.body);
        return `for (${init}; ${test}; ${update}) ${body}`;

    case 'ForInStatement':
        var left = getStatement(lines, statement.left);
        var right = getStatement(lines, statement.right);
        var body = getStatement(lines, statement.body);
        return `for (${left} in ${right}) ${body}`;

    case 'ForOfStatement':
        var left = getStatement(lines, statement.left);
        var right = getStatement(lines, statement.right);
        var body = getStatement(lines, statement.body);
        return `for (${left} of ${right}) ${body}`;

    case 'FunctionExpression':
    case 'FunctionDeclaration':
        var g = (statement.generator && !statement.async) ? '*' : '';
        var a = statement.async ? 'async ' : '';
        return `${a}function${g} ${span(lines, statement.loc)}`;

    case  'ClassExpression':
    case 'ClassStatement':
        return `class ${span(lines, statement.loc)} }`;

    case  'ArrowFunctionExpression':
        // statement.loc produces things like `= () => {}`
        var params = statement.params
            .map(p => getStatement(lines, p)).toString();
        var body = getStatement(lines, statement.body);
        return `(${params}) => ${body}`;

    case 'UpdateExpression':
        // ++/-- isn't included in statement.loc
        var argument = getStatement(lines, statement.argument);
        if (statement.prefix)
            return `${statement.operator}${argument}`;
        else
            return `${argument}${statement.operator}`;

    default:
        return span(lines, statement.loc);
    }
}

/**
 * Retrieve a span from lines using loc.start: {line: .., column: ..} syntax
 */
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

function variableDeclaration (lines, statement, prefix) {
    let replacement = '';
    for (let declaration of statement.declarations) {
        replacement += '(';
        replacement += pattern(lines, declaration.id, prefix);

        let init = declaration.init;
        if (init) {
            replacement += `= ${getStatement(lines, init)}`;
        } else {
            // Handle cases like 'let foo'
            replacement += `=  ${prefix}${declaration.id.name}`;
        }
        replacement += ')';
        replacement += ',';
    }
    replacement = replacement.replace(/,$/, '');
    return replacement;
}

// Rebuild a pattern, prefixing when appropriate.
function pattern(lines, ptrn, prefix) {
    if (!ptrn)
        return '';
    switch (ptrn.type) {
    case 'Identifier':
        return prefix + ptrn.name;
    case 'ArrayPattern':
        return arrayPattern(lines, ptrn, prefix);
    case 'ObjectPattern':
        return objectPattern(lines, ptrn, prefix);
    case 'AssignmentExpression':
        return pattern(lines, ptrn.left, prefix) + ptrn.operator
            + pattern(lines, ptrn.right, prefix);
    default:
        return span(lines, ptrn.loc);
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

function mapLine(sourceMap, line) {
    let i = sourceMap.length-1;
    while (i > 0 && sourceMap[i].sink > line) {
        i--;
    }
    return sourceMap[i].source + (line - sourceMap[i].sink);
}

let fileScopes = {};
function findScope(path) {
    let scope;
    try {
        // (We try in case the module has syntax errors)
        scope = findModule(path);
    } catch(e) {
        scope = null;
        print(`Couldn't load module, fall back to default scope: ${e.message}`)
    }

    // Create a new scope, indexed by the path
    if (scope === null) {
        if (!fileScopes[path])
            fileScopes[path] = {};
        scope = fileScopes[path];
    }
    return scope;
}

function Eval(code, path) {
    emacs.module = findScope(path);

    let sourceMap;
    let evalResult;
    let result;
    let success = true;
    try {
        try {
            [code, sourceMap] = parseAndReplace(code, 'emacs.module.');
        } catch(e) {
            // Let eval take care of syntax errors too
        }
        evalResult =  (0, eval)(`with(emacs.module){ ${code} }`);
        result = {
            success: true,
        };

        try {
            result.value = prettyPrint(evalResult)

        } catch(e) {
            result.value = "Error during pretty printing: " + e.message;
        }

    } catch(e) {
        // Note: JSON.stringify(e) doesn't reliably include all fields
        if (sourceMap) {
            // lineNumber is one indexed, and sourceMap expect zero indexing
            // it also returns a zero indexed line, so we need to add 1.
            e.lineNumber = mapLine(sourceMap, e.lineNumber - 1) + 1;
        }
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

/**
 * Reload the the path by disabling the extension, re-evaluate the code in
 * the path and enable the extension again.
 */
function Reload(code, path) {
    // Make sure that we're in an ext
    let [type, extensionImports, root] = findExtensionImports(path);

    if (type !== 'extension') {
        return [false, 'Not in a valid extension'];
    }

    // Disable the extension
    extensionImports.extension.disable();

    // Reload the code
    const [evalSuccess, result] = Eval(code, path);
    // Enable the extension again
    extensionImports.extension.enable();
    return [evalSuccess, result];
}

/**
   Run extension.disable and then restart Gnome Shell
 */
function Restart(path) {
    let [type, extensionImports, _] = findExtensionImports(path);
    if (type !== 'extension')
        return;
    extensionImports.extension.disable();
    imports.gi.Meta.restart(
        `Restarting (disabled ${extensionImports.__moduleName__} first)`);
}

function findModule(path) {
    let [type, extensionImports, projectRoot] = findExtensionImports(path);

    // (projectRoot does not end with slash)
    let relPath = path.slice(projectRoot.length+1);

    // Find the module object we're in
    if (relPath.endsWith('.js')) {
        relPath = relPath.substring(0, relPath.length - 3);
        return relPath.split('/').reduce((module, name) => {
            if (module[name]) {
                return module[name];
            }
            return empty;
        },  extensionImports);
    } else {
        return null;
    }
}

function findExtensionImports(path) {
    let [type, projectRoot] = findExtensionRoot(path);
    if (projectRoot === null || type === null) {
        return [null, null, null];
    }

    if (type === 'extension') {
        let extension = findExtension(projectRoot);
        if (extension) {
            return [type, extension.imports, projectRoot];
        }
    } else if (type === 'shell') {
        return [type, imports, projectRoot];
    }
}

function findExtension(projectRoot) {
    let metadataFile = `${projectRoot}/metadata.json`;
    if (GLib.file_test(metadataFile, GLib.FileTest.IS_REGULAR)) {
        const [success, metadata] = GLib.file_get_contents(metadataFile);
        let uuid = JSON.parse(metadata.toString()).uuid;
        if (uuid === undefined)
            return false;
        return imports.misc.extensionUtils.extensions[uuid];
    }
}

function getGlobalCompletionsAndKeywords() {
    const keywords = ['true', 'false', 'null', 'new', 'typeof', 'function',
                      'throw', 'catch', 'try', 'const', 'let', 'var'];
    const windowProperties = Object.getOwnPropertyNames(window);

    return keywords.concat(windowProperties);
}

/**
 * Find the suffix of text that makes most sense as completion context
 * Eg. "foo(bar.b" -> "bar.b"
 */
function findExpressionToComplete(text) {
    const begin = JsParse.getExpressionOffset(text, text.length-1);
    if (begin < 0) {
        return null;
    }

    return text.slice(begin)
}

/**
 * "foo[0].bar.ba" -> ["foo[0].bar", "ba"]
 * "fooo" -> [null, "fooo"]
 */
function splitIntoBaseAndHead(expr) {
    let base = null, attrHead = null;
    // Look for expressions like "Main.panel.foo" and match Main.panel and foo
    let matches = expr.match(/(.+)\.(.*)/);
    if (matches) {
        [base, attrHead] = matches.slice(1); // (first item is whole match)
    } else {
        attrHead = expr;
    }

    return [base, attrHead]
}

function isGObject(object) {
    try {
        // NB: not a 100% sure this test is accurate
        return object && typeof(object) === 'object' && object.__metaclass__;
    } catch(e) {
        // GjsFileImporter (eg. `imports`) throw when accessing non-existent property
        return false;
    }
}

function completionCandidates(text, path) {
    let completions = [];

    let scope = findScope(path);

    let expr = findExpressionToComplete(text); // Note: In emacs atm. `text` will usually equal `expr`
    if (expr === null) {
        verboseLog("Trying to complete invalid expression", text)
        return [];
    }

    let [base, attrHead] = splitIntoBaseAndHead(expr);

    if (base === null) {
        // Complete scope variables, global variables and keywords
        completions = completions.concat(
            getGlobalCompletionsAndKeywords(),
            JsParse.getAllProps(scope)
        );
    } else {
        // Need to evaluate the owner of `attrHead`
        if (JsParse.isUnsafeExpression(base)) {
            verboseLog("Unsafe expr", base, attrHead);
            return [];
        }

        emacs.module = scope;
        let baseObj = null;
        try {
            baseObj = (0, eval)(`with(emacs.module) { ${base} }`);
        } catch(e) {
            // Some objects, eg. `imports`, throw when trying to access missing
            // properties. (Completing `imports.not_exist.foo` will throw)
            verboseLog("Failed to eval base", base);
            return [];
        }

        completions = completions.concat(JsParse.getAllProps(baseObj));

        if (isGObject(baseObj)) {
            // NB: emacs.list_properties.call(x) crashes gnome-shell when x is a
            //     (non-empty) string or number
            emacs.list_properties.call(baseObj)
                .forEach((x) => {
                    // list_properties gives names with "-" not "_"
                    completions.push(x.name.replace(/-/g, "_"))
                });
        }
    }

    return completions.filter((x) =>
                              typeof(x) === 'string' &&
                              x.startsWith(attrHead) &&
                              JsParse.isValidPropertyName(x));
};
