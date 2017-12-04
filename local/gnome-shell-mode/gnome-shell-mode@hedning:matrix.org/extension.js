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

/// Add custom printers here indexed by constructor name
// (obj, key) => String or primitive to be serialized further to JSON
// `key` is undefined if top-level object
emacs.pretty_printers = {};

emacs.pp_rect = function(xywh) {
    let [x,y,w,h] = xywh.map( v => v.toFixed(1) );
    return `{ x: ${x}, y: ${y}, w: ${w}, h: ${h} }`;
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
    let x = box.get_x();
    let y = box.get_y();
    let w = box.get_width();
    let h = box.get_height();
    return `ActorBox ${emacs.pp_rect([x,y,w,h])}`;
}

emacs.pretty_printers["Meta_Rectangle"] = function(box, key) {
    return `Meta_Rectangle ${emacs.pp_rect([box.x, box.y, box.width, box.height])}`;
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

emacs.completion_candidates = emacs.completion_candidates;

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
