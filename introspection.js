// Exploring introspection/debugging capabilities of gjs/gobject-introspection

GObject = imports.gi.GObject;
find_property = GObject.Object.find_property;
list_properties = GObject.Object.list_properties;

pp_properties = (obj) => {
    pped_props = list_properties.call(obj).map((x)=>{return x.name + ": " + x.blurb});
    return pped_props.join("\n");
}


pp_properties(global);

prop = find_property.call(global, "display");
