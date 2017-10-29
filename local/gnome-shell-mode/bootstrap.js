(function (path) {
    const Gio = imports.gi.Gio;
    const extensionUtils = imports.misc.extensionUtils;
    const extensionSystem = imports.ui.extensionSystem;
    const ExtensionFinder = new extensionUtils.ExtensionFinder();

    const uuid = "gnome-shell-mode@hedning:matrix.org";
    let dir = Gio.File.new_for_path(`${path}${uuid}`);
    let extension = extensionUtils.createExtensionObject(uuid, dir, 1);

    extensionSystem.loadExtension(extension);
    extensionSystem.enableExtension(uuid);
})
