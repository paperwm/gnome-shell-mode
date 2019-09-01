(function (path) {
    const Gio = imports.gi.Gio;
    let extensionUtils;
    let extensionSystem;
    // Work around differences between 3.32 and 3.34
    if (imports.misc.extensionUtils.createExtensionObject) {
        extensionSystem = imports.ui.extensionSystem;
        extensionUtils = imports.misc.extensionUtils;
    } else {
        extensionSystem = imports.ui.main.extensionManager;
        extensionUtils = extensionSystem;
    }

    const uuid = "gnome-shell-mode@hedning:matrix.org";
    let dir = Gio.File.new_for_path(`${path}${uuid}`);
    let extension = extensionUtils.createExtensionObject(uuid, dir, 1);

    extensionSystem.loadExtension(extension);
    extensionSystem.enableExtension(uuid);
})
