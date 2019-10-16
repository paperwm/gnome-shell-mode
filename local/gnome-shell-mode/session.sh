#!/usr/bin/env bash

OLD_DISPLAY=$DISPLAY

TYPE=${1:-$XDG_SESSION_TYPE}; shift
# TYPE=x11
TYPE=wayland
ROOT=$1; shift
UUID=$1; shift

d=0
while [ -e /tmp/.X11-unix/X${d} ]; do
    d=$((d + 1))
done
NEW_DISPLAY=:$d

XDG_RUNTIME_DIR=$(mktemp -d)

CACHE=${XDG_CACHE_HOME:-$HOME/.cache}/${UUID}${SUFFIX}
mkdir -p $CACHE
export XDG_CONFIG_HOME=${CACHE}/config
export XDG_DATA_HOME=${CACHE}/local
mkdir -p $XDG_DATA_HOME/gnome-shell/extensions
ln -fsn $ROOT $XDG_DATA_HOME/gnome-shell/extensions/${UUID}
export XDG_CACHE_HOME=${CACHE}/cache

DISPLAY=$NEW_DISPLAY
eval $(dbus-launch --exit-with-session --sh-syntax)
echo $DBUS_SESSION_BUS_ADDRESS

DISPLAY=$OLD_DISPLAY
args=()
case "$TYPE" in
    wayland)
        args=(--nested --wayland)
        ;;
    x11)
        Xephyr $NEW_DISPLAY &
        DISPLAY=$NEW_DISPLAY
        args=--x11
        ;;
esac

# dconf reset -f /  # Reset settings
dconf write /org/gnome/shell/enabled-extensions "['${UUID}']"

export CLUTTER_SHOW_FPS=1
export SHELL_DEBUG=all
export MUTTER_DEBUG=1
export MUTTER_DEBUG_NUM_DUMMY_MONITORS=2
export MUTTER_DEBUG_DUMMY_MONITOR_SCALES=1
export MUTTER_DEBUG_TILED_DUMMY_MONITORS=1
unset NIX_GSETTINGS_OVERRIDES_DIR
# unset XDG_DATA_DIRS
# unset GIO_EXTRA_MODULES
# unset GI_TYPELIB_PATH
# unset GST_PLUGIN_SYSTEM_PATH_1_0
# unset GST_PLUGIN_SYSTEM_PATH
# export LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver-32/lib
# export G_MESSAGES_DEBUG=all
# /home/hed/paperwm/gnome-shell-3.28/bin/gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
# /nix/store/9mydygvs8vk7cyncnpfyfp2iz8j5sn8z-gnome-shell-3.33.91/bin/gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
# /nix/store/pichwkv9ipq6mnp0jra6wpzgag23w46v-gnome-shell-3.32.2/bin/gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
# export GI_TYPELIB_PATH=$(nix eval --raw nixpkgs.vte)/lib/girepository-1.0:$GI_TYPELIB_PATH
# gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
# ~/paperwm/gnome-shell-3.28/bin/gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
# ~/paperwm/gnome-shell-3.32/bin/gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
gnome-shell ${args[*]} 2>&1 | sed 's/\x1b\[[0-9;]*m//g'
