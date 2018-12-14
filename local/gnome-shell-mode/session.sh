#!/usr/bin/env bash

OLD_DISPLAY=$DISPLAY

d=0
while [ -e /tmp/.X11-unix/X${d} ]; do
    d=$((d + 1))
done
NEW_DISPLAY=:$d

export XDG_CONFIG_HOME=${XDG_CACHE_HOME:-$HOME/.cache}/paperwm/.config
mkdir -p $XDG_CONFIG_HOME

DISPLAY=$NEW_DISPLAY
eval $(dbus-launch --exit-with-session --sh-syntax)
echo $DBUS_SESSION_BUS_ADDRESS

TYPE=${1:-$XDG_SESSION_TYPE}
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
shift

dconf reset -f /  # Reset settings
dconf write /org/gnome/shell/enabled-extensions "['$1']"

gnome-shell $args 2>&1 | sed 's/\x1b\[[0-9;]*m//g'

