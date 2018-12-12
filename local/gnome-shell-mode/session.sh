#!/usr/bin/env bash

old_display=$DISPLAY

d=0
while [ -e /tmp/.X11-unix/X${d} ]; do
    d=$((d + 1))
done

NEW_DISPLAY=:$d

export XDG_CONFIG_HOME=${XDG_CACHE_HOME:-$HOME/.cache}/paperwm/.config
mkdir -p $XDG_CONFIG_HOME

args=()

DISPLAY=$NEW_DISPLAY
eval $(dbus-launch --exit-with-session --sh-syntax)
echo $DBUS_SESSION_BUS_ADDRESS

DISPLAY=$old_display
case "$1" in
    w*|-w*|--w*)
        echo 'Running Wayland Gnome Shell'
        args=(--nested --wayland)
        ;;
    *)
        echo 'Running X11 Gnome Shell'
        Xephyr $NEW_DISPLAY &
        DISPLAY=$NEW_DISPLAY
        args=--x11
        ;;
esac


dconf reset -f /  # Reset settings
dconf write /org/gnome/shell/enabled-extensions "['paperwm@hedning:matrix.org']"

gnome-shell $args 2>&1 | sed 's/\x1b\[[0-9;]*m//g'

