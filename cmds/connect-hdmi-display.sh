#!/bin/bash

xrandr --output HDMI-2 --auto
if [ "$1" = "left" ]
then
    xrandr --output HDMI-2 --left-of eDP-1
elif [ "$1" = "right" ]
then
    xrandr --output HDMI-2 --right-of eDP-1
elif [ "$1" = "disconnect" ]
then
    xrandr --output HDMI-2 --off
else
    echo "Unkown argument"
    echo "Usage: connect-hdmi-display [left | right | disconnect]"
fi
