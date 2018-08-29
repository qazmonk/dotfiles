#!/bin/bash

xrandr --output HDMI2 --auto
if [ "$1" = "left" ]
then
    xrandr --output HDMI2 --left-of eDP1
elif [ "$1" = "right" ]
then
    xrandr --output HDMI2 --right-of eDP1
elif [ "$1" = "disconnect" ]
then
    xrandr --output HDMI2 --off
else
    echo "Unkown argument"
    echo "Usage: connect-hdmi-display [left | right | disconnect]"
fi
