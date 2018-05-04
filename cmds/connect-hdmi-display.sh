#!/bin/bash

xrandr --output HDMI2 --auto
if [ "$1" = "left" ]
then
    xrandr --output HDMI2 --left-of eDP1
else
    xrandr --output HDMI2 --right-of eDP1
fi
