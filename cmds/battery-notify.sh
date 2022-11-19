#!/bin/bash
if [ -r "$HOME/.dbus/Xdbus" ]; then
    . "$HOME/.dbus/Xdbus"
fi
charging=$(acpi | grep -o 'Charging')

battery_sum=$(acpi -b | grep -E -o '[0-9]+%' | sed 's/%//' | paste -s -d + | bc)
battery_denom=$(echo "$(acpi -b | wc -l)" | bc)

battery=$(echo "${battery_sum}/${battery_denom}" | bc)
echo $battery
if [[ $battery -le 10 ]] && [[ -z $charging ]]
then
    notify-send -u critical "Battery Low!"
elif [[ $battery -le 5 ]] && [[ -z $charging ]]
then
    notify-send -u critical "Battery Critical! Suspending"
    systemctl suspend
fi
