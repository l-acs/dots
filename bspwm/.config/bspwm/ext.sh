#!/bin/sh

# floating desktops
current="$(bspc query -d focused -D)"
floating_desktop="$(bspc query -d '^10' -D)"
[ "$current" = "$floating_desktop" ] && echo 'state=floating' && exit

monitor_count=$( bspc query -M | wc -l)
second_monitor_occupied=$(bspc query --monitor '^2.occupied' -D && echo true)

wid=$1
class=$2
instance=$3
consequences=$4



if [ "$instance" = zoom ] ; then
    title=$(xtitle "$wid" | tr '[:upper:]' '[:lower:]')
    case "$title" in
	'zoom'|'leave meeting')
	    echo "state=floating" #"focus=off"
	    ;;
	'zoom meeting')
	    echo 'state=tiled'
	    ;;
    esac
fi
