#!/bin/sh

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

case "$class" in
    "firefox")

	if [ $(pgrep -f 'firefox .*-childID.*' | wc -l) -gt 1 ]; then # there's already a firefox open
	    [ $monitor_count -ge 1 ] && [ ! $second_monitor_occupied ]  && echo 'monitor=^2 follow=on focus=on'
	else
	    echo 'desktop=^5' # this is the first firefox
	fi

	;;
esac
