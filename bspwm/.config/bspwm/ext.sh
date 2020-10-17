#!/bin/sh

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

# case "$class" in
#     Lutris|Liferea)
# 	eval "$consequences"
# 	[ "$state" ] || echo "state=pseudo_tiled"
# 	;;
# esac
