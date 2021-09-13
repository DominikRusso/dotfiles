#!/bin/sh

killall -q polybar

export POLYBAR_OFFSET_Y=$( echo $( xrdb -q | grep Xft.dpi | cut -f 2 ) / -28 | bc )
polybar main -c ~/.config/polybar/config.ini &
