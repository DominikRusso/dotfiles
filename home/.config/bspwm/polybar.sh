#!/bin/sh

# kill all running instances
killall -q polybar

# start main bar on every monitor
for monitor in $(xrandr --query | grep " connected" | cut -d " " -f 1); do
  MONITOR=$monitor polybar main -c ~/.config/polybar/config.ini &
done
