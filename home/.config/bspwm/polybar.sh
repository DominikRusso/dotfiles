#!/bin/sh

killall -q polybar

polybar main -c ~/.config/polybar/config.ini &

