setxkbmap -layout de

sxhkd &
unclutter &
xcompmgr &
xwallpaper --zoom ~/.config/wallpaper.png &

if [ $WM = dwm ]; then
    dwmstatus &
fi;

exec $WM

