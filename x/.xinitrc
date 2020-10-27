#keybinds
sxhkd &

bg="$PICTURE/wall/red-lake-mountains-minimal.jpg"
feh --no-fehbg --bg-scale "$bg"
wal -i "$bg"  -a 85 -b 111111 &

#no bell
xset -b &

#compositor
picom -b &

bar &

#usb daemon & tray icon
udiskie -s &

#swap super and ctrl
xmodmap ~/.config/xmodmap

#notifications daemon
dunst &

# start mpd and mpdas
(systemctl restart --user mpd &&
     systemctl restart --user mpdas) &
#(pgrep -x mpdas || mpdas -d -c ~/.config/mpdasrc)


# eyes!
redshift -c ~/.config/redshift.conf &

#unmute?
unmute.sh &

#layouts etc
export GTK_IM_MODULE=uim
export QT_IM_MODULE=uim
uim-wim &
export XMODIFIERS=@im=uim

exec bspwm
#exec cwm -c ~/.config/cwm/cwmrc
#exec dwm


