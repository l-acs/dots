#keybinds
sxhkd &

#~/.config/feh/.fehbg
wal -i ~/Documents/Pictures/Wall/red-lake-mountains-minimal.jpg  -l -a 60 -b "fdf6e3"
#wal -i /home/l-acs/Documents/Pictures/Wall/joshua-comet.jpg

#no bell
xset -b

#compositor
picom -b &

#bar
while sleep 1; do $HOME/Projects/code/lojbar/lojbar; done | lemonbar -f "Ubuntu Mono" -a 50 | bash &

#usb daemon & tray icon
udiskie -s &

#right super
xmodmap -e "keycode 135 = Super_R"

#notifications daemon
dunst &


# start mpd if not already running
[ ! -s ~/.config/mpd/pid ] && mpd &

#unmute?
~/.scripts/unmute.sh

#layouts etc
export GTK_IM_MODULE=uim
export QT_IM_MODULE=uim
uim-wim &
export XMODIFIERS=@im=uim


#use space as control
#s2cctl start

exec bspwm
#exec cwm -c ~/.config/cwm/cwmrc
#exec dwm

#autostart
signal-desktop &

