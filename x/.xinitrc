#keybinds
sxhkd &

#~/.config/feh/.fehbg
wal -i ~/Documents/Pictures/Wall/red-lake-mountains-minimal.jpg  -l -a 60 -b "fdf6e3"
#wal -i /home/l-acs/Documents/Pictures/Wall/joshua-comet.jpg

#no bell
xset -b

#boomer solution: no bar tmux
tmux set -g status off

#compositor
picom -b &

#bar
$HOME/Projects/code/lojbar/lojbar | lemonbar -f "Ubuntu Mono" -a 50 | bash &

#usb daemon & tray icon
udiskie -s &

#right super
xmodmap ~/.config/xmodmap

#notifications daemon
dunst &

# start mpd and mpdas
systemctl restart --user mpd
(pgrep -x mpdas || mpdas -d -c ~/.config/mpdasrc)




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

