/usr/bin/pipewire &
/usr/bin/pipewire-pulse &
/usr/bin/pipewire-media-session &
slstatus &
udiskie &
dunst &
kwalletd5 &
clipmenud &
dropbox &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

bash ~/.screenlayout/.screenlayout.sh

# Resolution and scaling

# LG Monitor via USB C hub
# xrandr --output "DP-4"
# Blinking cursor thing
xrandr --output DP-1 --scale 0.9999x0.9999


# Built in display for Framework
# modesetting
xrandr --output "eDP-1" --scale 0.7x0.7

# allow for double/triple finger click on Framework trackpad
xinput set-prop "PIXA3854:00 093A:0274 Touchpad" "libinput Click Method Enabled" 0 1
xbindkeys

feh --bg-scale ~/.config/backgrounds/wall.jpg

# swap control w/ caps and window w/ alt
setxkbmap -device 13 -option "ctrl:swapcaps" -option "altwin:swap_lalt_lwin"
# key repeat rate [delay] [rate]
xset r rate 200 25

# Only when not using Happy Hacker
# setxkbmap -option ctrl:swapcaps
picom -b

while :; do
	ssh-agent dwm
done
