#!/usr/bin/env bash
trayer --edge top --align right --SetDockType true  --expand true --width 10 --height 12  --transparent true --tint 0x073642 --alpha 0 &

xrdb -merge ~/.dotfiles/lib/solarized/xresources-colors-solarized/Xresources
xrdb -merge ~/.Xdefaults
xsetroot -solid '#002b36'
xscreensaver -no-splash &

exec xmonad
