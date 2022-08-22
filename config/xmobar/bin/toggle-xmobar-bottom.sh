#!/bin/bash

pidof xmobar-bottom && killall xmobar-bottom

if pidof spotify>/dev/null; then
  xmobar-bottom $* spotify &
elif pidof 'Google Play Music Desktop Player'>/dev/null; then
  xmobar-bottom $* google-play-music-desktop-player &
else
  xmobar-bottom $* &
fi
