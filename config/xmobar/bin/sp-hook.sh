#!/bin/bash

p="/tmp/music"
fmt="{{xesam:trackNumber}} {{title}} <fc=sienna3>{{artist}}</fc> {{album}} <fc=grey50>{{duration(mpris:length)}}</fc>"

info=$(playerctl -p spotifyd -f "$fmt" metadata)

[[ -p $p ]] || mkfifo $p

echo -e "$info" > "/tmp/debug"
echo "$info" > $p
