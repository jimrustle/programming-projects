#!/bin/bash

run() {
    if ! pgrep -f "$1" ;
    then
        "$@"&
    fi
}

run setxkbmap us -variant altgr-intl
run xmodmap "/home/marisa/.Xmodmap"
run xrdb -merge "/home/marisa/.Xresources"
run urxvtd -q -f -o
run nitrogen --restore
run synclient CircularScrolling=1
run synclient TapButton1=1
run synclient TapButton2=3
run xpad 
run xset m 3 2
run spotify
run steam
run thunderbird
run slack
run nm-applet
run discord

