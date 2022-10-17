#!/usr/bin/env zsh

# change-volume

# Arbitrary but unique message tag
msgTag="loudiness"

# Change the volume using alsa(might differ if you use pulseaudio)
pulseaudio-ctl "$@" > /dev/null

# Query pulseaudio for the current volume and whether or not the speaker is muted
volume="$(pulseaudio-ctl full-status | awk '{print $1}')"
mute="$(pulseaudio-ctl full-status | awk '{print $2}')"
if [ "$volume" -eq 0 ] || [ "$mute" = "yes" ]; then
    # Show the sound muted notification
    dunstify -a "change-volume" -u low -i audio-volume-muted \
        -h string:x-dunst-stack-tag:$msgTag "Volume muted"
else
    # Show the volume notification
    dunstify -a "change-volume" -u low -i audio-volume-high \
        -h string:x-dunst-stack-tag:$msgTag \
        -h int:value:"$volume" "Volume: ${volume}%"
fi
