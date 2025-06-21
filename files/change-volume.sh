#!/usr/bin/env bash

# change-volume

# Arbitrary but unique message tag
msgTag="loudiness"

# Change the volume using wpctl
case "$1" in
    up) wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%+ ;;
    down) wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%- ;;
    mute) wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle ;;
    *) echo "Usage: $0 {up|down|mute}" && exit 1 ;;
esac

# Query wpctl for the current volume and whether or not the speaker is muted
volume=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print int($2 * 100)}')
mute=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $3}')
if [ "$volume" -eq 0 ] || [ "$mute" = "[MUTED]" ]; then
    # Show the sound muted notification
    dunstify -a "change-volume" -u low -i audio-volume-muted \
        -h string:x-dunst-stack-tag:$msgTag "Volume muted"
else
    # Show the volume notification
    dunstify -a "change-volume" -u low -i audio-volume-high \
        -h string:x-dunst-stack-tag:$msgTag \
        -h int:value:"$volume" "Volume: ${volume}%"
fi
