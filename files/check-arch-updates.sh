#!/usr/bin/env bash
icon="󱘊"
class="arch-updates"

if ! updates_arch=$(checkupdates 2> /dev/null | wc -l ); then
    updates_arch=0
fi

if [ "$updates_arch" -gt 0 ]; then
    echo "{\"text\": \"Arch: $updates_arch\", \"icon\":\"$icon\", \"class\":\"$class\"}"
else
    echo ""
fi
