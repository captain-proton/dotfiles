#!/bin/sh
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]
then
  echo "%{F#4C566A}  %{F-}"
else
  missing=$(echo info | bluetoothctl | grep "Missing device" | wc -c)
  if [ $missing -gt 0 ]; then
    echo "%{F#EBCB8B}  %{F-}"
  else
    DEVICE=$(echo info | bluetoothctl | grep 'Alias' | sed -E 's/Alias://' | xargs)
    echo "%{F#D8DEE9} 󰂱 %{F-} %{F#8FBCBB}$DEVICE%{F-}"
  fi
fi
