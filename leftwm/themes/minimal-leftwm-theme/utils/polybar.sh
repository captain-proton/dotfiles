#!/usr/bin/env bash
export SCRIPTPATH
SCRIPTPATH="$( cd "$(dirname "$0")/.." || exit; pwd -P )"

# [ STATUSBAR ]
pkill polybar

index=0
monitors=($(polybar -m | sed s/:.*//))
leftwm-state -q -n -t "$SCRIPTPATH"/sizes.liquid | sed -r '/^\s*$/d' | while read -r width x y
do
  barname="mainbar$index"
  monitor=${monitors[index]} width=$(( width - 16 )) polybar -c "$SCRIPTPATH"/polybar.config $barname &> /tmp/polybar.log &
  let index=index+1
done
