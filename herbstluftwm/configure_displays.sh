#!/bin/bash

# This script looks for two files inside the
# $HOME/.screenlayout directory. One must be named
# 'xrandr_<name>' containing the output of the xrandr
# command. The other one must be named '<name>.sh'
# and should contain the commands to set the herbstluftwm
# monitor settings including the xrandr settings.

XRANDR_VAL=`xrandr`
LAYOUT_PATH=$HOME/.screenlayout

find_xrandr_setting() {
    MON_ENV=""
    for file in $LAYOUT_PATH/*; 
    do
        if [ "$MON_ENV" != "" ]; then
            break
        fi
        
        FILENAME=$(basename -- "$file")
        FILENAME="${FILENAME%.*}"

        DIFF=`diff -u $file <(echo "$XRANDR_VAL")`

        # look for all files starting with xrandr
        # found the target environment if the diff
        # is empty
        if [[ $FILENAME = xrandr* ]] && [[ "$DIFF" == "" ]]; then
            MON_ENV=$(echo $FILENAME | cut -d '_' -f 2)
        fi

    done
}

set_monitors() {
    MON_SCRIPT="$LAYOUT_PATH/$1.sh"
    if [ -f $MON_SCRIPT ];
    then
        echo "Setting monitors using $MON_SCRIPT"
        bash $MON_SCRIPT
    fi
}

find_xrandr_setting
if [ "$MON_ENV" != "" ];
then
    set_monitors $MON_ENV
fi



# for file in $LAYOUT_PATH/*; do

#    FILENAME=$(basename -- "$file")
#    FILENAME="${FILENAME%.*}"

#    DIFF=`diff -u $file <(echo "$XRANDR_VAL")`

#    # look for all files starting with xrandr
#    # if the diff 
#    if [[ $FILENAME = xrandr* ]] && [[ "$DIFF" == "" ]]; then
       
#        MON_ENV=$(echo $FILENAME | cut -d '_' -f 2)
#        MON_SCRIPT="$LAYOUT_PATH/$MON_ENV.sh"

#        if [ -f $MON_SCRIPT ]; then
#            echo "Setting monitors using $MON_SCRIPT"
#            bash $MON_SCRIPT
#        fi
#    fi

# done
