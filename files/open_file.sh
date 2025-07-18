#!/bin/bash
SEARCH_PATH="$1"
cd "$SEARCH_PATH" || exit
TARGET_DIR=$(pwd)

# Search files, present them via tofi, and open the selected one
selected_file=$(fd . -I --type f "$TARGET_DIR" | sed "s|^$TARGET_DIR/||g" | tofi --ascii-input=true | tr -d '\r')

if [[ -z "$selected_file" ]]; then
    echo "File $selected_file is empty"
    exit 1
fi

if [[ ! -f "$selected_file" ]]; then
    echo "$selected_file does not exist"
    exit 1
fi

xdg-open "$selected_file"
