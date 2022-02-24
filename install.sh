#!/bin/sh

case $1 in
   work|home)
     echo "Setting up dotfiles for $1"
     sudo pacman -S --needed ansible python
     ansible-playbook -i localhost, -c local --ask-become-pass setup/$1.yml ;;
   *)
     echo "Enter $0 work|home" ;;
esac