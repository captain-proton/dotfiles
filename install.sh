#!/bin/bash

case $1 in
  work|home)
    echo "Setting up dotfiles for $1"
    sudo pacman -S --needed ansible python

    # this needs to be done beforehand, so the yay module is available
    yay=`pacman -Q yay`
    if [[ $yay != yay* ]] && [[ ! -f playbooks/library/yay ]]
    then
      echo "Installing yay and necessary plugins"
      ansible-playbook --ask-become-pass playbooks/yay.yml
    fi

    echo "Executing setup"
    ansible-playbook --ask-become-pass setup/$1.yml ;;
  *)
    echo "Enter $0 work|home" ;;
esac