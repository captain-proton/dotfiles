#!/usr/bin/env zsh
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(git
  asdf
  tmux
  systemd
  direnv
  poetry
  ansible
  emacs
  zsh-syntax-highlighting
  zsh-completions
  zsh-autosuggestions
)

# extra files in $HOME/.zsh/configs/pre , $HOME/.zsh/configs , and $HOME/.zsh/configs/post
# these are loaded first, second, and third, respectively.
_load_settings() {
  _dir="$1"
  if [ -d "$_dir" ]; then
    if [ -d "$_dir/pre" ]; then
      for config in "$_dir"/pre/**/*(N-.); do
        . $config
      done
    fi

    for config in "$_dir"/**/*(N-.); do
      case "$config" in
        "$_dir"/pre/*)
          :
          ;;
        "$_dir"/post/*)
          :
          ;;
        *)
          if [ -f $config ]; then
            . $config
          fi
          ;;
      esac
    done

    if [ -d "$_dir/post" ]; then
      for config in "$_dir"/post/**/*(N-.); do
        . $config
      done
    fi
  fi
}
_load_settings "$HOME/.zsh/configs"

# Add ssh private key to the agent
[[ -f $HOME/.ssh/id_rsa ]] && ssh-add -l |grep -q `ssh-keygen -lf ~/.ssh/id_rsa  | awk '{print $2}'` || ssh-add ~/.ssh/id_rsa

# Local config
[[ -f $HOME/.zshrc.local ]] && source $HOME/.zshrc.local

# To customize prompt, run `p10k configure` or edit $HOME/.p10k.zsh.
[[ ! -f $HOME/.p10k.zsh ]] || source $HOME/.p10k.zsh

# Create .path file for $PATH modifications
[[ ! -f $HOME/.path ]] && echo "#\!/usr/bin/env zsh" > $HOME/.path

source $HOME/.path
source $ZSH/oh-my-zsh.sh

# aliases
[[ -f $HOME/.aliases ]] && source $HOME/.aliases
