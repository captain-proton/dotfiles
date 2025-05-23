#!/usr/bin/env zsh
export ZSH="$HOME/.oh-my-zsh"

# If you do not want any theme enabled, just set ZSH_THEME to blank: ZSH_THEME=""
ZSH_THEME=""

plugins=(git
  asdf
  systemd
  fzf
  direnv
  poetry
  ansible
  extract
  zsh-syntax-highlighting
  zsh-completions
  zsh-autosuggestions
  archlinux
  alias-finder
  pdm
  docker
  ssh
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

# Local config
[[ -f $HOME/.zshrc.local ]] && source $HOME/.zshrc.local

# Take a look at the playbooks/path.yml file
[[ ! -f $HOME/.local/bin/path ]] || source $HOME/.local/bin/path
source $ZSH/oh-my-zsh.sh

# aliases
[[ -f $HOME/.aliases ]] && source $HOME/.aliases

eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
