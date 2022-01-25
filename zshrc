export ZSH="$HOME/.oh-my-zsh"

ZSH_BASE=$HOME/dotfiles/zsh # Base directory for ZSH configuration
source $ZSH_BASE/antigen/antigen.zsh # Load Antigen

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions

antigen theme romkatv/powerlevel10k


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

antigen apply

# aliases
[[ -f $HOME/.aliases ]] && source $HOME/.aliases

# To customize prompt, run `p10k configure` or edit $HOME/.p10k.zsh.
[[ ! -f $HOME/.p10k.zsh ]] || source $HOME/.p10k.zsh

