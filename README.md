# DOTFILES

I use these dotfiles on my development machine and on servers. The main
applications which are configured are vim and zsh with [oh-my-zsh](https://ohmyz.sh/),
the [powerlevel10k theme](https://github.com/romkatv/powerlevel10k) and a bunch of 
plugins including [YouCompleteMe](https://github.com/ycm-core/YouCompleteMe).
A few steps are needed to make this configuration getting to work. Take
a look at the ansible playbook `dotfiles.yml` to get detailed information
how a setup is done. Use the playbook to install the dotfiles on a server,
and the `install` script on a development machine.
