#!/bin/bash

case $1 in
  work|home)
    echo "Setting up dotfiles for $1"
    python=$(pacman -Q python)
    if [[ $python != python* ]]
    then
      sudo pacman -S --needed python
    fi

    pipx=$(pacman -Q python-pipx)
    if [[ $pipx != python-pipx* ]]
    then
      sudo pacman -S --needed python-pipx
    fi

    if ! command -v poetry 1>/dev/null; then
        pipx install poetry
    fi

    # Create a python virtual environment if necessary
    if [ ! -f '.venv/bin/python' ]; then
      # This python executable points to the system installed version
      # asdf-vm will be installed later on
      echo "Creating virtual env"
      python -m venv .venv
    fi

    # this needs to be done beforehand, so the yay module is available
    yay=$(pacman -Q yay)
    if [[ $yay != yay* ]] && [[ ! -f playbooks/library/yay ]]
    then
      echo "Installing yay and necessary plugins"
      poetry run ansible-playbook playbooks/yay.yml
    fi

    # Install all required ansible roles and collections
    echo "Installing ansible roles and collections"
    poetry run ansible-galaxy install -r ansible_requirements.yml

    # Check if a vault password file has been set
    # inside the environment and a vault.yml file exists
    # that must be decrypted.
    # Ansible must ask for a password if .
    if compgen -G "host_vars/localhost/vault*" > /dev/null && [ -z "$ANSIBLE_VAULT_PASSWORD_FILE" ]
    then
      ASK_VAULT_PASS='--ask-vault-pass'
    else
      ASK_VAULT_PASS=''
    fi

    echo "Executing setup"
    poetry run ansible-playbook setup/$1.yml $ASK_VAULT_PASS
    ;;
  *)
    echo "Enter $0 work|home" ;;
esac
