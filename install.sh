#!/bin/bash

case $1 in
  work|home)
    echo "Setting up dotfiles for $1"
    python=`pacman -Q python`
    if [[ $python != python* ]]
    then
      sudo pacman -S --needed python
    fi

    # Create a python virtual environment if necessary
    if [ ! -f '.venv/bin/python' ]; then
      # This python executable points to the system installed version
      # asdf-vm will be installed later on
      echo "Creating virtual env"
      python -m venv .venv
    fi

    # There is also a ansible playbook used to manage poetry.
    # Without poetry and the virtual env all further installation
    # tasks can not be executed. Therefore poetry must be present.
    POETRY_BIN="$HOME/.local/bin/poetry"
    if [ ! -L "$POETRY_BIN" ]; then
      curl -sSL https://install.python-poetry.org | python3 -
      $POETRY_BIN install
    fi

    # this needs to be done beforehand, so the yay module is available
    yay=`pacman -Q yay`
    if [[ $yay != yay* ]] && [[ ! -f playbooks/library/yay ]]
    then
      echo "Installing yay and necessary plugins"
      $POETRY_BIN run ansible-playbook playbooks/yay.yml
    fi

    # Check if a vault password file has been set
    # inside the environment and a vault.yml file exists
    # that must be decrypted.
    # Ansible must ask for a password if .
    if compgen 'host_vars/localhost/vault*' > /dev/null && [[ -z "$ANSIBLE_VAULT_PASSWORD_FILE" ]]
    then
      ASK_VAULT_PASS='--ask-vault-pass'
    else
      ASK_VAULT_PASS=''
    fi

    echo "Executing setup"
    $POETRY_BIN run ansible-playbook setup/$1.yml $ASK_VAULT_PASS ;;
  *)
    echo "Enter $0 work|home" ;;
esac
