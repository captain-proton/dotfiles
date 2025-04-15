#!/bin/bash

function join_by {
local d=${1-} f=${2-}
    if shift 2; then
        printf %s "$f" "${@/#/$d}"
    fi
}

SETUP_FILES=()
for YML in setup/*.yml
do
    FILENAME="$(basename $YML)"
    SETUP_FILES+=("${FILENAME%.*}")
done

if [[ -n "$1" && ${SETUP_FILES[*]} =~ $1 ]]; then
    echo "Setting up dotfiles for $1"
    python=$(pacman -Q python)
    if [[ $python != python* ]]; then
        sudo pacman -S --needed python
    fi

    # Create a python virtual environment if necessary
    if [ ! -f '.venv/bin/python' ]; then
        # This python executable points to the system installed version
        # asdf-vm will be installed later on
        echo "Creating virtual env"
        python -m venv .venv
    fi

    # Activate virtual environment
    source .venv/bin/activate

    # Install dependencies, for example ansible-dev-tools
    pip install -r requirements.txt

    # this needs to be done beforehand, so the yay module is available
    yay=$(pacman -Q yay)
    if [[ $yay != yay* ]] && [[ ! -f playbooks/library/yay ]]; then
        echo "Installing yay and necessary plugins"
        ansible-playbook --ask-become-pass playbooks/yay.yml
    fi

    # Install all required ansible roles and collections
    echo "Installing ansible roles and collections"
    ansible-galaxy install -r ansible_requirements.yml

    # Check if a vault password file has been set
    # inside the environment and a vault.yml file exists
    # that must be decrypted.
    # Ansible must ask for a password if .
    if compgen -G "host_vars/localhost/vault*" >/dev/null && [ -z "$ANSIBLE_VAULT_PASSWORD_FILE" ]; then
        ASK_VAULT_PASS='--ask-vault-pass'
    else
        ASK_VAULT_PASS=''
    fi

    echo "Executing setup"
    ansible-playbook --ask-become-pass setup/"$1".yml $ASK_VAULT_PASS
else
    printf "Enter '%s (" "$0"
    join_by \| "${SETUP_FILES[@]}"
    echo ")'"
fi

