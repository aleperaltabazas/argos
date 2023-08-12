#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'

if hash stack 2/dev/null; then
    echo -e "${GREEN}Stack already installed"
else
    echo -e "${RED}Stack installation not found"
    echo "Please, install Stack first"
    echo "\nhttps://docs.haskellstack.org/en/stable/#how-to-install-stack"
    exit 1
fi

stack --resolver lts-21.1 install
mv ~/.local/bin/argos-exe ~/.local/bin/argos
~/.local/bin/argos compile argos -s ./argos.argos
sudo mv argos-completion.bash /etc/bash_completion.d/argos-completion.bash

echo -e "${GREEN} Argos installed successfully!"
