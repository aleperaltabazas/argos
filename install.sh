#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'

if hash stack 2/dev/null; then
    echo -e "${GREEN}Stack already installed"
else
    echo -e "${RED}Stack installation not found"
    echo -e "Installing Stack..."
    curl -sSL https://get.haskellstack.org/ | sh
fi

stack install
mv ~/.local/bin/argos-exe ~/.local/bin/argos
sudo argos compile argos -s ./argos.argos -t /etc/bash_completion.d/argos-completion.bash

echo -e "${GREEN} Argos installed successfully!"