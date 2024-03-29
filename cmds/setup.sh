#!/bin/bash

# sudo apt install git
# mkdir ~/packages/

# curl https://pyenv.run | bash

cd ~/dotfiles/cmds/
chmod a+x *.sh
chmod a+x ~/dotfiles/.bashrc.global

if [ -f ~/.bashrc ]; then
    mv ~/.bashrc ~/.bashrc.old
fi

ln -s ~/dotfiles/.bashrc.base ~/.bashrc
ln -s ~/dotfiles/init.el ~/.emacs

source setup-emacs.sh
