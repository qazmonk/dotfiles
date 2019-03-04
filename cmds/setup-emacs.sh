#!/bin/bash

sudo apt install autoconf automake make
sudo aptitude install libpng-dev zlib1g-dev
sudo aptitude install libpoppler-glib-dev
sudo aptitude install libpoppler-private-dev
sudo aptitude install imagemagick


cd ~/packages/
git clone git@github.com:politza/pdf-tools.git
cd ~/packages/pdf-tools/
make install-server-deps
make -s
cd ~/dotfiles/


sudo apt install elpa-counsel elpa-ivy-hydra
