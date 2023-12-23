#! /bin/bash

set -e
cd src
ghc -O2 main.hs -o sc
sudo cp sc /usr/local/bin
rm *.hi *.o
cd ..

if [ ! -d "/usr/local/lib/scriptLang" ]; then
    sudo mkdir /usr/local/lib/scriptLang
fi
sudo cp -r ./lib/* /usr/local/lib/scriptLang

echo "Finished installation!"
echo "Use 'sc' to run a script"