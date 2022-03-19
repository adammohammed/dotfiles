# Dotfiles

[![Build Status](https://travis-ci.org/adammohammed/dotfiles.svg?branch=master)](https://travis-ci.org/adammohammed/dotfiles)

## Do a bare repo setup

```bash
$ git clone --bare https://www.github.com/adammohammed/dotfiles.git $HOME/.dotfiles.git
echo ".dotfiles.git" >> .gitignore
echo 'alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME"' >> $HOME/.zshrc
dotfiles checkout
dotfiles config --local status.showUntrackedFiles no
```
