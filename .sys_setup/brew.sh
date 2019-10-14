#!/bin/sh

# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# install cask
brew install cask

# install more recent versions of some macOS tools
brew install git
brew install grep
brew install openssh
brew install screen
brew install php

# install general stuff
brew cask install basictex
brew install cmake
brew install ffmpeg
brew install gcc
brew install geoip
brew install git-lfs
brew install go
brew install htop
brew install id3lib
brew install imagemagick
brew install libomp
brew install moreutils
brew install neovim
brew install open-mpi
brew install pandoc
brew install proselint
brew install python
brew install r
brew install rust
brew install screenfetch
brew install watch
brew install wget
brew install youtube-dl

# install haskell platform
brew install ghc cabal-install

# install hacking and CTF tools
brew install afl-fuzz
brew install binwalk
brew install cifer
brew install gdb
brew install hashpump
brew install hydra
brew install john
brew install knock
brew install netpbm
brew install nikto
brew install nmap
brew install pngcheck
brew install radare2
brew install shellcheck
brew install socat
brew install sqlmap
brew install tcpflow
brew install tcpreplay
brew install tcptrace
brew install z3

# install some gui stuff
brew cask install cutter
brew cask install macvim	# "gui"
