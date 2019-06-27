#!/bin/sh

# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


# install useful utilities like `sponge`.
brew install moreutils

# install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils

# install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names

# install `wget` with IRI support.
brew install wget --with-iri

# install more recent versions of some macOS tools.
brew install vim --with-override-system-vi
brew install git
brew install grep
brew install openssh
brew install screen
brew install php
brew install gmp

# install general stuff
brew install cmake
brew install gcc
brew install geoip
brew install git-lfs
brew install htop
brew install imagemagick
brew install libomp
brew install neovim
brew install open-mpi
brew install pandoc
brew install python
brew install r
brew install watch
brew install youtube-dl

# install hacking and CTF tools
brwe install afl-fuzz
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

