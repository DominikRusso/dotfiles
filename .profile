export TERM="xterm-256color"


# set lang
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8


# default programs
export EDITOR="nvim"
export VISUAL="nvim"

# path
PATH=~/.local/bin:$PATH

# clean up ~
export CTCDIR="$HOME/.local/share/contacts"
export MYVIMRC="$HOME/.config/vim/.vimrc"
export TASKRC="$HOME/.config/task/.taskrc"
export VIMINIT="source $MYVIMRC"
export ZDOTDIR="$HOME/.config/zsh"
export ZSHCACHE="$HOME/.cache/zsh"

# program settings
export CLICOLOR=1
export LSCOLORS="exfxcxdxbxegedabagacad" # BSD
export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43" # Linux
export FZF_DEFAULT_OPTS="--layout=reverse --height 50%"
