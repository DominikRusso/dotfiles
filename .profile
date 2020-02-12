export TERM="xterm-256color"

# set lang
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# default programs
export BROWSER="brave"
export EDITOR="nvim"
export FILE="lf"
export PAGER="less"
export READER="zathura"
export TERMINAL="alacritty"
export VISUAL="nvim"
export WM="dwm"

# path
PATH=~/.local/bin:$PATH

# XDG
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

# clean up ~/
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export CTCDIR="$XDG_DATA_HOME/ctcs"
export LESSHISTFILE="-"
export MYVIMRC="$XDG_CONFIG_HOME/vim/.vimrc"
export RANDFILE="$XDG_CACHE_HOME/rnd"
export TASKRC="$XDG_CONFIG_HOME/task/.taskrc"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export VIMINIT="source $MYVIMRC"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export ZSHCACHE="$XDG_CACHE_HOME/zsh"

# program settings
export CLICOLOR=1
export FZF_DEFAULT_OPTS="--layout=reverse --height 33%"
export LSCOLORS="exfxcxdxbxegedabagacad" # BSD
export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43" # Linux

