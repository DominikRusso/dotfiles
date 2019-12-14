#
#            _
#    _______| |__  _ __ ___
#   |_  / __| '_ \| '__/ __|
#    / /\__ \ | | | | | (__
#   /___|___/_| |_|_|  \___|
#
#

# prompt ---------------------------------------------------------------------
PS1="%B%F{red}[%F{green}%n%F{white}@%F{green}%M%F{white}:%F{cyan}%3~%F{red}]%F{white}%#%b "


# load aliases and shortcuts -------------------------------------------------
[ -f "$HOME/.config/shortcuts" ] && source "$HOME/.config/shortcuts"
[ -f "$HOME/.config/aliases" ] && source "$HOME/.config/aliases"


# history --------------------------------------------------------------------
HISTFILE=~/.cache/zsh/history
HISTSIZE=100000
SAVEHIST=100000
setopt APPEND_HISTORY               # append history to HISTFILE when shell exits
setopt BANG_HIST                    # contextual history expansion (for !)
setopt EXTENDED_HISTORY             # save timestamps and duration


# vim keybindings ------------------------------------------------------------
bindkey -v
export KEYTIMEOUT=1                 # remove delay for going into normal mode

bindkey "^?" backward-delete-char   # enable backspacing over previous inserts
bindkey "^e" edit-command-line      # edit line in $EDITOR
autoload edit-command-line
zle -N edit-command-line

function zle-keymap-select {        # change cursor for different vi modes
    if [[ ${KEYMAP} == vicmd ]] ||
       [[ $1 = 'block' ]]
    then
        echo -ne '\e[1 q'
    elif [[ ${KEYMAP} == main  ]] ||
         [[ ${KEYMAP} == viins ]] ||
         [[ ${KEYMAP} = '' ]] ||
         [[ $1 = 'beam' ]]
    then
        echo -ne '\e[5 q'
    fi
}

zle -N zle-keymap-select

zle-line-init() {
    echo -ne "\e[5 q"
}

zle -N zle-line-init

echo -ne '\e[5 q'                   # use beam shape cursor on startup

preexec() {                         # use beam shape cursor for new prompts
    echo -ne '\e[5 q'
}

# completion -----------------------------------------------------------------
autoload -Uz compinit

setopt extendedglob                                     # enable more globbing
setopt no_nomatch                                       # when globbing fails use cmd as is
setopt globdots                                         # complete hidden files

compinit -d "$ZSHCACHE/zcompdump"                       # specify comppdump loc
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE

zstyle ':completion:*' rehash true                      # auto update for new programs

zstyle ':completion:*' completer \
    _complete _approximate                              # enable approximate completion

zstyle ':completion:*' matcher-list \
    '' 'm:{a-zA-Z}={A-Za-z}' \
    'r:|[._-]=* r:|=*' 'l:|=* r:|=*'                    # case insensitive completion

zstyle ':completion:*:*:-command-:*:*' ignored-patterns '_*'    # don't suggest completion functions

zstyle ':completion:*' insert-tab false                 # don't tab on empty buffer
zstyle -e ':completion:*' hosts 'reply=()'              # don't complete hosts from /etc/hosts


# keybindings ----------------------------------------------------------------
typeset -g -A key

key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

if [[ ${+terminfo[smkx]} && ${+terminfo[rmkx]} ]]
then
    autoload -Uz add-zle-hook-widget
    function zle_application_mode_start {
        echoti smkx
    }
    function zle_application_mode_stop {
        echoti rmkx
    }
    add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
    add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

autoload -Uz up-line-or-beginning-search    # up searches history upwards based on buffer
autoload -Uz down-line-or-beginning-search  # down searches history downwards based on buffer
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search


# functions ------------------------------------------------------------------

# cd into parent dir if trying to cd into file
cd() {
    if (( $+2 )); then
        builtin cd "$@"
        return 0
    fi

    if [ -f "$1" ]; then
        echo "cd ${1:h}" >&2
        builtin cd "${1:h}"
    else
        builtin cd "${@}"
    fi
}

cdf() {
    location=~/Documents
    if [ -z $2 ]; then
        name=$1
    else
        location=$1
        name=$2
    fi
    builtin cd $(find $location -type d -name $name 2> /dev/null)
}

# reload shell
reload() {
    exec "${SHELL}" "$@"
}

