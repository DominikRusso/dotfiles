#
#            _              
#    _______| |__  _ __ ___ 
#   |_  / __| '_ \| '__/ __|
#    / /\__ \ | | | | | (__ 
#   /___|___/_| |_|_|  \___|
#

# custom prompt
PS1="%B%F{9}[%F{10}%n%F{15}@%F{10}%m%F{15}:%F{14}%3~%F{9}]%F{15}%# %b"


# keybindings
typeset -g -A key

key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"

[[ -n "${key[Up]}" ]] && bindkey -- "${key[Up]}" up-line-or-history
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-history

if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
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


# history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY_TIME

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}" ]] && bindkey -- "${key[Up]}" up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search


# command completion
autoload -Uz compinit
compinit
setopt COMPLETE_ALIASES
setopt extendedGlob
setopt promptsubst

# include hidden files
_comp_options+=(globdots)

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion::complete:*' use-cache 1


# force emacs keybindings
bindkey -e


# lang
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# set default visual and editor to neovim
export EDITOR=nvim
export VISUAL="$EDITOR"


# aliases
source $HOME/.aliases

# set up colors
red='\033[0;38;5;9m'
RED='\033[1;38;5;9m'
green='\033[0;38;5;10m'
GREEN='\033[1;38;5;10m'
yellow='\033[0;38;5;11m'
YELLOW='\033[1;38;5;11m'
blue='\033[0;38;5;12m'
BLUE='\033[1;38;5;12m'
purple='\033[0;38;5;5m'
PURPLE='\033[1;38;5;5m'
cyan='\033[0;38;5;14m'
CYAN='\033[1;38;5;14m'
RESET='\033[0m'


# FUNCTIONS

# cd into parent dir if trying to cd into file
cd() {
    if (( $+2 )); then
        builtin cd "$@"
        return 0
    fi

    if [ -f "$1" ]; then
        echo "${RED}cd ${1:h}${RESET}" >&2
        builtin cd "${1:h}"
    else
        builtin cd "${@}"
    fi
}


# reload shell
reload() {
    exec "${SHELL}" "$@"
}


# escape weird as fuck path into single-argument string
escape() {
    local escape_string_input
    read escape_string_input
    printf '%q\n' "$escape_string_input"
}


# set terminal title
termtitle() {
    case "$TERM" in
        rxvt*|xterm*|nxterm|gnome|screen|screen-*)
            local prompt_host="${(%):-%m}"
            local prompt_user="${(%):-%n}"
            local prompt_char="${(%):-%~}"
            case "$1" in
                precmd)
                    printf '\e]0;%s@%s: %s\a' "${prompt_user}" "${prompt_host}" "${prompt_char}"
                ;;
                preexec)
                    printf '\e]0;%s [%s@%s: %s]\a' "$2" "${prompt_user}" "${prompt_host}" "${prompt_char}"
                ;;
            esac
        ;;
    esac
}


# set terminal title
precmd() {
    termtitle precmd
}


# set terminal title with current command
preexec() {
    termtitle preexec "${(V)1}"
}
