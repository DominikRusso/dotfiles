# zshrc is read by interactive shells.
# used for setting a user's interactive shell configuration

# prompt ---------------------------------------------------------------------
PS1="%B%n%F{red}@%F{clear}%M%F{white} %F{red}%3~%F{clear}%#%b "


# general --------------------------------------------------------------------

# see `man zshoptions`
setopt auto_cd
setopt cd_silent
setopt interactive_comments

unsetopt beep


# load aliases and shell procedures ------------------------------------------
[ -f "$XDG_CONFIG_HOME/shell/aliases" ] &&
    source "$XDG_CONFIG_HOME/shell/aliases"
[ -f "$XDG_CONFIG_HOME/shell/procedures" ] &&
    source "$XDG_CONFIG_HOME/shell/procedures"


# history --------------------------------------------------------------------
HISTFILE=$XDG_CACHE_HOME/zsh/history
HISTSIZE=100000  # number of history items loaded into memory
SAVEHIST=1000000 # number of history items in history file

# see `man zshoptions`
setopt bang_hist
setopt extended_history
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt inc_append_history_time


# vim keybindings ------------------------------------------------------------
bindkey -v                          # use vim keybindings
export KEYTIMEOUT=1

bindkey "^?" backward-delete-char   # enable backspacing over previous inserts
bindkey "^e" edit-command-line      # edit line in $EDITOR with ^e
autoload edit-command-line
zle -N edit-command-line

function zle-keymap-select () {      # change cursor for different vim modes
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select

zle-line-init() {
    echo -ne "\e[5 q"
}
zle -N zle-line-init

echo -ne '\e[5 q'                    # use beam shape cursor on startup
preexec() {
    echo -ne '\e[5 q'                # use beam shape cursor for new prompts
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

zstyle ':completion:*:*:-command-:*:*' \
    ignored-patterns '_*'                               # don't suggest completion functions

zstyle ':completion:*' insert-tab false                 # don't tab on empty buffer
zstyle -e ':completion:*' hosts 'reply=()'              # don't complete hosts from /etc/hosts


# keybindings ----------------------------------------------------------------
typeset -g -A key

key[k]="${terminfo[kcuu1]}"
key[j]="${terminfo[kcud1]}"

[[ -n "${key[k]}"   ]] && bindkey -- "${key[k]}"   up-line-or-beginning-search
[[ -n "${key[j]}" ]] && bindkey -- "${key[j]}" down-line-or-beginning-search

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
