# use vim mode
fish_vi_key_bindings

# use vim cursors
fish_vi_cursor

# hide mode indicator
function fish_mode_prompt
    # NOP
end

# abbreviations
abbr -a 'cd..' 'cd ..'
abbr -a chomod 'chmod -v'
abbr -a chown 'chown -v'
abbr -a cp 'cp -iv'
abbr -a ll 'ls -Alh'
abbr -a llp 'expac "%m %n" | sort -h | numfmt --to=iec | tail -n 20'
abbr -a ls 'ls -F --group-directories-first'
abbr -a mkdir 'mkdir -pv'
abbr -a mp 'mp -iv'
abbr -a nb 'newsboat'
abbr -a nf 'neofetch'
abbr -a open 'xdg-open'
abbr -a qr 'qrencode -m 2 -t ansi'
abbr -a R 'R -q'
abbr -a r 'R -q'
abbr -a rm 'rm -v'
abbr -a sdn 'sudo shutdown -h now'
abbr -a sp 'sudo pacman'
abbr -a wttr 'curl v2.wttr.in'
abbr -a x 'startx $XINITRC'
abbr -a yt 'youtube-dl -i --add-metadata'
abbr -a yta 'youtube-dl -i -x --add-metadata --audio-format mp3'

# aliases
alias diff 'diff --color=auto'
alias ffmpeg 'ffmpeg -hide_banner'
alias gdb 'gdb -q -nh -x $XDG_CONFIG_HOME/gdb/init'
alias ls 'ls --color=auto'
alias tmux 'tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf'
alias vim 'nvim'
alias vimdiff 'nvim -d'

# environment variables
set -x TERM "alacritty"

# set lang
set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

# default programs
set -x BROWSER "qutebrowser"
set -x EDITOR "nvim"
set -x PAGER "less"
set -x READER "zathura"
set -x SUDO_ASKPASS "$HOME/.local/bin/dmenu_askpass"
set -x TERMINAL "alacritty"
set -x VISUAL "nvim"

# XDG
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"

# clean up ~/
set -x CARGO_HOME "$XDG_DATA_HOME/cargo"
set -x GNUPGHOME "$XDG_DATA_HOME/gnupg"
set -x LESSHISTFILE "-"
set -x MYVIMRC "$XDG_CONFIG_HOME/vim/vimrc"
set -x PYTHONSTARTUP "$XDG_CONFIG_HOME/python/pythonrc"
set -x RANDFILE "$XDG_CACHE_HOME/rnd"
set -x RIPGREP_CONFIG_PATH "$XDG_CONFIG_HOME/ripgrep/rgrc"
set -x RUSTUP_HOME "$XDG_DATA_HOME/rustup"
set -x TASKRC "$XDG_CONFIG_HOME/task/taskrc"
set -x TMUX_TMPDIR "$XDG_RUNTIME_DIR"
set -x VIMINIT "source $MYVIMRC"
set -x XAUTHORITY "$XDG_RUNTIME_DIR/Xauthority"
set -x XINITRC "$XDG_CONFIG_HOME/xinitrc"

# PATH
set PATH ~/.local/bin $CARGO_HOME/bin $PATH

# program settings
setenv FZF_DEFAULT_OPTS '--layout=reverse --height 25%'
setenv LS_COLORS 'di=0:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;'

