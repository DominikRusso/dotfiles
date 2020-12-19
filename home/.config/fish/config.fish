# use vim mode
fish_vi_key_bindings

# use vim cursors
fish_vi_cursor
set -x fish_cursor_insert line
set -x fish_cursor_replace_one underscore

# hide mode indicator
function fish_mode_prompt
    # NOP
end

# disable fish greeting
set fish_greeting

# abbreviations
abbr -a chmod 'chmod -v'
abbr -a chown 'chown -v'
abbr -a cl 'clear'
abbr -a cp 'cp -iv'
abbr -a ipinfo 'curl ipinfo.io'
abbr -a ll 'ls -AFhl' # list long
abbr -a lm 'ls -1AFt' # last modified
abbr -a ls 'ls -F'
abbr -a mkdir 'mkdir -pv'
abbr -a mv 'mv -iv'
abbr -a myip 'curl ipinfo.io/ip'
abbr -a nb 'newsboat'
abbr -a nf 'neofetch'
abbr -a open 'xdg-open'
abbr -a qr 'qrencode -m 2 -t ansi'
abbr -a rm 'rm -v'
abbr -a sdn 'sudo shutdown -h now'
abbr -a sha 'shasum -a 256'
abbr -a sp 'sudo pacman'
abbr -a untar 'tar -zxvf'
abbr -a wttr 'curl v2.wttr.in'
abbr -a yt 'youtube-dl -i --add-metadata'
abbr -a yta 'youtube-dl -i -x --add-metadata --audio-format mp3'

# aliases
alias diff 'diff --color=auto'
alias ffmpeg 'ffmpeg -hide_banner'
alias gdb 'gdb -q -nh -x $XDG_CONFIG_HOME/gdb/init'
alias ls 'ls --color=auto --group-directories-first'
alias R 'R -q'
alias r 'R -q'
alias startx 'startx $XINITRC'
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
set -x MYSQL_HISTFILE "$XDG_CACHE_HOME/mysql_history"
set -x MYVIMRC "$XDG_CONFIG_HOME/vim/vimrc"
set -x NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/npm/npmrc"
set -x PYTHONSTARTUP "$XDG_CONFIG_HOME/python/pythonrc"
set -x RANDFILE "$XDG_CACHE_HOME/rnd"
set -x RIPGREP_CONFIG_PATH "$XDG_CONFIG_HOME/ripgrep/rgrc"
set -x RUSTUP_HOME "$XDG_DATA_HOME/rustup"
set -x TASKRC "$XDG_CONFIG_HOME/task/taskrc"
set -x TMUX_TMPDIR "$XDG_RUNTIME_DIR"
set -x VIMINIT "source $MYVIMRC"
set -x XAUTHORITY "$XDG_RUNTIME_DIR/Xauthority"
set -x XINITRC "$XDG_CONFIG_HOME/xinitrc"
set -x XMONAD_CONFIG_DIR "$XDG_CONFIG_HOME/xmonad"

# PATH
set PATH ~/.local/bin $CARGO_HOME/bin $PATH

# program settings
set -x FZF_DEFAULT_OPTS '--reverse --height 25%'
set -x LS_COLORS 'di=0:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;'
set -x SXHKD_SHELL 'dash'

# fish colors
set -x fish_color_autosuggestion 555 brblack
set -x fish_color_cancel -r
set -x fish_color_command white
set -x fish_color_comment brblack
set -x fish_color_cwd red --bold
set -x fish_color_end bryellow
set -x fish_color_error brred
set -x fish_color_host white --bold
set -x fish_color_host_remote yellow
set -x fish_color_match brblue
set -x fish_color_normal white
set -x fish_color_operator cyan
set -x fish_color_param white
set -x fish_color_quote brwhite
set -x fish_color_redirection bryellow
set -x fish_color_search_match --background=brblack
set -x fish_color_selection white --bold --background=brblack
set -x fish_color_status brred --bold
set -x fish_color_user white --bold
set -x fish_color_valid_path -u
set -x fish_pager_color_description yellow
set -x fish_pager_color_prefix -u --bold
set -x fish_pager_color_progress black --background=white
