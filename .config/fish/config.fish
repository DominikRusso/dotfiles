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

if command -v nvim > /dev/null
    alias vim nvim
end

# program settings
setenv FZF_DEFAULT_OPTS '--layout=reverse --height 25%'
setenv LS_COLORS 'di=0:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;'

