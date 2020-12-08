# find file and open in vim
function vf
    set file (fd . $HOME -t f --hidden --no-ignore \
            | rg -v \
                -e '\.git/' \
                -e '\.jpeg$' \
                -e '\.jpg$' \
                -e '\.mkv$' \
                -e '\.mp3$' \
                -e '\.mp4$' \
                -e '\.ogg$' \
                -e '\.png$' \
            | sed "s|$HOME|~|" | fzf | sed "s|~|$HOME|")
    [ -n "$file" ] && "$EDITOR" "$file"
end
