# cd with fzf
function cdf
    set dir (fd . $HOME -t d --hidden --no-ignore | rg -v -e '\.git' \
            | sed "s|$HOME|~|" | fzf | sed "s|~|$HOME|")
    [ -d "$dir" ] && builtin cd "$dir"
end
