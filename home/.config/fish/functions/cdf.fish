# cd with fzf
function cdf
    set dir (sed "s|$HOME|~|" < "$XDG_CACHE_HOME"/dirdb | fzf | sed "s|~|$HOME|")
    [ -d "$dir" ] && builtin cd "$dir"
end

