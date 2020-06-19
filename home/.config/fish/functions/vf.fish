# find file and open in vim
function vf
    set file (sed "s|$HOME|~|" < "$XDG_CACHE_HOME"/filedb | fzf | sed "s|~|$HOME|")
    [ -n "$file" ] && "$EDITOR" "$file"
end

