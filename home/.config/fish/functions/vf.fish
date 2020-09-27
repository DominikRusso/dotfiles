# find file and open in vim
function vf
    set file (find $HOME/.config $HOME/repos $HOME/docs $HOME/.local $HOME/downloads -type f | rg -v '.*/\.git/.*' | sed "s|$HOME|~|" | fzf | sed "s|~|$HOME|")
    [ -n "$file" ] && "$EDITOR" "$file"
end

