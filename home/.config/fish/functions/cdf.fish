# cd with fzf
function cdf
    set dir (find $HOME/.config $HOME/repos $HOME/docs $HOME/media $HOME/.local $HOME/downloads -type d | rg -v '.*/\.git/.*' | sed "s|$HOME|~|" | fzf | sed "s|~|$HOME|")
    [ -d "$dir" ] && builtin cd "$dir"
end

