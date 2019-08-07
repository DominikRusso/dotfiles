"          _
"         (_)
"   __   ___ _ __ ___  _ __ ___
"   \ \ / / | '_ ` _ \| '__/ __|
"    \ V /| | | | | | | | | (__
"     \_/ |_|_| |_| |_|_|  \___|
"


""" GENERAL """""""""""""""""""""""""""""

    set nocompatible                    " enter current millennium

    set encoding=utf-8                  " set encoding
    set fileencoding=utf-8
    set termencoding=utf-8

    let mapleader=","                   " define mapleader

    filetype plugin on                  " enable file type specific settings

    set lazyredraw                      " dont redraw when not necessary

""" PLUGINS """""""""""""""""""""""""""""

    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    " let Vundle manage Vundle, required
    Plugin 'VundleVim/Vundle.vim'

    Plugin 'scrooloose/syntastic'
    Plugin 'scrooloose/nerdtree'

    call vundle#end()


""" VISUAL & UI """""""""""""""""""""""""

    set background=dark                 " use colors that are visible on dark background

    syntax on                           " enable syntax highlighting

    set number relativenumber           " set relative and absolute line numbering

    set shortmess=I                     " disable splash screen

    set so=8                            " set scroll offset to 8 lines

    set laststatus=2

    set guicursor=                      " block cursor in terminal nvim

    set showcmd                         " show command in bottom bar

    highlight VisualGuide80 ctermbg=235             " visual guide 1 color
    highlight VisualGuide100 ctermbg=238            " visual guide 2 color
    highlight VisualGuide120 ctermbg=241 ctermfg=0  " visual guide 3 color
    call matchadd('VisualGuide80', '\(\%>80v\)')    " visual guide starting @ 81
    call matchadd('VisualGuide100', '\(\%>100v\)')  " visual guide starting @ 101
    call matchadd('VisualGuide120', '\(\%>120v\)')  " visual guide starting @ 121

    set wildmenu                        " enable wildmenu

    set showmatch                       " show matching brackets
    set mat=0                           " dont move cursor back

    set visualbell                      " disable visual bells
    set t_vb=

    set splitbelow splitright           " better splitting behavior

    map <C-h> <C-w>h                    " better split navigation
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l

""" STATUS LINE """""""""""""""""""""""""

    set statusline=%t\   "tail of the filename

    " display a warning if file format isnt unix
    set statusline+=%#warningmsg#
    set statusline+=%{&ff!='unix'?'['.&ff.']':''}
    set statusline+=%*

    " display a warning if file encoding isnt utf-8
    set statusline+=%#warningmsg#
    set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
    set statusline+=%*

    " read only flag
    set statusline+=%#identifier#
    set statusline+=%r
    set statusline+=%*

    " modified flag
    set statusline+=%#warningmsg#
    set statusline+=%m
    set statusline+=%*

    " display a warning if &et is wrong, or we have mixed-indenting
    set statusline+=%#error#
    set statusline+=%{StatuslineTabWarning()}
    set statusline+=%*

    set statusline+=%#error#
    set statusline+=%{StatuslineTrailingSpaceWarning()}
    set statusline+=%*

    set statusline+=%{StatuslineLongLineWarning()}

    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*

    " display a warning if &paste is set
    set statusline+=%#error#
    set statusline+=%{&paste?'[paste]':''}
    set statusline+=%*

    set statusline+=%=                                  " left/right separator
    set statusline+=%{StatuslineCurrentHighlight()}\ \  " current highlight
    set statusline+=%c,                                 " cursor column
    set statusline+=%l/%L                               " cursor line/total lines
    set statusline+=\ %P                                " percent through file
    set laststatus=2

    " recalculate the trailing whitespace warning when idle, and after saving
    autocmd cursorhold,bufwritepost * unlet! b:statusline_trailing_space_warning

    " return '[\s]' if trailing white space is detected
    " return '' otherwise
    function! StatuslineTrailingSpaceWarning()
        if !exists("b:statusline_trailing_space_warning")

            if !&modifiable
                let b:statusline_trailing_space_warning = ''
                return b:statusline_trailing_space_warning
            endif

            if search('\s\+$', 'nw') != 0
                let b:statusline_trailing_space_warning = '[\s]'
            else
                let b:statusline_trailing_space_warning = ''
            endif
        endif
        return b:statusline_trailing_space_warning
    endfunction

    " return the syntax highlight group under the cursor ''
    function! StatuslineCurrentHighlight()
        if !exists('g:statusline_show_current_highlight')
            return ''
        endif

        let name = synIDattr(synID(line('.'),col('.'),1),'name')
        if name == ''
            return ''
        else
            return '[' . name . ']'
        endif
    endfunction

    " recalculate the tab warning flag when idle and after writing
    autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning

    " return '[&et]' if &et is set wrong
    " return '[mixed-indent]' if spaces and tabs are used to indent
    " return an empty string if everything is fine
    function! StatuslineTabWarning()
        if !exists("b:statusline_tab_warning")
            let b:statusline_tab_warning = ''

            if !&modifiable
                return b:statusline_tab_warning
            endif

            let tabs = search('^\t', 'nw') != 0

            " find spaces that arent used as alignment in the first indent column
            let spaces = search('^ \{' . &ts . ',}[^\t]', 'nw') != 0

            if tabs && spaces
                let b:statusline_tab_warning =  '[mixed-indent]'
            elseif (spaces && !&et) || (tabs && &et)
                let b:statusline_tab_warning = '[&et]'
            endif
        endif
        return b:statusline_tab_warning
    endfunction

    " recalculate the long line warning when idle and after saving
    autocmd cursorhold,bufwritepost * unlet! b:statusline_long_line_warning

    " return a warning for "long lines" where "long" is either &textwidth or 80 (if
    " no &textwidth is set)
    " return '' if no long lines
    " return '[#x,my,$z] if long lines are found, were x is the number of long
    " lines, y is the median length of the long lines and z is the length of the
    " longest line
    function! StatuslineLongLineWarning()
        if !exists("b:statusline_long_line_warning")

            if !&modifiable
                let b:statusline_long_line_warning = ''
                return b:statusline_long_line_warning
            endif

            let long_line_lens = s:LongLines()

            if len(long_line_lens) > 0
                let b:statusline_long_line_warning = "[" .
                            \ '#' . len(long_line_lens) . "," .
                            \ 'm' . s:Median(long_line_lens) . "," .
                            \ '$' . max(long_line_lens) . "]"
            else
                let b:statusline_long_line_warning = ""
            endif
        endif
        return b:statusline_long_line_warning
    endfunction

    " return a list containing the lengths of the long lines in this buffer
    function! s:LongLines()
        let threshold = (&tw ? &tw : 80)
        let spaces = repeat(" ", &ts)
        let line_lens = map(getline(1,'$'), 'len(substitute(v:val, "\\t", spaces, "g"))')
        return filter(line_lens, 'v:val > threshold')
    endfunction

    " find the median of the given array of numbers
    function! s:Median(nums)
        let nums = sort(a:nums)
        let l = len(nums)

        if l % 2 == 1
            let i = (l-1) / 2
            return nums[i]
        else
            return (nums[l/2] + nums[(l/2)-1]) / 2
        endif
    endfunction


""" AUTOMATIC BEHAVIOR """"""""""""""""""

    " auto read file if it is changed from the outside
    set autoread

    " disable auto comment on newline
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

    " turn on spell check in git commits
    autocmd FileType gitcommit set spell

    " treat dashed strings as words in stylesheets
    autocmd Filetype css,scss,sass setlocal iskeyword+=-


""" BACKSPACING """""""""""""""""""""""""

    set backspace=indent,eol,start      " backspace behavior that makes sense


""" INDENTATION """""""""""""""""""""""""

    set autoindent                      " automatic indentation on newlines

    set  smartindent                    " smart indentation

    set smarttab                        " smart tabs

    set expandtab                       " tabs are spaces

    set shiftwidth=4                    " tab = 4 spaces
    set tabstop=4
    set softtabstop=4


""" SEARCH """"""""""""""""""""""""""""""

    set hlsearch                        " highlight search

    set incsearch                       " instant highlighting

    set ignorecase                      " case insensitive search
    set smartcase                       " except when query contains uppercase


""" MISC """"""""""""""""""""""""""""""""


    runtime macros/matchit.vim                                  " enable matching (use % key)

    autocmd Filetype css,scss,sass setlocal iskeyword+=-        " treat dashed strings as words in stylesheets

    " toggle spellcheck to english
    map <leader>e :setlocal spell! spelllang=en_us<CR>

    " toggle spellcheck to german
    map <leader>g :setlocal spell! spelllang=de_de<CR>

    " map substitute to S
    nnoremap S :%s///g<Left><Left><Left>

    " reload vimrc
    nnoremap <leader>sv :source $MYVIMRC<CR>
