"          _
"         (_)
"   __   ___ _ __ ___  _ __ ___
"   \ \ / / | '_ ` _ \| '__/ __|
"    \ V /| | | | | | | | | (__
"     \_/ |_|_| |_| |_|_|  \___|
"


""" GENERAL """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    set nocompatible                    " disable vi compatibility -> enter current millennium

    let mapleader=","                   " set mapleader

    set encoding=utf-8                  " set encoding
    set fileencoding=utf-8
    set termencoding=utf-8

    set autoread                        " auto read file if it is changed from the outside
    set lazyredraw                      " don't redraw when not necessary

    autocmd BufEnter * set fo-=c fo-=r fo-=o    " disable auto-comment on newline 


" backspacing

    set backspace=indent,eol,start      " backspace over autoindentation, lines, previously inserted text


" indenting

    set autoindent                      " add/remove indentation level in special cases
    set expandtab                       " tabs are spaces
    set shiftwidth=4                    " tab = 4 spaces
    set smartindent                     " copy indentation level onto next level
    set smarttab                        " different tab behaviour in different cases
    set softtabstop=4                   " softtab stop every 4 spaces
    set tabstop=4                       " tab = 4 spaces


" searching

    set hlsearch                        " highlight search
    set incsearch                       " instant highlighting
    set ignorecase                      " case insensitive search
    set smartcase                       " except when query contains uppercase


" wrapping

    set tw=0                            " stop text being broken at 80


" splitting

    set splitbelow splitright           " better splitting behavior

    map <C-h> <C-w>h                    " better split navigation
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l


""" PLUGINS """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    call plug#begin('~/.config/vim/plugged')
    Plug 'bling/vim-airline'    " status line
    Plug 'scrooloose/nerdtree'  " file explorer
    Plug 'scrooloose/syntastic' " syntax checking
    Plug 'tpope/vim-commentary' " commenting lines
    Plug 'tpope/vim-surround'   " parens brackets quotes etc
    call plug#end()

""" VISUAL & UI """""""""""""""""""""""""

    set guifont=Menlo:h13               " readable font size in gvim
    set guicursor=                      " default gui cursor

    set background=dark                 " use colors that are visible on dark background

    syntax on                           " enable syntax highlighting

    set number relativenumber           " set relative and absolute line numbering

    set shortmess=I                     " disable splash screen

    set so=8                            " set scroll offset to 8 lines
    
    set wildmenu                        " enable wildmenu
    set wildmode=longest,list           " complete longest common string, then list alternatives

    set showmatch                       " show matching brackets
    set mat=0                           " dont move cursor back
    
    set noerrorbells                    " disable error bells
    set visualbell
    set t_vb=

    set guioptions-=r                   " disable scrollbars
    set guioptions-=R
    set guioptions-=l
    set guioptions-=L


""" STATUS LINE """""""""""""""""""""""""

    set showcmd                         " show command in bottom bar

    " tail of the filename
    set statusline=%t\

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
    
    set laststatus=2                                    " always display status bar

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


""" COMMANDS """"""""""""""""""""""""""""

    " enable matching (use % key)
    runtime macros/matchit.vim

    " treat dashed strings as words in stylesheets
    autocmd Filetype css,scss,sass setlocal iskeyword+=-

    " toggle spellcheck to english
    map <leader>e :setlocal spell! spelllang=en_us<CR>

    " toggle spellcheck to german
    map <leader>d :setlocal spell! spelllang=de_de<CR>

    " map substitute to S
    nnoremap S :%s///g<Left><Left><Left>

    " reload vimrc
    nnoremap <leader>sv :source $MYVIMRC<CR>


""" FILE TYPE SPECIFIC STUFF """"""""""""

    " turn on spell check in git commits
    autocmd FileType gitcommit set spell

    " treat dashed strings as words in stylesheets
    autocmd Filetype css,scss,sass setlocal iskeyword+=-

