"          _
"         (_)
"   __   ___ _ __ ___  _ __ ___
"   \ \ / / | '_ ` _ \| '__/ __|
"    \ V /| | | | | | | | | (__
"     \_/ |_|_| |_| |_|_|  \___|
"


""" GENERAL """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    set nocompatible                    " disable vi compatibility

    let mapleader=","                   " set mapleader

    set encoding=utf-8                  " set encoding
    set fileencoding=utf-8
    set termencoding=utf-8

    set autoread                        " auto read external file changes
    set lazyredraw                      " don't redraw when not necessary

    autocmd BufEnter * set fo-=c fo-=r fo-=o    " disable auto-comment on newline


" backspacing

    set backspace=indent,eol,start      " backspace over autoindents, lines, previous insertions


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

    map <C-h> <C-w>h
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

    set number relativenumber           " hybrid line numbering

    " use absolute line numbering when not in focus
    augroup numbertoggle
        autocmd!
        autocmd BufEnter,FocusGained * set relativenumber
        autocmd BufLeave,FocusLost   * set norelativenumber
    augroup END

    set shortmess=I                     " disable splash screen
    set shortmess+=a                    " use abbreviations in file messages

    set so=8                            " set scroll offset to 8 lines

    set wildmenu                        " enable wildmenu
    set wildmode=longest,list           " complete longest common string, then list alternatives

    set showmatch                       " show matching brackets
    set mat=0                           " dont move cursor back

    set noerrorbells                    " disable error bells
    set visualbell
    set t_vb=

    set guioptions-=m                   " disable menu bar

    set guioptions-=r                   " disable scrollbars
    set guioptions-=R
    set guioptions-=l
    set guioptions-=L


""" COMMANDS """"""""""""""""""""""""""""

    " enable matching (use % key)
    runtime macros/matchit.vim

    " check file in shellcheck
    map <leader>s :w \| !clear && shellcheck %<CR>

    " toggle spellcheck to english
    map <leader>e :setlocal spell! spelllang=en_us<CR>

    " toggle spellcheck to german
    map <leader>d :setlocal spell! spelllang=de_de<CR>

    " map substitute to S
    nnoremap S :%s///g<Left><Left><Left>

    " reload vimrc
    nnoremap <leader>v :source $MYVIMRC<CR>

    " automatically delete trailing whitespace on save
    autocmd BufWritePre * silent! %s/\s\+$//e


""" FILE TYPE SPECIFIC STUFF """"""""""""

    " turn on spell check in git commits
    autocmd FileType gitcommit set spell

    " treat dashed strings as words in stylesheets
    autocmd Filetype css,scss,sass setlocal iskeyword+=-

