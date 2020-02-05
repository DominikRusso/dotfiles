"          _
"         (_)
"   __   ___ _ __ ___  _ __ ___
"   \ \ / / | '_ ` _ \| '__/ __|
"    \ V /| | | | | | | | | (__
"     \_/ |_|_| |_| |_|_|  \___|
"


""" GENERAL """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    set nocompatible                    " disable vi compatibility

    let mapleader=","                   " set mapleader

    set autoread                        " auto read external file changes
    set lazyredraw                      " don't redraw when not necessary


" backspacing

    set backspace=indent,eol,start      " backspace over autoindents, lines, previous insertions


" encoding

    set encoding=utf-8
    set fileencoding=utf-8
    set termencoding=utf-8


" indenting

    set autoindent                      " add/remove indentation level in special cases
    set expandtab                       " tabs are spaces
    set shiftwidth=4                    " tab = 4 spaces
    set smartindent                     " copy indentation level onto next level
    set smarttab                        " different tab behavior in different cases
    set softtabstop=4                   " softtab stop every 4 spaces
    set tabstop=4                       " tab = 4 spaces

" mouse

    set mouse=a                         " enable mouse in terminal vim


" searching

    set hlsearch                        " highlight search
    set incsearch                       " instant highlighting
    set ignorecase                      " case insensitive search
    set smartcase                       " except when query contains uppercase


" splitting

    set splitbelow splitright           " better splitting behavior
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l


" wrapping

    set tw=0                            " stop text being broken at 80


""" PLUGINS """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    call plug#begin('~/.config/vim/plugged')
    Plug 'vim-airline/vim-airline'          " status line
    Plug 'vim-airline/vim-airline-themes'   " status line themes
    Plug 'scrooloose/nerdtree'              " file explorer
    Plug 'scrooloose/syntastic'             " syntax checking
    Plug 'tpope/vim-commentary'             " commenting lines
    Plug 'tpope/vim-surround'               " fancy stuff with pairs of things

    Plug 'ryanoasis/vim-devicons'           " file type icons (load last)
    call plug#end()


""" VISUAL & UI """""""""""""""""""""""""""""""""""""""""""""""""""""

    set background=dark                 " use colors that are visible on dark background

    set number relativenumber           " hybrid line numbering

    syntax on                           " enable syntax highlighting

    set shortmess=I                     " disable splash screen
    set shortmess+=a                    " use abbreviations in file messages

    set so=8                            " set scroll offset to 8 lines

    set wildmenu                        " enable wildmenu
    set wildmode=longest,list           " complete longest common string, then list alternatives

    set showmatch                       " show matching brackets
    set mat=0                           " don't move cursor back

    set noerrorbells                    " disable error bells
    set visualbell
    set t_vb=

    let &t_SI = "\<Esc>[6 q"            " solid vertical bar in insert mode
    let &t_SR = "\<Esc>[4 q"            " solid underscore cursor in replace mode
    let &t_EI = "\<Esc>[2 q"            " solid block cursor in normal mode


""" BEHAVIOR & COMMANDS """""""""""""""""""""""""""""""""""""""""""""

    " matching using % key
    runtime macros/matchit.vim

    " check file in shellcheck
    map <leader>s :w \| !clear && shellcheck %<CR>

    " toggle spellcheck to english
    map <leader>e :setlocal spell! spelllang=en_us<CR>

    " toggle spellcheck to german
    map <leader>d :setlocal spell! spelllang=de_de<CR>

    " map substitute to S
    nnoremap S :%s//g<Left><Left>

    " reload vimrc
    nnoremap <leader>v :source $MYVIMRC<CR>

    " automatically delete trailing whitespace on save
    autocmd BufWritePre * silent! %s/\s\+$//e

    " no auto-commenting on newline
    autocmd BufEnter * set fo-=c fo-=r fo-=o

    " absolute line numbering when not in focus
    augroup numbertoggle
        autocmd!
        autocmd BufEnter,FocusGained * set relativenumber
        autocmd BufLeave,FocusLost   * set norelativenumber
    augroup END


""" FILE TYPE SPECIFIC STUFF """"""""""""""""""""""""""""""""""""""""

    " turn on spell check in git commits
    autocmd FileType gitcommit set spell

    " treat dashed strings as words in stylesheets
    autocmd Filetype css,scss,sass setlocal iskeyword+=-

