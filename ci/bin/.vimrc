@REM set nocompatible              " obbligatorio
@REM filetype off                  " obbligatorio

" numeri
set number

" Codifica
set encoding=utf-8

" Indentazione
:set shiftwidth=4
:set tabstop=4
:set expandtab

" Scroll, wrap, ...
:set nowrap
:set sidescroll=16

" Parentesi
:set showmatch

" Highlight sintassi
set syntax=on

" Code editing
au BufNewFile, BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

au BufNewFile, BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2

au BufRead, BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

" Rimappature tasti
map <C-n> :NERDTreeToggle<CR>

" Colori
set t_Co=16777216

" Attiva il tema scelto
syntax enable
colorscheme dracula

" Airline setup
let g:airline_powerline_fonts = 0
let g:airline_symbols = {}
let g:airline_symbols_ascii = 1

@REM " Status git 
@REM " https://shapeshed.com/vim-statuslines/
@REM " https://www.reddit.com/r/vim/comments/pq3xwa/how_to_get_the_git_branch_in_the_statusline/
@REM function GitBranch()
@REM     return trim(system("git branch --show-current"))
@REM endfunction

@REM set laststatus=2
@REM set statusline=%m\%y\ %.100F\ %{GitBranch()}\ %=%(B:%n\ R:%l:%L\ C:%c\ %P\ %h%)

@REM " imposta percorso Vundle di runtime e inizializza Vundle
@REM set rtp+=~/.vim/bundle/Vundle.vim
@REM call vundle#begin()

@REM " abilita la gestione pacchetti con Vundle
@REM Plugin 'gmarik/Vundle.vim'

@REM " Tutti i plugin devono essere inseriti tra vundle#begin e vundle#end

@REM " NerdTree (https://github.com/preservim/nerdtree)
@REM Plugin 'preservim/nerdtree' 
@REM "Plugin 'Xuyuanp/nerdtree-git-plugin'
@REM "Plugin 'ryanoasis/vim-devicons'
@REM Plugin 'jistr/vim-nerdtree-tabs'
@REM " let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree

@REM Plugin 'vim-syntastic/syntastic'
@REM Plugin 'nvie/vim-flake8'
@REM let python_highlight_all=1
@REM syntax on

@REM Plugin 'jnurmine/Zenburn'
@REM Plugin 'altercation/vim-colors-solarized'
@REM " call togglebg#map("<F5>")

@REM Plugin 'tpope/vim-fugitive'

@REM call vundle#end()            " required
@REM filetype plugin indent on    " required
