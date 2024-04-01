set nocompatible              " obbligatorio
filetype off                  " obbligatorio

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
au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

au BufNewFile,BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2

au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

" Rimappature tasti
map <C-n> :NERDTreeToggle<CR>

" Status git 
" https://shapeshed.com/vim-statuslines/
" https://www.reddit.com/r/vim/comments/pq3xwa/how_to_get_the_git_branch_in_the_statusline/
function GitBranch()
    return trim(system("git branch --show-current"))
endfunction

set laststatus=2
set statusline=%m\%y\ %.100F\ %{GitBranch()}\ %=%(B:%n\ R:%l:%L\ C:%c\ %P\ %h%)


" imposta percorso Vundle di runtime e inizializza Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" abilita la gestione pacchetti con Vundle
Plugin 'gmarik/Vundle.vim'

" Tutti i plugin devono essere inseriti tra vundle#begin e vundle#end

" NerdTree (https://github.com/preservim/nerdtree)
Plugin 'preservim/nerdtree' 
"Plugin 'Xuyuanp/nerdtree-git-plugin'
"Plugin 'ryanoasis/vim-devicons'
Plugin 'jistr/vim-nerdtree-tabs'
" let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree

Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'
let python_highlight_all=1
syntax on

Plugin 'jnurmine/Zenburn'
Plugin 'altercation/vim-colors-solarized'
" call togglebg#map("<F5>")

Plugin 'tpope/vim-fugitive'

call vundle#end()            " required
filetype plugin indent on    " required
