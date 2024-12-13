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
