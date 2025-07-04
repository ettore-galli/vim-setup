call plug#begin('~/.vim/plugged')

" Plugin manager
Plug 'junegunn/vim-plug'

" Python IDE essentials
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'dense-analysis/ale'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
Plug 'vim-python/python-syntax'
Plug 'tpope/vim-fugitive'

call plug#end()

" Impostazioni base
syntax on
filetype plugin indent on
set number relativenumber
set tabstop=4 shiftwidth=4 expandtab
set clipboard=unnamedplus
set mouse=a
set encoding=utf-8
set termguicolors

" COC (autocompletamento)
let g:coc_global_extensions = ['coc-pyright']

" ALE (linting)
let g:ale_linters = {'python': ['flake8']}
let g:ale_fixers = {'python': ['autopep8']}
let g:ale_python_flake8_executable = 'flake8'
let g:ale_fix_on_save = 1

" Airline
let g:airline#extensions#tabline#enabled = 1

" NERDTree toggle
nnoremap <C-n> :NERDTreeToggle<CR>