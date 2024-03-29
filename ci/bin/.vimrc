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

" imposta percorso Vundle di runtime e inizializza Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" abilita la gestione pacchetti con Vundle
Plugin 'gmarik/Vundle.vim'

" Tutti i plugin devono essere inseriti tra vundle#begin e vundle#end

call vundle#end()            " required
filetype plugin indent on    " required
