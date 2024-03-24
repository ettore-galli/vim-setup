set nocompatible              " obbligatorio
filetype off                  " obbligatorio

" numeri
set number

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
