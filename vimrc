" automatic reload of .vimrc file
augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

let mapleader="\<Space>"

" character encoding used inside vim (required by ycm)
set encoding=utf-8

" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

if filereadable(expand("~/.vimrc.bundles"))
  source ~/.vimrc.bundles
endif

" enable syntax highlighting
syntax on
filetype plugin indent on

" color scheme
set background=dark
colorscheme solarized

" better copy & paste
set pastetoggle=<F2>
set clipboard=unnamed

" show line numbers
set relativenumber
set tw=79   " document width
set nowrap  " do not automatically wrap
set fo-=t   " do not automatically wrap text when typing

" set tabs to have 4 spaces
set tabstop=4
set softtabstop=4

" indent when moving to the next line while writing code
set autoindent

" expand tabs into spaces
set expandtab

" when using the >> or << commands, shift lines by 4 spaces
set shiftwidth=4

" show the matching part of the pair for [] {} and ()
set showmatch

" remove ugly split bar
set fillchars+=vert:│
hi clear VertSplit

" enable all Python syntax highlighting features
let python_highlight_all = 1

" make search case insensitive
set hlsearch
set incsearch
set ignorecase
set smartcase

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

"""" plugin settingns

" airline plugin
let g:airline_powerline_fonts = 1
set laststatus=2
let g:airline_theme = 'powerlineish'
if !exists('g:airline_powerline_fonts')
    " Use the default set of separators with a few customizations
    let g:airline_left_sep=' ›'  " Slightly fancier than '>'
    let g:airline_right_sep='‹ ' " Slightly fancier than '<'
endif

" CtrlP
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

if filereadable(expand("~/.vimrc.map"))
  source ~/.vimrc.map
endif
