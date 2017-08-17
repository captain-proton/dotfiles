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
set number
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

" Map <C-g> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-g> :nohl<CR><C-g>

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

" copy and paste
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <ESC>"+pa

nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" autoclose brackets
ino " ""<left>
ino ' ''<left>
ino < <><left>
ino ( ()<left>
ino [ []<left>
ino { {}<left>
ino {<CR> {<CR>}<ESC>0

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$

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

" NERDTree
map <Leader>n :NERDTreeToggle<CR>

