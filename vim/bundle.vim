" get vundle if it is not present
if !isdirectory(expand("~/.vim/bundle/Vundle.vim"))
    silent !git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
endif


" Required Vundle setup
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Autocomplete
Plugin 'Valloric/YouCompleteMe'

" Fuzzy file finder
Plugin 'ctrlpvim/ctrlp.vim'

" Operate on file trees
Plugin 'scrooloose/nerdtree'

" lean and mean status/tabline for vim that's light as air
Plugin 'vim-airline/vim-airline'

" airline themes
Plugin 'vim-airline/vim-airline-themes'

" precision colorscheme for the vim text editor
Plugin 'altercation/vim-colors-solarized'

" Tender color scheme
Plugin 'jacoborus/tender.vim'

" Vastly improved Javascript indentation and syntax support in Vim.
Plugin 'pangloss/vim-javascript'

" Markdown Vim Mode
Plugin 'plasticboy/vim-markdown'

" a Git wrapper so awesome, it should be illegal
Plugin 'tpope/vim-fugitive'

" Provide easy code formatting in Vim by integrating existing code formatters.
Plugin 'chiel92/vim-autoformat'

" Multiple cursors
Plugin 'terryma/vim-multiple-cursors'


" All of your Plugins must be added before the following line
call vundle#end()

" required
filetype plugin indent on
