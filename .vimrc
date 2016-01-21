set nocompatible
filetype off

" set the runtime path to include Vundle and init
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'
"Plugin 'bling/vim-airline' " status line
Plugin 'tpope/vim-fugitive' " Git branch
Plugin 'scrooloose/nerdtree' " File System
"All Plugins must go before this line
call vundle#end()
filetype plugin indent on

" Vim Powerline
python from powerline.vim import setup as pline_setup
python pline_setup()
python del pline_setup

set autoread " detect when a file is changed

set laststatus=2
let g:airline_powerline_fonts=1
" make backspace behave in a sane manner
set backspace=indent,eol,start

" set a map leader for more combos
let mapleader=','
let g:mapleader =','

set history=1000 " change history to 1000
set textwidth=80

" Tab Control
set expandtab " inserts spaces rather than tabs for <TAB>
set smarttab  " tab respects tabstop, shiftwidth and sofftabstop
set tabstop=4 " visible width of tabs
set softtabstop=4 " edit as if the tabs are 4 characters wide
set shiftwidth=4 " number of spaces to use for indent and unindent
set shiftround   " round indent to a mutiple of shiftwidth

" enable clipboard
set clipboard=unnamed

" faster redrawing
set ttyfast

" Searching
set ignorecase " case insensitive searching
set smartcase " case-sensitive if capital letter
set hlsearch  " highlight matches
set incsearch " search as you type
set nolazyredraw " don't redraw while exectuing macros

set showmatch " show matching braces
set mat=2

set encoding=utf8
let base16colorspace=256 " Access colors present in 256 Colorspace
set t_Co=256             " explicitly tell vim terminal supports 256 colors

" set number " shows line numbers
" set relativenumber "shows relative line numbers
set number

set wrap "turn on line wrapping
set linebreak " set soft wrapping
set showbreak=.. "show 2 dots at break

set autoindent " automatically set indent of new line
set smartindent

" CHange backup directories
set backupdir=~/.vim-tmp,~/.tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,/var/tmp,/tmp


""""""""""""""""""""""""""""""
"""""""" Key Bindings """"""""
""""""""""""""""""""""""""""""
" General Mappings/Shortcuts

" Quick Save Shortcut
nmap <leader>, :w<cr>

" remove extra whitespace
nmap <leader><space> :%s/\s\+$<cr>

" disable exmode
noremap Q <NOP>

" clear highlighted search
noremap <space> :set hlsearch! hlsearch?<cr>
