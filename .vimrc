syntax on

" Vundle BEGIN
set nocompatible
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'git://github.com/sheerun/vim-polyglot.git'
Plugin 'git://github.com/kien/ctrlp.vim.git'

call vundle#end()
filetype plugin indent on
" Vundle END

" Color Theme
colorscheme onedark
let g:airline_theme='onedark'

" Enable search results highlight
set hlsearch
hi Search ctermbg=darkgrey ctermfg=grey

" Settings
set encoding=utf-8
set expandtab
set ignorecase
set incsearch
set autoread
set autoindent
set number
set ruler
set scrolloff=3
set shiftwidth=2
set showcmd
set smartcase
set softtabstop=2
set tabstop=8
set paste

" Search visually selected text
vnoremap // y/<C-R>"<CR>

