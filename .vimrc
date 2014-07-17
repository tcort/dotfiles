" VIM Start-up File
" Thomas Cort <linuxgeek@gmail.com>
" Last Modified: July 17, 2014
"
" gnome terminal suggestions:
"   Font: DejaVu Sans Mono Book 12
"   Colors: White on black
"   Terminal Bell: Off
"   Show menubar by default in new terminals: Off
"
" Initial Setup:
"	mkdir -p ~/.vim/bundle
" Installing the colour scheme:
" 	git clone https://github.com/Lokaltog/vim-distinguished /tmp/vim-distinguished
" 	cp -R /tmp/vim-distinguished/colors  ~/.vim/colors
" 	rm -rf /tmp/vim-distinguished/colors
" Install vundle to .vim/bundle/vundle
"	git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" Run BundleInstall
"	vim +BundleInstall
" Install jshint
"	sudo npm install -g jshint

if v:version < 700
	echoerr 'This vimrc requires Vim 7 or later.'
	quit
endif

" Don't emulate vi bugs and limitations!
set nocompatible

" Vundle Bundles
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'jelera/vim-javascript-syntax'
Bundle 'pangloss/vim-javascript'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'scrooloose/syntastic'
Bundle 'Raimondi/delimitMate'
filetype plugin indent on

" Enable lint
let g:syntastic_check_on_open=1
let g:syntastic_always_populate_loc_list=1

" Better backspace.
set backspace=indent,eol,start

" Show lines above/below the cursor
set scrolloff=12

" Ignore object files
set wildignore=*.o

" Always show the status line.
set laststatus=2

" Always show absolute line numbers
set number

" Always show position of the cursor.
set ruler

" Favourite colour scheme in default install.
set t_Co=256
syntax on
set background=dark
colorscheme distinguished

" No pesky backup files.
set nobackup

" Disable wife annoyance feature.
set noerrorbells

" Improve searching
set ignorecase smartcase
set incsearch
set nohlsearch

" JavaScript Completion and Indenting (2 spaces per indent, no tabs).
autocmd FileType javascript set softtabstop=4
autocmd FileType javascript set shiftwidth=4
autocmd FileType javascript set tabstop=4
autocmd FileType javascript set expandtab
autocmd FileType javascript set smarttab

" Enable spell checking
autocmd BufNewFile,BufRead,BufEnter *.txt,*.md set spell spelllang=en

" Add a line at column 80 for C files to remind me to stay inside the lines ;)
autocmd BufNewFile,BufRead,BufEnter *.c,*.h set colorcolumn=80

" Custom Keys
" Ctrl+c in insert mode to line split
imap <C-c> <CR><Esc>O
