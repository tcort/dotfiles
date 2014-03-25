" VIM Start-up File
" Thomas Cort <linuxgeek@gmail.com>
" Last Modified: March 25, 2014

" gnome terminal suggestions:
"   Font: DejaVu Sans Mono Book 12
"   Colors: White on black
"   Terminal Bell: Off
"   Show menubar by default in new terminals: Off

" Installing the colour scheme:
" 	mkdir ~/.vim
" 	git clone https://github.com/Lokaltog/vim-distinguished /tmp/vim-distinguished
" 	cp -R /tmp/vim-distinguished/colors  ~/.vim/colors
" 	rm -rf /tmp/vim-distinguished/colors

if v:version < 700
	echoerr 'This vimrc requires Vim 7 or later.'
	quit
endif

" Don't emulate vi bugs and limitations!
set nocompatible

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

" Enable spell checking
autocmd BufNewFile,BufRead,BufEnter *.txt,*.md set spell spelllang=en

" Add a line at column 80 for C files to remind me to stay inside the lines ;)
autocmd BufNewFile,BufRead,BufEnter *.c,*.h set colorcolumn=80

