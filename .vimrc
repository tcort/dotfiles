" VIM Start-up File
" Thomas Cort <linuxgeek@gmail.com>
" Last Modified: July 17, 2014

" Only supported version of vim are versions I've tested with.
if v:version < 800
	echoerr 'This .vimrc requires vim 8 or later.'
	quit
endif

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

" Default colour scheme
set t_Co=256
syntax on
set background=dark

" No pesky backup files.
set nobackup

" Disable wife annoyance feature.
set noerrorbells

" Improve searching
set ignorecase smartcase
set incsearch

" JavaScript Indenting (4 spaces per indent, no tabs).
autocmd FileType javascript set tabstop=4
autocmd FileType javascript set expandtab
autocmd FileType javascript set softtabstop=4
autocmd FileType javascript set shiftwidth=4
autocmd FileType javascript set shiftround

" Enable spell checking
autocmd BufNewFile,BufRead,BufEnter *.txt,*.md set spell spelllang=en

" Add a line at column 80 for C files to remind me to stay inside the lines ;)
autocmd BufNewFile,BufRead,BufEnter *.c,*.h set colorcolumn=80

