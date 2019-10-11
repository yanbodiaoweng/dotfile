set number
set backspace=indent,eol,start
set nocompatible
syntax on
set showmode
set showcmd
set mouse=a
set encoding=utf-8
set t_Co=256
filetype indent on
set autoindent
set tabstop=2

set shiftwidth=4
set softtabstop=2
set cursorline
set textwidth=80
set wrap
set linebreak
set wrapmargin=2
set scrolloff=5
set laststatus=2
set  ruler
set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase
" set spell spelllang=en_us
set nobackup
set noswapfile
set undofile
set backupdir=~/.vim/.backup//  
set directory=~/.vim/.swp//
set undodir=~/.vim/.undo// 
set autochdir
set noerrorbells
set visualbell
set history=1000
set autoread
" set listchars=tab:»■,trail:■
" set list

set wildmenu
set wildmode=longest:list,full




" set your own personal modifier key to something handy
map <Space> <Leader>

" use ,v to make a new vertical split, ,s for horiz, ,x to close a split
noremap <leader>wv <c-w>v<c-w>l
noremap <leader>ws <c-w>s<c-w>j
noremap <leader>wd <c-w>c

" use ctrl-h/j/k/l to switch between splits
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
