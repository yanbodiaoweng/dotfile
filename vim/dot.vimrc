set number
set backspace=indent,eol,start
set viminfo=%,\"100,'10,/50,:100,h,f0,n~/.vim/cache/.viminfo
set nocompatible
syntax on
set showmode
set showcmd
set mouse=a
set encoding=utf-8
set t_Co=256
filetype plugin indent on
set autoindent
set tabstop=2

set hidden
set t_vb=
set shiftwidth=4
set softtabstop=2
set cursorline
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif 
set guicursor=i:block-iCursor-blinkon0,v:block-vCursor
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


"""""""""""""""""""""""""""""""""" Plungin
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
Plug 'fatih/vim-go', { 'tag': '*' }
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'
let g:go_auto_type_info = 1 
au filetype go inoremap <buffer> . .<C-x><C-o>

Plug 'vim-airline/vim-airline'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'ctrlpvim/ctrlp.vim'
" Initialize plugin system
call plug#end()


"""""""""""""""""""""""""""""""""" keybindings
" set your own personal modifier key to something handy
map <Space> <Leader>

" use ,v to make a new vertical split, ,s for horiz, ,x to close a split
noremap <leader>wv <c-w>v<c-w>l
noremap <leader>ws <c-w>s<c-w>j
noremap <leader>wd <c-w>c


" buffers
noremap <leader>r :History<CR>
noremap <leader>b :Buffers<CR>
noremap <leader>s :Lines<CR>
noremap <leader><TAB> <Space>b<CR>


" use ctrl-h/j/k/l to switch between splits
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

nnoremap <silent> <Esc>0 :NERDTreeToggle<CR>

fun! s:fzf_root()
	let path = finddir(".git", expand("%:p:h").";")
	return fnamemodify(substitute(path, ".git", "", ""), ":p:h")
endfun

nnoremap <silent> <Leader>ff :exe 'Files ' . <SID>fzf_root()<CR>
