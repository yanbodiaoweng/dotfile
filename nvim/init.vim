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
set tabstop=4
set expandtab

" set autowrite
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
set autoindent
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
"if empty(glob('~/.vim/autoload/plug.vim'))
"  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
"    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
"endif

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin(stdpath('data') . '/plugged')

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" Plug 'Xuyuanp/nerdtree-git-plugin'
" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
Plug 'fatih/vim-go', { 'tag': '*' }

" Plug 'dracula/vim', { 'as': 'dracula' }

Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'jceb/vim-orgmode'

Plug 'mhinz/vim-startify'
" Plug 'vim-airline/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'itchyny/lightline.vim'
" Plug 'https://github.com/vim-scripts/fcitx.vim.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdcommenter' 
Plug 'doums/darcula'
Plug 'ryanoasis/vim-devicons'

Plug 'sheerun/vim-polyglot'
Plug 'buoto/gotests-vim'
" Plug 'janko/vim-test'

"Plug 'jistr/vim-nerdtree-tabs'

Plug 'ianding1/leetcode.vim'
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }

call plug#end()

set termguicolors
" colorscheme dracula
colorscheme darcula
autocmd CursorHold * silent call CocActionAsync('highlight')
set sessionoptions+=globals

" for vim-go 
let g:go_def_mapping_enabled = 0

let g:startify_padding_left=100

"autocmd vimenter * NERDTree

let g:ranger_map_keys = 0
let g:NERDTreeHijackNetrw = 0 
let g:ranger_replace_netrw = 1
let g:lightline = {
                \	'colorscheme': 'darcula',
                \ 'active': {
                \   'left': [ [ 'mode', 'paste' ],
                 \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
                 \ },
                 \ 'component_function': {
                 \   'cocstatus': 'coc#status',
                 \   'readonly': 'LightlineReadonly',
                 \   'currentfunction': 'CocCurrentFunction'
                 \ },
                \}

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

let g:coc_global_extensions = [
      \'coc-pairs',
      \'coc-json',
      \'coc-tsserver',
      \'coc-tslint-plugin',
      \'coc-eslint',
      \'coc-prettier',
      \'coc-snippets',
      \'coc-lists',
      \'coc-yank',
      \'coc-yaml',
      \]

function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction
" Add spaces after comment delimiters by default
" let g:NERDSpaceDelims = 1
" " Use compact syntax for prettified multi-line comments
" let g:NERDCompactSexyComs = 1
" " Align line-wise comment delimiters flush left instead of following code indentation
" let g:NERDDefaultAlign = 'left'
" let g:NERDCommentEmptyLines = 1
" " Enable trimming of trailing whitespace when uncommenting
" let g:NERDTrimTrailingWhitespace = 1
" " Enable NERDCommenterToggle to check all selected lines is commented or not
" let g:NERDToggleCheckAllLines = 1
set guifont=DroidSansMono\ Nerd\ Font:h11
let g:org_indent = 1

let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_generate_tags = 1
let g:go_code_completion_enabled = 1
let g:go_auto_sameids = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_function_parameters = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_variable_assignments = 1
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
"""""""""""""""""""""""""""""""""" keybindings
" set your own personal modifier key to something handy
map <Space> <Leader>

let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
" use ,v to make a new vertical split, ,s for horiz, ,x to close a split
noremap <leader>wv <c-w>v<c-w>l
noremap <leader>ws <c-w>s<c-w>j
noremap <leader>wd <c-w>c


" buffers
noremap <leader>r :<C-u>CocList mru<CR>
noremap <leader>b :<C-u>CocList buffers<CR>
noremap <leader>o :<C-u>CocList -A outline<CR>
noremap <leader><TAB> <C-^>
noremap <leader>j :Ranger<CR>
noremap <leader>y :<C-u>CocList -A --normal yank<CR>

noremap <leader>D :<C-u>CocList files -F<CR>
noremap <leader>d :<C-u>CocList files<CR>
noremap <leader>F :<C-u>CocList grep -F<CR>
noremap <leader>f :<C-u>CocList words<CR>
"
" press <esc> to cancel.
nmap f <Plug>(coc-smartf-forward)
nmap F <Plug>(coc-smartf-backward)
nmap ; <Plug>(coc-smartf-repeat)
nmap , <Plug>(coc-smartf-repeat-opposite)
 
augroup Smartf
  autocmd User SmartfEnter :hi Conceal ctermfg=220 guifg=#6638F0
  autocmd User SmartfLeave :hi Conceal ctermfg=239 guifg=#504945
augroup end
" tools
map <leader>; <Plug>NERDCommenterToggle<CR>

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" code nevigation
" au filetype go noremap ; :GoDef<CR>
 au filetype go noremap r :GoReferrers<CR>
 au filetype go noremap s :GoImplements<CR>
"
" Remap keys for gotos
nmap <silent> ; <Plug>(coc-definition)
" nmap <silent> " <Plug>(coc-type-definition)
"nmap <silent> s <Plug>(coc-implementation)
"nmap <silent> r <Plug>(coc-references)
"nmap <silent> r <Plug>(coc-references)


" use ctrl-h/j/k/l to switch between splits
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

"nnoremap <silent> <Esc>0 :NERDTreeToggle<CR>
nnoremap <silent> <A-0> :NERDTreeToggle<CR>
nnoremap <silent> <A-f> :NERDTreeFind<CR>
au filetype go noremap <silent> <A-CR> :GoFillStruct<CR>

nnoremap <leader>ll :LeetCodeList<cr>
nnoremap <leader>lt :LeetCodeTest<cr>
nnoremap <leader>ls :LeetCodeSubmit<cr>
nnoremap <leader>li :LeetCodeSignIn<cr>

let g:leetcode_username = "FerrisEris"
let g:leetcode_solution_filetype = "golang"

function! Toggleterminal() abort
	if !has('nvim')
		return v:false
	endif

	if !exists('g:terminal')
		let g:terminal = {
			\ 'loaded': v:null,
			\ 'termbufferid': v:null,
			\ 'originbufferid': v:null
		\ }
	endif

	function! g:terminal.on_exit(jobid, data, event)
		silent execute 'buffer' g:terminal.originbufferid
		silent execute 'bdelete!' g:terminal.termbufferid

		let g:terminal = {
			\ 'loaded': v:null,
			\ 'termbufferid': v:null,
			\ 'originbufferid': v:null
		\ }
	endfunction

	" Create terminal and finish.
	if !g:terminal.loaded
		let g:terminal.originbufferid = bufnr('')

		enew | call termopen(&shell, g:terminal)
		let g:terminal.loaded = v:true
		let g:terminal.termbufferid = bufnr('')

		return v:true
	endif

	" Go back to origin buffer if current buffer is terminal.
	if g:terminal.termbufferid ==# bufnr('')
		silent execute 'buffer' g:terminal.originbufferid

	" Launch terminal buffer and start insert mode.
	else
		let g:terminal.originbufferid = bufnr('')
		silent execute 'buffer' g:terminal.termbufferid
		startinsert
	endif
endfunction


" Toggle terminal buffer or create new one if there is none.

nnoremap <silent> <A-1> :call Toggleterminal()<Enter>
tnoremap <silent> <A-1> <C-\><C-n>:call Toggleterminal()<Enter>


" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
" set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)


function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" " Remap for format selected region
" xmap <leader>f  <Plug>(coc-format-selected)
" nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
" nmap <silent> <C-d> <Plug>(coc-range-select)
" xmap <silent> <C-d> <Plug>(coc-range-select)
"
" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" " Show all diagnostics
" nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions
" nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" " Show commands
" nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document
" nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" " Search workspace symbols
" nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list
" nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
" let g:go_def_mapping_enabled = 1

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
