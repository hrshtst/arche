" A handy configuration of Vim for my personal use.
"
" Author: Hiroshi Atsuta <atsuta@ieee.org>
" 
" Credits: I intentionally or unintentionally borrowed ideas and code
" from Vim experts which are open to public to write this configuration.
" Although I am not able to recall all of the references, main resources 
" can be listed as follows:
"   - .vimrc written by j7k6 (https://github.com/j7k6/dotfiles/blob/master/.vimrc)
"   - .vimrc written by Douglas Black (https://github.com/dougblack/dotfiles/blob/master/.vimrc)
"   - an article about key mappings by itmammoth (https://qiita.com/itmammoth/items/312246b4b7688875d023)

" Automatic installation of plugin manager
" ----------------------------------------
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins
" ----------------------------------------
call plug#begin('~/.vim/plugged')

" an alternative to vim-powerline
Plug 'itchyny/lightline.vim'

" theme
Plug 'joshdick/onedark.vim'

" syntax highlighting
Plug 'sheerun/vim-polyglot'

" enhanced support for vim in terminal
Plug 'wincent/terminus'

" interactive fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" commentary support
Plug 'tpope/vim-commentary'

call plug#end()

" Basic
" ----------------------------------------
set backspace=indent,eol,start
set clipboard&
set clipboard^=unnamed,unnamedplus
set encoding=utf8
set lazyredraw
set ttyfast
set nocursorline        " hilighting cursor line makes vim slower

" Colors
" ----------------------------------------
syntax on               " enable syntax highlighting
colorscheme onedark
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }

" Spaces & Tabs
" ----------------------------------------
filetype plugin on      " load filetype-specific plugin files
filetype indent on      " load filetype-specific indent files
set expandtab           " use spaces for tab
set tabstop=2           " use 4 spaces for one tab
set softtabstop=2       " insert spaces when typing tab 
set shiftwidth=2        " 4 spaces for indent
set modeline            " enable modeline
set modelines=1         " only one line is effective for modeline 
set autoindent          " enable autoindentation
set smartindent         " reflect the syntax when doing indentation

" UI Layout
" ----------------------------------------
set number              " show line numbers
set showcmd             " show command in the bottom line
set showmatch           " highlight matching parenthesis
set matchtime=1         " make time to show matching shorter
set wildmenu            " show fancy command completion
set wildmode=longest:full,full  " friendly completion behavior
set scrolloff=999       " keep cursor vertically center
set wrap                " wrap longer lines than width of window
set showbreak=↪\  
set listchars=tab:→\ ,trail:·,precedes:«,extends:»,eol:¶
set laststatus=2        " always show status bar
set noshowmode          " no show mode, use lightline.vim

" Searching
" ----------------------------------------
set ignorecase          " ignore case when searching
set smartcase           " but disable if upper case characters contained
set incsearch           " search as characters are entered
set hlsearch            " highlight all matches
let @/ = ""             " clear the last used search pattern

" Backups
" ----------------------------------------
set backup              " make backup files
set writebackup         " make backup files
set backupdir=~/.vim/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,~/tmp/*
set swapfile            " make swap files
set directory=~/.vim/tmp,/var/tmp,/tmp
set undofile            " make undo history persistent
set undolevels=9999     " (approximately) unlimited undos
set undodir=~/.vim/undo,/var/tmp,/tmp

" Key Mappings
" ----------------------------------------
" move vertically by visual line
nnoremap j gj
nnoremap k gk
" move to beginnin/end of line
nnoremap <s-h> ^
nnoremap <s-l> $
" yank characters under cursor until eol
nnoremap Y y$
" enable Emacs keybinds for cursor movement
inoremap <c-p> <up>
inoremap <c-n> <down>
inoremap <c-b> <left>
inoremap <c-f> <right>
inoremap <c-a> <home>
inoremap <c-e> <end>
" jj is escape
inoremap jj <esc>
" delete and backspace
inoremap <c-d> <del>
inoremap <c-h> <bs>
" escape and move cursor one char right
inoremap <c-]> <esc><right>
" turn highlight off and refresh window
nnoremap <silent> <c-l> :<c-u>nohlsearch<cr><c-l>
" go to last accessed window 
nnoremap <silent> <c-o> <c-w>p
" disable yank when deleting characters with x
vnoremap x "_x
nnoremap x "_x
" highlight last inserted text
nnoremap gV `[v`]

" Leader Shortcuts
" ----------------------------------------
let mapleader=","
nnoremap <leader>l :<c-u>set list! list?<cr>
nnoremap <leader>n :<c-u>set number! number?<cr>
nnoremap <leader>p :<c-u>set paste! paste?<cr>
nnoremap <leader>s :<c-u>source $MYVIMRC<cr>
nnoremap <silent> <leader>t :<c-u>%s/\s\+$//ge<cr>
nnoremap <leader>w :<c-u>set wrap! wrap?<cr>

" Use <space> as another prefix key
" ----------------------------------------
" highlight words under the cursor
nnoremap <silent> <space><space> "zyiw:let @/ = '\<' . @z . '\>'<cr>:set hlsearch<cr>
" fzf functions
nnoremap <silent> <space>b :<c-u>Buffers<cr>
nnoremap <silent> <space>c :<c-u>Commands<cr>
nnoremap <silent> <space>f :<c-u>Files<cr>
nnoremap <silent> <space>r :<c-u>History<cr>

" Automatic commands
" ----------------------------------------
augroup vimrc
  autocmd!
  autocmd FileType lisp setlocal commentstring=;;\ %s
  autocmd FileType make setlocal noexpandtab
augroup END

