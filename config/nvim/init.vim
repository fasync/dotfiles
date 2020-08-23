" VIM Configuration File
" set nocompatible
" filetype off
syntax on
filetype plugin indent on
set modelines=0
set wrap

set formatoptions=tcqrn1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set noshiftround

set scrolloff=5
set backspace=indent,eol,start

set ttyfast

set laststatus=2
set showmode
set showcmd

set showmatch
set matchpairs+=<:>

set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

autocmd BufWritePre *.py ImpSort!
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
set shortmess+=c

inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

set number
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [POS=%l,%v][%p%%]\ [BUFFER=%n]\ %{strftime('%c')}

set hlsearch
set incsearch
set ignorecase
set smartcase

set viminfo='100,<9999,s100

nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

let python_highlight_all = 1
let g:lucius_style="dark"


" Plugins
call plug#begin(stdpath('data') . '/plugged')

Plug 'scrooloose/nerdtree'
Plug 'scrooloose/Syntastic'
Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }
Plug 'tweekmonster/impsort.vim'
Plug 'jonathanfilip/vim-lucius'

" ncm2
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-jedi'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-pyclang'

Plug 'davidhalter/jedi-vim'
call plug#end()

colorscheme lucius
