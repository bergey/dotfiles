set nocompatible

" tab and indent options
set tabstop=4
set autoindent
set smartindent
set shiftwidth=4
set smarttab
set showmatch
set expandtab
set showcmd
"filetype plugin indent on
set ruler
set backspace=indent,eol,start

nnoremap Q gqap

"useful prompt when :q w/o saving
set confirm

" lcase implies case insensitive, any ucase char implies case sensitive
set ignorecase
set smartcase

" if cursor is on first or last line of screen, shift screen for context
set scrolloff=2

" shelllike tab-completion of filenames
set wildmode=longest,list


" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endi

map <F5> :set norl nohkmap<CR>
map <F8> :set rl hkmap<CR>
imap <F5> <ESC>:set norl nohkmap<CR>a
imap <F8> <ESC>:set rl hkmap<CR>a

"au BufRead,BufNewFile *.TRD set filetype=trnsed
"au BufRead,BufNewFile *.trd set filetype=trnsed
"au! Syntax trnsed source ~/.vim/syntax/trnsed.vim

highlight Comment ctermfg=Cyan
