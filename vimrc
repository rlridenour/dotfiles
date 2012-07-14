" File vimrc
" Author Randy Ridenour
" Modified 9 August 2011 1500

" Pathogen
call pathogen#infect()
syntax on

filetype plugin indent on     " required! 



""""""""""""""""""""""""""""""""""""""""""""""""""
" General

""""""""""""""""""""""""""""""""""""""""""""""""""


" window size
" set lines=60
" set columns=80

" Use Vim settings, not Vi
set nocompatible

" disable intro message
" set shortmess+=I



set grepprg=grep\ -nH\ $*



au BufWinLeave ?* mkview
au BufWinEnter ?* silent loadview

set confirm

" Time to wait after ESC
set timeoutlen=2000

" Default font
" set guifont=Inconsolata:h14

" Disable modelines
set modelines=0

set scrolloff=3
set showmode
set wildmenu
set wildmode=list:longest,full
set cursorline
set ttyfast
set undofile
set visualbell "don't beep



let mapleader = ","

" Edit vimrc with leader-ev
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""
" Searching

""""""""""""""""""""""""""""""""""""""""""""""""""

" search case-sensitivity
" Set ignorecase on
set ignorecase
set smartcase


" smart search (override 'ic' when pattern has uppers)
set scs

" Leader-space to clear highlighted search terms
nnoremap <leader><space> :noh<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""
" Text Formatting/Layout

""""""""""""""""""""""""""""""""""""""""""""""""""

set autoindent

" Set smart indenting
set smartindent

" Set tab spacing
set tabstop=4

set shiftwidth=4

" Show line and column number of cursor position
set ruler

set showcmd

syntax enable
" colorscheme torte
" colorscheme ir_black

" set background=dark
" colorscheme solarized

" set background=light
" colorscheme solarized

set linebreak
set spell spelllang=en_us
nmap <silent> <leader>s :set spell!<CR>
set enc=utf-8


" Turn on line numbering. Turn it off with "set nonu"
set nu


setlocal wrap linebreak nolist
set virtualedit=
setlocal display+=lastline

""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Cues

""""""""""""""""""""""""""""""""""""""""""""""""""

" show matching brackets for a moment
set showmatch

" how many tenths of a second to blink matching brackets for
set matchtime=5

" do not highlight searched phrases
set hlsearch

" but do highlight as you type you search phrase 
set incsearch

noremap  <buffer> <silent> k gk
"noremap  <buffer> <silent> gk k
noremap  <buffer> <silent> j gj
"noremap  <buffer> <silent> gj j
noremap  <buffer> <silent> 0 g0
noremap  <buffer> <silent> $ g$



noremap  <buffer> <silent> <Up>   gk
noremap  <buffer> <silent> <Down> gj
noremap  <buffer> <silent> <Home> g<Home>
noremap  <buffer> <silent> <End>  g<End>
inoremap <buffer> <silent> <Up>   <C-o>gk
inoremap <buffer> <silent> <Down> <C-o>gj
inoremap <buffer> <silent> <Home> <C-o>g<Home>
inoremap <buffer> <silent> <End>  <C-o>g<End>

:map <C-Right> <End>
:map <C-Left> <Home>
:map <C-Down> <PageDown>
:map <C-Up> <PageUp>

" Map space to search
map <space> /

"Use jk to escape
inoremap jk <Esc>
inoremap kj <Esc>

nnoremap ; :


" Window splitting

nmap <silent> <leader>sh :leftabove vnew<cr>
nmap <silent> <leader>sl :rightbelow vnew<cr>
nmap <silent> <leader>sk :leftabove new<cr>
nmap <silent> <leader>sj :rightbelow new<cr>
nmap <silent> <leader>swh :topleft vnew<cr>
nmap <silent> <leader>swl :botright vnew<cr>
nmap <silent> <leader>swk :topleft new<cr>
nmap <silent> <leader>swj :botright new<cr>

" Scroll the window next to the current one
"   (especially useful for two-window split)
nmap <silent> <leader>j <c-w>w<c-d><c-w>W
nmap <silent> <leader>k <c-w>w<c-u><c-w>W

" Remap omni-completion to CTRL+SPACE
" inoremap <> <C-x><C-o>


" let g:miniBufExplMapWindowNavVim = 1 
" let g:miniBufExplMapWindowNavArrows = 1 
" let g:miniBufExplMapCTabSwitchBufs = 1 
" let g:miniBufExplModSelTarget = 1




  autocmd BufRead *.md  set ai formatoptions=tcroqn2 comments=n:&gt;

augroup END


"Multimarkdown

"Multimarkdown to HTML  
nmap <leader>mmd :%!/usr/local/bin/mmd <cr>  
"</cr></leader> 


"Multimarkdown to LaTeX  
nmap <leader>mtex :%!/usr/local/bin/mmd2tex <cr>  
"</cr></leader> 


"Multimarkdown to LaTeX  
nmap <leader>modf :%!/usr/local/bin/mmd2odf <cr>  
"</cr></leader> 


" Turn off LustyExplorer ruby warning
let g:LustyExplorerSuppressRubyWarning = 1

" NERDTree settings (launch with \nt or \\)
let g:NERDTreeQuitOnOpen = 1
nmap <silent> <leader>nt <Esc>:NERDTreeToggle<CR>
nmap <silent> <leader>, <Esc>:NERDTreeToggle<CR>
 

:set hidden

let g:LatexBox_viewer = "skim"
let g:LatexBox_latexmk_options = "-pvc"

let g:PreviewBrowsers='safari,firefox'

" Marked Preview <leader>m
:nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>

" TeX completion
" set filetype on
" au FileType * exec("setlocal dictionary+=".$HOME."/.vim/dictionaries/".expand('<amatch>'))
" set complete+=k
