"set the height of my window:
set lines=50

  "set the width:
set columns=80	
  
  "default MacVim colorscheme:
set background=dark
colorscheme solarized
" colorscheme ir_black

" set background=light
" colorscheme solarized

" change the default EasyMotion shading to something more readable with Solarized
" hi link EasyMotionTarget ErrorMsg
" hi link EasyMotionShade  Comment

  "while I like to spell check my .tex files, etc., I don't it going when I'm doing quick stuff in Vim so I set it just for MacVim here
set spell

set guifont=DejaVu\ Sans\ Mono:h13
set columns=80
set guioptions-=T
set guioptions-=r

" Command-1 for 80 columns, Command-2 for double.
map <D-1> :set co=80<CR>
map <D-2> :set co=165<CR>:winc =<CR>

" if has("gui_macvim")
"   " Remove default keybindings for Command-Control-Left/Right
"   macmenu Tools.Next\ Error key=
"   macmenu Tools.Previous\ Error key=
"   " switch to previous buffer
"   map  :bp
"   " switch to next buffer
"   map  :bN
" endif

"PeepOpen
if has("gui_macvim")
  macmenu &File.New\ Tab key=<nop>
  map <D-t> <Plug>PeepOpen
end

