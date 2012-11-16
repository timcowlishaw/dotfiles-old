call pathogen#infect()

vnoremap <Left> h
vnoremap <Right> l
vnoremap <Up> k
vnoremap <Down> j

set incsearch
set number
set expandtab
set tabstop=2
set backspace=indent,eol,start
set expandtab
set shiftwidth=2
set shiftround
set scrolloff=3
set wrap
set formatoptions=1
set lbr
set guifont=Mensch
set mouse=a
set noswapfile
set grepprg=grep\ -nH\ $*
let g:NERDTreeChDirMode=0
let g:tex_flavor='latex'
let g:MarkdownPreviewTMP=$HOME.'/.vim/tmp/'
let g:MarkdownPreviewDefaultStyles = $HOME.'/.vim/stylesheets/'
let g:ackprg="ack-grep -H --nocolor --nogroup --column"
set ofu=syntaxcomplete#Complete
syntax on
set background=dark
let g:solarized_termcolors=16
se t_Co=16
colorscheme solarized
filetype plugin on
filetype indent on
filetype plugin indent on

function AlwaysCD()
  if bufname("") !~ "^ftp://"
    lcd %:p:h
  endif
endfunction

if has("autocmd")
  autocmd VimEnter * call AlwaysCD()
  \ if line("'\'") > 1 && line("'\'") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
endif

"turn off the arrow keys except for in command line mode!
inoremap  <Up> ""
inoremap  <Down> ""
inoremap  <Left> ""
inoremap  <Right> ""

vnoremap  <Up> ""
vnoremap  <Down> ""
vnoremap  <Left> ""
vnoremap  <Right> ""

nnoremap  <Up> ""
nnoremap  <Down> ""
nnoremap  <Left> ""
nnoremap  <Right> ""

onoremap  <Up> ""
onoremap  <Down> ""
onoremap  <Left> ""
onoremap  <Right> ""
"Useful keybindings
noremap <C-T> :CommandT<CR>
noremap <C-P> :bp<CR>
noremap <C-N> :bn<CR>
noremap <Nul> :BufExplorer<CR>

noremap <C-J> <C-W>j
noremap <C-K> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l

noremap <C-f> <C-W>t<C-W>K
noremap <C-g> <C-W>t<C-W>H
set wmh=0
vmap r "_dP

com T :NERDTreeToggle
command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

"...and for NerdTree:
let NERDTreeShowFiles=1
let NERDTreeHighlightCursorline=1


com R TlistToggle
let Tlist_Use_Right_Window=1


"from: http://vim.wikia.com/wiki/Pretty-formatting_XML
 function! DoPrettyXML()
  " save the filetype so we can restore it later
  let l:origft = &ft
  set ft=
  " delete the xml header if it exists. This will
  " permit us to surround the document with fake tags
  " without creating invalid xml.
  1s/<?xml .*?>//e
  " insert fake tags around the entire document.
  " This will permit us to pretty-format excerpts of
  " XML that may contain multiple top-level elements.
  0put ='<PrettyXML>'
  $put ='</PrettyXML>'
  silent %!xmllint --format -
  " xmllint will insert an <?xml?> header. it's easy enough to delete
  " if you don't want it.
  " delete the fake tags
  2d
  $d
  " restore the 'normal' indentation, which is one extra level
  " too deep due to the extra tags we wrapped around the document.
  silent %<
  " back to home
  1
  " restore the filetype
  exe "set ft=" . l:origft
endfunction

"use indents as the folding method"
set foldmethod=indent
set foldlevelstart=20


"make vim save and load the folding of the document each time it loads"
"also places the cursor in the last place that it was left."
au BufWinLeave * mkview
au BufWinEnter * silent loadview

command! PrettyXML call DoPrettyXML()

"Strip trailing whitespace on save!
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

"Add Git Grep func
func GitGrep(...)
  let save  = &grepprg
  set grepprg=git\ grep\ -n\ $*
  let command = 'grep'
  for arg in a:000
    let command = command . ' ' . arg
  endfor
  silent execute command
  botright copen
  let &grepprg = save
  let @/=a:000
  set hlsearch
  redraw!
endfun
command -nargs=? G call GitGrep(<f-args>)

func GitGrepWord()
  normal! "zyiw
  call GitGrep('-w -e ', getreg('z'))
endf
nmap <C-x>G :call GitGrepWord()<CR>
