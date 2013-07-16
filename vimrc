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

"Useful keybindings
noremap <C-T> :CommandT<CR>
noremap <Nul> :BufExplorer<CR>

"navigate pup-ups with ctrl-j/k
inoremap <expr> <C-j> pumvisible() ? "\<C-N>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-P>" : "\<C-k>"

"adding emacs keybindings for command mode
cnoremap <c-a> <Home>
cnoremap <c-e> <End>

"navigate history with ctrl-j/k
cnoremap <c-k> <Up>
cnoremap <c-j> <Down>

set wmh=0
vmap r "_dP

com T :NERDTreeToggle
command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

"...and for NerdTree:
let NERDTreeShowFiles=1
let NERDTreeHighlightCursorline=1


com L TagbarToggle


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

set textwidth=80
set colorcolumn=+1

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

so ~/.vim/bundle/autotag/autotag.vim

let g:tagbar_type_scala = {
    \ 'ctagstype' : 'Scala',
    \ 'kinds'     : [
        \ 'p:packages:1',
        \ 'V:values',
        \ 'v:variables',
        \ 'T:types',
        \ 't:traits',
        \ 'o:objects',
        \ 'a:aclasses',
        \ 'c:classes',
        \ 'r:cclasses',
        \ 'm:methods'
    \ ]
\ }

let g:tagbar_type_markdown = {
	\ 'ctagstype' : 'markdown',
	\ 'kinds' : [
		\ 'h:Heading_L1',
		\ 'i:Heading_L2',
		\ 'k:Heading_L3'
	\ ]
\ }

let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }
