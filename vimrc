set nocompatible
syntax on
filetype plugin indent on
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
set mouse=a
set noswapfile
set grepprg=grep\ -nH\ $*
let g:NERDTreeChDirMode=0
let g:tex_flavor='latex'
let g:MarkdownPreviewTMP=$HOME.'/.vim/tmp/'
let g:MarkdownPreviewDefaultStyles = $HOME.'/.vim/stylesheets/'
set ofu=syntaxcomplete#Complete
syntax on
set background=light
let g:solarized_termcolors=16
se t_Co=16
colorscheme solarized
filetype plugin on
filetype indent on
filetype plugin indent on

"Useful keybindings

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
com A :Ack
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
set showcmd

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

function! OpenSpec()
  let repl = substitute(substitute(expand('%'), '\.rb', '', ''), "lib/", "spec/", "")
  let path = repl . '_spec.rb'
  exec('tabe ' . path)
endfunction

function! VsplitSpec()
  let repl = substitute(substitute(expand('%'), '\.rb', '', ''), "lib/", "spec/", "")
  let path = repl . '_spec.rb'
  exec('vsplit ' . path)
endfunction

function! PromoteToLet()
    :normal! dd
    " :exec '?^\s*it\>'
    :normal! P
    :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
    :normal ==
    :normal! dd
endfunction

:command! PromoteToLet :call PromoteToLet():


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
set mouse=a
map <leader>jra      :!bundle exec rspec --no-color <CR>
map <leader>jrf      :!bundle exec rspec --no-color % <CR>
map <leader>jna      :!nosetests <CR>
map <leader>rpf      :!python % <CR>
map <leader>rrf      :!ruby %<CR>
map <leader>rb       :!bash <CR>
map <leader>rv       :so ~/.vimrc <CR>
map <leader>rm       :!touch % && make <CR>
map <leader>lt       *<CR>
map <leader>li       gg/def.*init<CR>
map <leader>lcd      gg/class.*<CR>
map <leader>lrp      /^ *p <CR>
map <leader>eal      :Align & <CR>
map <leader>eap      :Align => <CR>
map <leader>eae      :Align = <CR>
map <leader>ea{      :Align {<CR>
map <leader>epl      :PromoteToLet<cr>
map <leader>orf      :call OpenSpec()<CR>
map <leader>orv      :call VsplitSpec()<CR>
map <leader>ors      :e .rspec<CR>
map <leader>ovr      :e ~/.vimrc<CR>
nnoremap <leader>h <Esc>:call HardMode()<CR>
map <leader>bi       :!bundle install
map <leader><leader> :noh<CR>
map <leader><space>  :BufExplorer<CR>
map <leader>t        :NERDTreeToggle<CR>
map <leader>g        :G<Space>
map <leader>a        :Ack<Space>
map <leader>gst      :Gist<CR>
map <leader>dif      :DiffOrig<CR>
map <leader>l        :TagbarToggle<CR>
map <leader>enl      :EvervimNotebookList<CR>
map <leader>ecn      :EvervimCreateNote<CR>
map <leader>ipy      :IPython<CR>
map <leader>ipc      :IPythonClipbaord<CR>
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>
let g:evervim_devtoken="S=s2:U=9f6fb:E=14b49ebdc47:C=143f23ab049:P=1cd:A=en-devtoken:V=2:H=b8ed4c4264cd58f9455ed14e77260335"
