syntax on
let g:edge_transparent_background = 1
colorscheme edge
set listchars=tab:»\ ,eol:↲
set list
set spelllang=de,en
set nobackup
set noswapfile
set clipboard=unnamed
set history=100
set ruler
set showcmd
set noshowmode
set incsearch
set hlsearch
set smartcase
set ignorecase
set hidden
set nowrap
set backspace=indent,eol,start
set expandtab
set tabstop=4
set shiftwidth=4
set number
set nonumber
set rnu
set timeout timeoutlen=1000 ttimeoutlen=100
set laststatus=2
set mouse=a
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set signcolumn=yes
autocmd TermOpen * setlocal nonumber
autocmd TermOpen * setlocal norelativenumber
lua require'colorizer'.setup()

augroup highlight_yank
    autocmd!
    au TextYankPost * silent! lua vim.highlight.on_yank{higroup="Search", timeout=300}
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function FixLastSpellError()
    normal! mm[s1z=`m
endfun

nnoremap <leader>bn <cmd> bnext <CR>
nnoremap <leader>bp <cmd> bprevious <CR>
nnoremap <leader>bd <cmd> bd <CR>
nnoremap <leader>sf :source %<cr>
nnoremap <leader>sp :call FixLastSpellError() <cr>  


map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Zettelkasten Wiki with Pandoc Markdown
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set spellsuggest=fast

let g:nv_search_paths = ['~/Documents/myBib/notes']
let g:zettel_pdf_dict = '~/Documents/myBib/pdfs'
let g:zettel_bib_file = '~/Documents/myBib/main.bib'

function! GetCiteKeyUnderCursor() abort
    let line = getline('.')
    let key = matchstr(line,
                \ '\%<'.(col('.')+1).'c'.
                \ '\(@\)\zs[^,\];[:space:]]\+'.
                \ '\%>'.col('.').'c')
    return key
endfun

function! OpenCiteKeyInBib() abort
    let key = GetCiteKeyUnderCursor()
    let string = "e " . g:zettel_bib_file . " | /" . l:key
    execute string
endfun

function! OpenCiteKeyNote() abort 
    let key = GetCiteKeyUnderCursor()
    let string = "Corpus " . key
    execute string
endfun

function! OpenCiteKeyPDF() abort
    let key = GetCiteKeyUnderCursor()
    let string = "silent ! open " . g:zettel_pdf_dict . "/" . l:key . ".pdf"
    execute string
endfun

nnoremap <silent> <leader>cn :call OpenCiteKeyNote() <CR>
nnoremap <silent> <leader>cb :call OpenCiteKeyInBib() <CR>
nnoremap <silent> <leader>cp :call OpenCiteKeyPDF() <CR>
nnoremap <Leader>en <cmd>lua require'telescope.builtin'.find_files{ cwd = "~/.config/nvim/" }<CR>



let g:pandoc#completion#bib#mode = "fallback"
let g:pandoc#biblio#sources = "g"
let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
let g:pandoc#biblio#bibs = [$HOME.'/Documents/myBib/main.bib']
let g:pandoc#completion#bib#use_preview = 1
let g:pandoc#modules#disabled = ["folding"]

let g:languagetool_server_jar = expand('/usr/local/Cellar/languagetool/*/libexec/languagetool-server.jar')
autocmd Filetype pandoc LanguageToolSetUp

function! ToPDF() abort
   let string = "!pandoc --pdf-engine=xelatex --citeproc --csl ~/.pandoc/apa.csl --lua-filter ~/.pandoc/apa_und.lua --bibliography ~/Documents/myBib/main.bib -i " . expand('%:p') . " -o " . expand('%:p:r') . ".pdf && open " . expand('%:p:r') . ".pdf"
   silent execute string
endfun

function! ToWord() abort
   let string = "!pandoc --citeproc --csl ~/.pandoc/apa.csl --lua-filter ~/.pandoc/apa_und.lua --bibliography ~/Documents/myBib/main.bib -i " . expand('%:p') . " -o " . expand('%:p:r') . ".docx && open " . expand('%:p:r') . ".docx"
   silent execute string
endfun
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => R IDE
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let R_hl_term = 0 "R Output is colored by
let R_hi_fun_globenv = 2
let R_csv_app = "terminal: vd"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" =>  Linting & Completion
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


sign define LspDiagnosticsSignError text= texthl=LspDiagnosticsSignError linehl= numhl=
sign define LspDiagnosticsSignWarning text= texthl=LspDiagnosticsSignWarning linehl= numhl=
sign define LspDiagnosticsSignInformation text= texthl=LspDiagnosticsSignInformation linehl= numhl=
sign define LspDiagnosticsSignHint text=ﬤ texthl=LspDiagnosticsSignHint linehl= numhl=
