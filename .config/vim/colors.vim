"  27 DodgerBlue2
"  33 DodgerBlue1
" 238 Gray27

" Major Syntax Groups
hi Comment      cterm=none      ctermfg=238         ctermbg=none
hi Constant     cterm=none      ctermfg=Gray        ctermbg=none
hi Identifier   cterm=none      ctermfg=Gray        ctermbg=none
hi Normal       cterm=none      ctermfg=Gray        ctermbg=none
hi PreProc      cterm=none      ctermfg=Gray        ctermbg=none
hi Special      cterm=none      ctermfg=Gray        ctermbg=none
hi Statement    cterm=none      ctermfg=Gray        ctermbg=none
hi Type         cterm=none      ctermfg=Gray        ctermbg=none
hi Underlined   cterm=underline ctermfg=none        ctermfg=none

" Minor Syntax Groups
hi Character    cterm=none      ctermfg=White       ctermbg=none
hi MatchParen   cterm=none      ctermfg=Black       ctermbg=White
hi Operator     cterm=none      ctermfg=White       ctermbg=none
hi SpecialChar  cterm=none      ctermfg=White       ctermbg=none
hi String       cterm=none      ctermfg=White       ctermbg=none
hi Title        cterm=none      ctermfg=Gray        ctermbg=none

" Non-Syntax
hi CursorLineNr cterm=none      ctermfg=White       ctermbg=none
hi DiffAdd      cterm=none      ctermfg=Black       ctermbg=DarkGreen
hi diffAdded    cterm=none      ctermfg=DarkGreen   ctermbg=none
hi DiffChange   cterm=none      ctermfg=Black       ctermbg=33
hi diffChanged  cterm=none      ctermfg=33          ctermbg=none
hi DiffDelete   cterm=none      ctermfg=White       ctermbg=DarkRed
hi diffRemoved  cterm=none      ctermfg=DarkRed     ctermbg=none
hi DiffText     cterm=none      ctermfg=Black       ctermbg=27
hi Error        cterm=none      ctermfg=Black       ctermbg=DarkRed
hi LineNr       cterm=none      ctermfg=238         ctermbg=none
hi SpecialKey   cterm=none      ctermfg=Black       ctermbg=White
hi SpellBad     cterm=underline ctermfg=Red         ctermbg=none
hi SpellCap     cterm=underline ctermfg=Yellow      ctermbg=none
hi SpellRare    cterm=underline ctermfg=none        ctermbg=none
hi Todo         cterm=none      ctermfg=Black       ctermbg=Green

hi! link diffCommon Normal
hi! link EndOfBuffer LineNr     " filler '~'

