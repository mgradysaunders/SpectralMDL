" Vim syntax file
" Language:    smdl-toy lens
" Filenames:   *.lens
"
" The lens prescription format that `smdl-toy` reads: one `lens` block holding
" the surfaces light passes through between the scene and the film, front
" first, with exactly one `stop` among them, and any glasses of its own that
" the surfaces name. A `.camera` names a `.lens` and says where the picture is
" taken from and on what sensor; `camera.vim` covers that file, `sensor.vim`
" the sensor, and `layout.vim` the scene itself.
" This file is derived directly from the parser in
" `programs/smdl-toy/Layout/LensFile.cc`, so the words it knows inside a block
" are exactly the ones that block accepts, and anything else there is flagged
" the way the parser flags it. That is why a `surface` and a `stop` do not
" take the same words: the stop is a flat opening and has no shape or medium
" of its own to state.
"
" A lens file is identified by its `.lens` extension. Every length in one is
" in millimeters, which is what published prescriptions are written in, but
" for a Sellmeier `c`, a squared wavelength in square micrometers as a glass
" datasheet prints it.
"
" Install: see the header of `layout.vim`, which covers all four files.
"
" Options:
"
"   g:lens_no_error_highlight  Do not highlight a word that is not a setting
"                              of the block it sits in as an error.
"   g:lens_no_buffer_options   Do not `setlocal` any buffer options. (Set this
"                              if you keep them in an ftplugin.)

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

"--{ Buffer options
if !exists("g:lens_no_buffer_options")
  setlocal comments=:#
  setlocal commentstring=#\ %s
  setlocal formatoptions-=t formatoptions+=croql
  setlocal suffixesadd=.lens

  " The layout's indenter, which is the same grammar: braces are the only
  " structure and `#` begins a comment. Defined there when the other files are
  " installed, and here when only this one is.
  if !exists("*LayoutIndent")
    function LayoutIndent() abort
      let l:prev = prevnonblank(v:lnum - 1)
      if l:prev == 0
        return 0
      endif
      let l:indent = indent(l:prev)
      if getline(l:prev) =~# '{\s*\%(#.*\)\=$'
        let l:indent += shiftwidth()
      endif
      if getline(v:lnum) =~# '^\s*}'
        let l:indent -= shiftwidth()
      endif
      return l:indent > 0 ? l:indent : 0
    endfunction
  endif
  setlocal nosmartindent nocindent nolisp
  setlocal autoindent
  setlocal indentexpr=LayoutIndent()
  setlocal indentkeys=0{,0},!^F,o,O,e
endif
"--}

" NOTE: When two `syn match`/`syn region` items can start at the same position,
" Vim gives priority to the one defined LAST, and a `syn keyword` beats both.
" The catch-alls that flag an unknown word therefore come first.

"--{ Comments
syn keyword lensTodo contained TODO FIXME XXX HACK NOTE BUG

" A prescription is worth nothing without the patent or the book it was
" transcribed from, so a lens file is usually more comment than surface.
syn match lensComment display "#.*$" contains=lensTodo,@Spell
"--}

"--{ Literals
" A quoted string may not span lines and has no escape sequences: the lexer
" takes every character up to the closing quote verbatim.
syn region lensString display oneline start=+"+ end=+"+

syn match lensNumber display
      \ "\w\@<![-+]\=\%(\d\+\%(\.\d*\)\=\|\.\d\+\)\%([eE][-+]\=\d\+\)\=\w\@!"
"--}

"--{ The catch-alls
" A word that is not a directive, and not a setting of the block it sits in, is
" an error in the parser. Defined before everything else so that every keyword
" and every `nextgroup` name below overrides it.
syn match lensBadWord display "\<\h\w*\>"

" The glass's own: a word after `glass` that is no glass name. After a
" definition's `glass` it is still followed into the block, so the settings
" there keep their meaning.
syn match lensGlassBadName contained "[^ \t{}=#]\+"
      \ nextgroup=lensGlassBlock skipwhite skipempty
"--}

"--{ Directives
" lens { ... }, one to a file: surfaces are a sequence, and two sequences have
" no meaningful union, so a second block does not merge the way a camera's
" does.
syn keyword lensStatement lens nextgroup=lensBlock skipwhite skipempty
"--}

"--{ Settings
" What a lens block holds: its name; the surfaces in the order light meets
" them, front first and film last; and the glasses of its own that they name,
" `glass NAME { ... }`, above or below the first surface that names one.
syn keyword lensSetting contained name
syn keyword lensSetting contained surface
      \ nextgroup=lensSurfaceBlock skipwhite skipempty
syn keyword lensSetting contained stop
      \ nextgroup=lensStopBlock skipwhite skipempty
syn keyword lensSetting contained glass
      \ nextgroup=lensGlassDefinedName,lensGlassBadName skipwhite skipempty

" A glass name is a letter, then letters, digits, `-`, and `_`, which is how
" makers spell a designation (N-BK7), and it matches ignoring case. A word
" with any other character in it is no name at all.
syn match lensGlassDefinedName contained "\a[-0-9A-Za-z_]*[^ \t{}=#]\@!"
      \ nextgroup=lensGlassBlock skipwhite skipempty

" A refracting surface. `radius` is signed, positive when the center of
" curvature lies on the film side; `thickness` reaches the next vertex; `ior`
" is the index of the space after the surface at the d line, the same at every
" wavelength, and defaults to air; `glass` names the glass that space is
" instead, from the built-in catalog or the lens block; `diameter` is the
" clear aperture and is required. `conic` and `aspheric` are the shape beyond
" a sphere, the latter the even polynomial coefficients from `r^4` up.
syn keyword lensSurfaceSetting contained radius thickness ior diameter
syn keyword lensSurfaceSetting contained conic aspheric
syn keyword lensSurfaceSetting contained glass
      \ nextgroup=lensGlassName,lensGlassBadName skipwhite skipempty
syn match lensGlassName contained "\a[-0-9A-Za-z_]*[^ \t{}=#]\@!"

" The aperture stop, of which there is exactly one. It refracts nothing, so it
" states no medium, and it is flat, so it states no shape: what it has is a
" position and an opening.
syn keyword lensStopSetting contained thickness diameter

" A glass the lens defines, by one of three methods: `ior` and `abbe`, the
" partial dispersion taken from Schott's normal line; those and
" `partial_dispersion`; or `sellmeier { b B1 B2 B3 c C1 C2 C3 }`, the rows in
" the order a datasheet prints them, beside which `ior` and `abbe` are the
" printed values the coefficients are checked against.
syn keyword lensGlassSetting contained ior abbe partial_dispersion
syn keyword lensGlassSetting contained sellmeier
      \ nextgroup=lensSellmeierBlock skipwhite skipempty
syn keyword lensSellmeierRow contained b c
"--}

"--{ Blocks
syn cluster lensCommon
      \ contains=lensComment,lensString,lensNumber,lensBadWord

syn region lensBlock contained matchgroup=lensDelim start="{" end="}"
      \ contains=@lensCommon,lensSetting

syn region lensSurfaceBlock contained matchgroup=lensDelim start="{" end="}"
      \ contains=@lensCommon,lensSurfaceSetting

syn region lensStopBlock contained matchgroup=lensDelim start="{" end="}"
      \ contains=@lensCommon,lensStopSetting

syn region lensGlassBlock contained matchgroup=lensDelim start="{" end="}"
      \ contains=@lensCommon,lensGlassSetting

" The coefficients: two rows of three numbers, each after its letter.
syn region lensSellmeierBlock contained matchgroup=lensDelim start="{" end="}"
      \ contains=@lensCommon,lensSellmeierRow
"--}

" Blocks nest at most three deep (lens, glass, sellmeier) and are short.
syn sync minlines=100

"--{ Highlight links
hi def link lensTodo             Todo
hi def link lensComment          Comment

hi def link lensString           String
hi def link lensNumber           Number

hi def link lensStatement        Statement

hi def link lensSetting          Label
hi def link lensSurfaceSetting   Label
hi def link lensStopSetting      Label
hi def link lensGlassSetting     Label
hi def link lensSellmeierRow     Keyword
hi def link lensGlassName        Identifier
hi def link lensGlassDefinedName Identifier

hi def link lensDelim            Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:lens_no_error_highlight")
  hi def link lensBadWord        Error
  hi def link lensGlassBadName   Error
endif
"--}

let b:current_syntax = "lens"

let &cpo = s:cpo_save
unlet s:cpo_save
