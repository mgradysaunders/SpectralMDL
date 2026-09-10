" Vim syntax file
" Language:    smdl-toy camera
" Filenames:   *.camera, *.response
"
" The camera format that `smdl-toy` reads beside a layout: one `camera` block
" holding the framing, the lens, the shutter, and the detector's `response`,
" with a `motion` track of `at <seconds>` keys inside it. Everything about
" the scene itself is in the `.layout`, which `layout.vim` covers; the
" prescription a `lens` names is in a `.lens`, which `lens.vim` covers; which
" instant to photograph and how big the picture is are the command line's
" alone.
"
" A `.response` file is the camera's `response` block on its own, in the
" same vocabulary, so this file covers it too. The one thing that costs: a
" top-level `response` is accepted in a `.camera` as well, where the parser
" refuses it, since the two extensions share one syntax file.
" This file is derived directly from the parser in
" `programs/smdl-toy/Layout/CameraFile.cc`, so the words it knows inside a
" block are exactly the ones that block accepts, and anything else there is
" flagged the way the parser flags it.
"
" A camera file is identified by its `.camera` extension, and a response file
" by `.response`.
"
" Install: see the header of `layout.vim`, which covers all three files.
"
" Options:
"
"   g:camera_no_error_highlight  Do not highlight a word that is not a setting
"                                of the block it sits in as an error.
"   g:camera_no_buffer_options   Do not `setlocal` any buffer options. (Set
"                                this if you keep them in an ftplugin.)

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

"--{ Buffer options
if !exists("g:camera_no_buffer_options")
  setlocal comments=:#
  setlocal commentstring=#\ %s
  setlocal formatoptions-=t formatoptions+=croql
  setlocal suffixesadd=.camera

  " The layout's indenter, which is the same grammar: braces are the only
  " structure and `#` begins a comment. Defined there when both files are
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
" The catch-all that flags an unknown word therefore comes first.

"--{ Comments
syn keyword cameraTodo contained TODO FIXME XXX HACK NOTE BUG

syn match cameraComment display "#.*$" contains=cameraTodo,@Spell
"--}

"--{ Literals
" A quoted string may not span lines and has no escape sequences: the lexer
" takes every character up to the closing quote verbatim. A camera holds the
" paths its `lens` and `response` name, and a response its `name`.
syn region cameraString display oneline start=+"+ end=+"+

syn match cameraNumber display
      \ "\w\@<![-+]\=\%(\d\+\%(\.\d*\)\=\|\.\d\+\)\%([eE][-+]\=\d\+\)\=\w\@!"
"--}

"--{ The catch-all
" A word that is not a directive, and not a setting of the block it sits in, is
" an error in the parser. Defined before everything else so that every keyword
" below overrides it.
syn match cameraBadWord display "\<\h\w*\>"
"--}

"--{ Directives
" camera { ... }, merged per field, last one wins.
syn keyword cameraStatement camera nextgroup=cameraBlock skipwhite skipempty

" response { ... } at the top level: the whole of a `.response` file.
syn keyword cameraStatement response
      \ nextgroup=cameraResponseBlock skipwhite skipempty
"--}

"--{ Settings
" The framing, the lens, the shutter, and the readout. `distortion_fit` is
" bare, since the flag it mirrors takes no value either, and
" `readout_direction` takes one of four words.
syn keyword cameraSetting contained look_from look_to look_up fovy shutter
syn keyword cameraSetting contained readout readout_direction
syn keyword cameraReadoutDirection contained down up left right
syn keyword cameraSetting contained fstop aperture focus blades blade_angle
syn keyword cameraSetting contained distortion_k1 distortion_k2 distortion_fit
syn keyword cameraSetting contained vignetting cat_eye cat_eye_radius

" A real lens and the sensor it covers. `lens` names a `.lens` beside this
" file and `sensor` is a width and a height in millimeters. With one, the
" field of view is a consequence rather than an input, so `fovy` and every
" setting that stands in for what a real lens does on its own are refused
" beside it; the parser says which, and it says it better than a color can.
syn keyword cameraSetting contained lens sensor

" response { ... } or response "x.response" inside camera: the detector's
" bands. Inside the block, `name` and `kind` (`relative` or `qe`), then
" `band NAME { <wavelength> <value> ... }` entries, and an optional
" `cfa { row NAME ... }` tile. A band name is any identifier, so the two
" blocks that hold names admit every word rather than flagging them.
syn keyword cameraSetting contained response
      \ nextgroup=cameraResponseBlock skipwhite skipempty
syn keyword cameraResponseSetting contained name kind
syn keyword cameraResponseKind contained relative qe
syn keyword cameraResponseSetting contained band
      \ nextgroup=cameraBandName skipwhite
syn match cameraBandName contained "\<\h\w*\>"
      \ nextgroup=cameraBandBlock skipwhite skipempty
syn keyword cameraResponseSetting contained cfa
      \ nextgroup=cameraCFABlock skipwhite skipempty
syn keyword cameraCFARow contained row
syn match cameraCFAName contained "\<\h\w*\>"

" motion { at <seconds> ... } inside camera: a track of keys at absolute times
" on the render clock. A key restates any setting but `blades`,
" `distortion_fit`, `lens`, `sensor`, `shutter`, `readout`, and
" `readout_direction`, which are not quantities to interpolate.
syn keyword cameraSetting contained motion
      \ nextgroup=cameraMotionBlock skipwhite skipempty
syn keyword cameraMotionAt contained at
syn keyword cameraMotionSetting contained look_from look_to look_up fovy
syn keyword cameraMotionSetting contained fstop aperture focus blade_angle
syn keyword cameraMotionSetting contained distortion_k1 distortion_k2
syn keyword cameraMotionSetting contained vignetting cat_eye cat_eye_radius
"--}

"--{ Blocks
syn cluster cameraCommon
      \ contains=cameraComment,cameraString,cameraNumber,cameraBadWord

syn region cameraBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=@cameraCommon,cameraSetting,cameraReadoutDirection

syn region cameraMotionBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=@cameraCommon,cameraMotionAt,cameraMotionSetting

syn region cameraResponseBlock contained matchgroup=cameraDelim
      \ start="{" end="}"
      \ contains=@cameraCommon,cameraResponseSetting,cameraResponseKind

" The knots: numbers only, so a stray word is flagged.
syn region cameraBandBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=cameraComment,cameraNumber,cameraBadWord

syn region cameraCFABlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=cameraComment,cameraCFARow,cameraCFAName
"--}

" Blocks nest at most three deep (camera, response, band), but a band's
" knots can run to hundreds of lines.
syn sync minlines=400

"--{ Highlight links
hi def link cameraTodo            Todo
hi def link cameraComment         Comment

hi def link cameraString          String
hi def link cameraNumber          Number

hi def link cameraStatement       Statement
hi def link cameraMotionAt        Keyword

hi def link cameraSetting         Label
hi def link cameraMotionSetting   Label
hi def link cameraReadoutDirection Constant
hi def link cameraResponseSetting Label
hi def link cameraResponseKind    Constant
hi def link cameraBandName        Identifier
hi def link cameraCFARow          Keyword
hi def link cameraCFAName         Identifier

hi def link cameraDelim           Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:camera_no_error_highlight")
  hi def link cameraBadWord       Error
endif
"--}

let b:current_syntax = "camera"

let &cpo = s:cpo_save
unlet s:cpo_save
