" Vim syntax file
" Language:    smdl-toy camera
" Filenames:   *.camera
"
" The camera format that `smdl-toy` reads beside a layout: one `camera` block
" holding the framing, the body and the lens it names, the shutter, and what
" the photographer turned, with a `motion` track of `at <seconds>` keys
" inside it. Everything about the scene itself is in the `.layout`, which
" `layout.vim` covers; the body a `sensor` names is in a `.sensor`, which
" `sensor.vim` covers; the prescription a `lens` names is in a `.lens`, which
" `lens.vim` covers; which instant to photograph and how big the picture is
" are the command line's alone.
" This file is derived directly from the parser in
" `programs/smdl-toy/Layout/CameraFile.cc`, so the words it knows inside a
" block are exactly the ones that block accepts, and anything else there is
" flagged the way the parser flags it.
"
" A camera file is identified by its `.camera` extension.
"
" Install: see the header of `layout.vim`, which covers all four files.
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
" The catch-all that flags an unknown word therefore comes first.

"--{ Comments
syn keyword cameraTodo contained TODO FIXME XXX HACK NOTE BUG

syn match cameraComment display "#.*$" contains=cameraTodo,@Spell
"--}

"--{ Literals
" A quoted string may not span lines and has no escape sequences: the lexer
" takes every character up to the closing quote verbatim. The only ones a
" camera holds are the paths its `lens` and `sensor` name.
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
"--}

"--{ Settings
" The framing, the shutter, the readout, and what the photographer turned.
" `distortion_fit` is bare, since the flag it mirrors takes no value either,
" and `readout_direction` takes one of four words.
syn keyword cameraSetting contained look_from look_to look_up fovy focal_length
syn keyword cameraSetting contained shutter
syn keyword cameraSetting contained readout readout_direction temperature iso
syn keyword cameraSetting contained white_balance
syn keyword cameraReadoutDirection contained down up left right
syn keyword cameraSetting contained fstop aperture focus blades blade_angle
syn keyword cameraSetting contained distortion_k1 distortion_k2 distortion_fit
syn keyword cameraSetting contained vignetting cat_eye cat_eye_radius

" The body and the lens. `sensor` names a `.sensor` beside this file, or
" `human`, the observer; `lens` names a `.lens`, or `ideal`, the thin lens.
" With a lens the field of view is a consequence rather than an input, so
" `fovy` and every setting that stands in for what a real lens does on its
" own are refused beside it; the parser says which, and it says it better
" than a color can.
syn keyword cameraSetting contained lens sensor
syn keyword cameraStandIn contained human ideal

" The two words `focus` takes in place of a distance, the second of which
" `iso` and `white_balance` take in place of a number too.
syn keyword cameraFocusWord contained infinity auto

" The white balance presets, which `white_balance` takes beside `auto` and
" a temperature in kelvin.
syn keyword cameraWhiteBalanceWord contained D65 daylight cloudy shade
syn keyword cameraWhiteBalanceWord contained tungsten fluorescent

" motion { at <seconds> ... } inside camera: a track of keys at absolute times
" on the render clock. A key restates any setting but `blades`,
" `distortion_fit`, `lens`, `sensor`, `temperature`, `iso`,
" `white_balance`, `shutter`, `readout`, and `readout_direction`, which are
" not quantities to interpolate, and
" states `focus` as a distance alone, never as `infinity` or `auto`.
syn keyword cameraSetting contained motion
      \ nextgroup=cameraMotionBlock skipwhite skipempty
syn keyword cameraMotionAt contained at
syn keyword cameraMotionSetting contained look_from look_to look_up fovy
syn keyword cameraMotionSetting contained focal_length
syn keyword cameraMotionSetting contained fstop aperture focus blade_angle
syn keyword cameraMotionSetting contained distortion_k1 distortion_k2
syn keyword cameraMotionSetting contained vignetting cat_eye cat_eye_radius
"--}

"--{ Blocks
syn cluster cameraCommon
      \ contains=cameraComment,cameraString,cameraNumber,cameraBadWord

syn region cameraBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=@cameraCommon,cameraSetting,cameraReadoutDirection,
      \ cameraStandIn,cameraFocusWord,cameraWhiteBalanceWord

syn region cameraMotionBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=@cameraCommon,cameraMotionAt,cameraMotionSetting
"--}

" Blocks nest at most two deep (camera, motion) and are short.
syn sync minlines=100

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
hi def link cameraStandIn         Constant
hi def link cameraFocusWord       Constant
hi def link cameraWhiteBalanceWord Constant

hi def link cameraDelim           Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:camera_no_error_highlight")
  hi def link cameraBadWord       Error
endif
"--}

let b:current_syntax = "camera"

let &cpo = s:cpo_save
unlet s:cpo_save
