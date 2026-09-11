" Vim syntax file
" Language:    smdl-toy camera
" Filenames:   *.camera, *.sensor
"
" The camera format that `smdl-toy` reads beside a layout: one `camera` block
" holding the framing, the body and the lens it names, the shutter, and what
" the photographer turned, with a `motion` track of `at <seconds>` keys
" inside it. Everything about the scene itself is in the `.layout`, which
" `layout.vim` covers; the prescription a `lens` names is in a `.lens`, which
" `lens.vim` covers; which instant to photograph and how big the picture is
" are the command line's alone.
"
" A `.sensor` file is the body a `sensor` names: one `sensor` block holding
" the pixels and the pitch, the `response` bands and their tile, the
" `detector`, and the readout, in the same syntax, so this file covers it
" too. The one thing that costs: a top-level `sensor` is accepted in a
" `.camera` as well, where the parser refuses it, since the two extensions
" share one syntax file.
" This file is derived directly from the parsers in
" `programs/smdl-toy/Layout/CameraFile.cc` and `Layout/SensorFile.cc`, so
" the words it knows inside a block are exactly the ones that block accepts,
" and anything else there is flagged the way the parser flags it.
"
" A camera file is identified by its `.camera` extension, and a sensor file
" by `.sensor`.
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
" paths its `lens` and `sensor` name, and a sensor its `name`.
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

" sensor { ... } at the top level: the whole of a `.sensor` file.
syn keyword cameraStatement sensor
      \ nextgroup=cameraSensorBlock skipwhite skipempty
"--}

"--{ Settings
" The framing, the shutter, the readout, and what the photographer turned.
" `distortion_fit` is bare, since the flag it mirrors takes no value either,
" and `readout_direction` takes one of four words.
syn keyword cameraSetting contained look_from look_to look_up fovy focal_length
syn keyword cameraSetting contained shutter
syn keyword cameraSetting contained readout readout_direction temperature iso
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
" `iso` takes in place of a number too.
syn keyword cameraFocusWord contained infinity auto

" The sensor block, the whole of a `.sensor` file: the pixels and the
" pitch (or the size for the pitch to follow from), the response, the
" detector, and the readout.
syn keyword cameraSensorSetting contained name pixels pitch size
syn keyword cameraSensorSetting contained readout readout_direction
syn keyword cameraSensorSetting contained response
      \ nextgroup=cameraResponseBlock skipwhite skipempty
syn keyword cameraSensorSetting contained detector
      \ nextgroup=cameraDetectorBlock skipwhite skipempty

" response { ... } inside sensor: the bands. Inside the block, `kind`
" (`relative` or `qe`) and `peak_qe`, then `band NAME { <wavelength>
" <value> ... }` entries, an optional `cfa { row NAME ... }` tile, and an
" optional `rgb NAME NAME NAME`. A band name is any identifier, so the
" blocks that hold names admit every word rather than flagging them.
syn keyword cameraResponseSetting contained kind peak_qe rgb
syn keyword cameraResponseKind contained relative qe
syn keyword cameraResponseSetting contained band
      \ nextgroup=cameraBandName skipwhite
syn match cameraBandName contained "\<\h\w*\>"
      \ nextgroup=cameraBandBlock skipwhite skipempty
syn keyword cameraResponseSetting contained cfa
      \ nextgroup=cameraCFABlock skipwhite skipempty
syn keyword cameraCFARow contained row
syn match cameraCFAName contained "\<\h\w*\>"

" detector { ... } inside sensor: what the body reads out with, every key
" one number with a documented default.
syn keyword cameraDetectorSetting contained base_iso full_well read_noise
syn keyword cameraDetectorSetting contained dark_current
syn keyword cameraDetectorSetting contained reference_temperature
syn keyword cameraDetectorSetting contained doubling_temperature
syn keyword cameraDetectorSetting contained black_level bits gain max_iso

" motion { at <seconds> ... } inside camera: a track of keys at absolute times
" on the render clock. A key restates any setting but `blades`,
" `distortion_fit`, `lens`, `sensor`, `temperature`, `iso`, `shutter`,
" `readout`, and `readout_direction`, which are not quantities to
" interpolate, and
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
      \ cameraStandIn,cameraFocusWord

syn region cameraMotionBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=@cameraCommon,cameraMotionAt,cameraMotionSetting

syn region cameraSensorBlock contained matchgroup=cameraDelim
      \ start="{" end="}"
      \ contains=@cameraCommon,cameraSensorSetting,cameraReadoutDirection

" The `rgb` names are identifiers too, admitted like the tile's.
syn region cameraResponseBlock contained matchgroup=cameraDelim
      \ start="{" end="}"
      \ contains=@cameraCommon,cameraResponseSetting,cameraResponseKind,
      \ cameraRGBName
syn match cameraRGBName contained "\<\h\w*\>"

" The knots: numbers only, so a stray word is flagged.
syn region cameraBandBlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=cameraComment,cameraNumber,cameraBadWord

syn region cameraCFABlock contained matchgroup=cameraDelim start="{" end="}"
      \ contains=cameraComment,cameraCFARow,cameraCFAName

syn region cameraDetectorBlock contained matchgroup=cameraDelim
      \ start="{" end="}"
      \ contains=@cameraCommon,cameraDetectorSetting
"--}

" Blocks nest at most three deep (sensor, response, band), but a band's
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
hi def link cameraStandIn         Constant
hi def link cameraFocusWord       Constant
hi def link cameraSensorSetting   Label
hi def link cameraResponseSetting Label
hi def link cameraResponseKind    Constant
hi def link cameraBandName        Identifier
hi def link cameraCFARow          Keyword
hi def link cameraCFAName         Identifier
hi def link cameraRGBName         Identifier
hi def link cameraDetectorSetting Label

hi def link cameraDelim           Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:camera_no_error_highlight")
  hi def link cameraBadWord       Error
endif
"--}

let b:current_syntax = "camera"

let &cpo = s:cpo_save
unlet s:cpo_save
