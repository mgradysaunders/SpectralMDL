" Vim syntax file
" Language:    smdl-toy sensor
" Filenames:   *.sensor
"
" The sensor format that `smdl-toy` reads for the body a camera names: one
" `sensor` block holding the pixel array and its pitch, the `response` bands
" and the tile that lays them over the pixels, the `detector` that turns
" electrons into digital numbers, and the readout. A `.camera` names a
" `.sensor` and says what the photographer turned; `camera.vim` covers that
" file, `lens.vim` the lens, and `layout.vim` the scene itself.
" This file is derived directly from the parser in
" `programs/smdl-toy/Layout/SensorFile.cc`, so the words it knows inside a
" block are exactly the ones that block accepts, and anything else there is
" flagged the way the parser flags it. A word a key takes as its value is
" known only right after that key, where the parser reads it: `relative` is a
" kind after `kind` and an error anywhere else.
"
" A sensor file is identified by its `.sensor` extension. Wavelengths are in
" nanometers, the pitch in micrometers, the size in millimeters, and the
" readout in seconds.
"
" Install: see the header of `layout.vim`, which covers all four files.
"
" Options:
"
"   g:sensor_no_error_highlight  Do not highlight a word that is not a setting
"                                of the block it sits in as an error.
"   g:sensor_no_buffer_options   Do not `setlocal` any buffer options. (Set
"                                this if you keep them in an ftplugin.)

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

"--{ Buffer options
if !exists("g:sensor_no_buffer_options")
  setlocal comments=:#
  setlocal commentstring=#\ %s
  setlocal formatoptions-=t formatoptions+=croql
  setlocal suffixesadd=.sensor

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
syn keyword sensorTodo contained TODO FIXME XXX HACK NOTE BUG

syn match sensorComment display "#.*$" contains=sensorTodo,@Spell
"--}

"--{ Literals
" A quoted string may not span lines and has no escape sequences: the lexer
" takes every character up to the closing quote verbatim. The only one a
" sensor holds is its `name`.
syn region sensorString display oneline start=+"+ end=+"+

syn match sensorNumber display
      \ "\w\@<![-+]\=\%(\d\+\%(\.\d*\)\=\|\.\d\+\)\%([eE][-+]\=\d\+\)\=\w\@!"
"--}

"--{ The catch-alls
" A word that is not a directive, and not a setting of the block it sits in, is
" an error in the parser. Defined before everything else so that every keyword
" and every `nextgroup` name below overrides it.
syn match sensorBadWord display "\<\h\w*\>"

" The tile's own: every identifier there is a band name, so what a row refuses
" is a word of any other shape, such as a number or a quoted string.
syn match sensorCFABad contained display "[^ \t{}=#]\+"

" The band's own: a word after `band` that is no name, `row` among them. It is
" still followed into the knots, so the blocks after it keep their meaning.
syn match sensorBandBadName contained "[^ \t{}=#]\+"
      \ nextgroup=sensorBandBlock skipwhite skipempty
"--}

"--{ Directives
" sensor { ... }, one to a file: a body is one thing, and two have no
" meaningful union, so a second block does not merge the way a camera's does.
syn keyword sensorStatement sensor nextgroup=sensorBlock skipwhite skipempty
"--}

"--{ Settings
" What a sensor block holds: its name; the pixels, and either their pitch
" (one number, or two for pixels that are not square) or the frame's size
" for the pitch to follow from; the readout, which a camera's own overrides;
" and the response and the detector.
syn keyword sensorSetting contained name pixels pitch size readout
syn keyword sensorSetting contained readout_direction
      \ nextgroup=sensorReadoutDirection skipwhite skipempty
syn keyword sensorReadoutDirection contained down up left right
syn keyword sensorSetting contained response
      \ nextgroup=sensorResponseBlock skipwhite skipempty
syn keyword sensorSetting contained detector
      \ nextgroup=sensorDetectorBlock skipwhite skipempty

" response { ... }: the bands, `band NAME { <wavelength> <value> ... }` in
" file order; `kind`, what their values are (`relative`, scaled to
" `peak_qe`, or `qe`); the `cfa { row NAME ... }` tile; and
" `rgb NAME NAME NAME`, the bands a develop maps to red, green, and blue.
" The tile and `rgb` may name a band declared below them, and a band's name
" is any identifier but `row`.
syn keyword sensorResponseSetting contained peak_qe
syn keyword sensorResponseSetting contained kind
      \ nextgroup=sensorResponseKind skipwhite skipempty
syn keyword sensorResponseKind contained relative qe
syn keyword sensorResponseSetting contained band
      \ nextgroup=sensorBandName,sensorBandBadName skipwhite skipempty
syn match sensorBandName contained "\<\%(row\>\)\@!\h\w*\>"
      \ nextgroup=sensorBandBlock skipwhite skipempty
syn keyword sensorResponseSetting contained cfa
      \ nextgroup=sensorCFABlock skipwhite skipempty
syn keyword sensorCFARow contained row
syn match sensorCFAName contained display "\<\h\w*\>"
syn keyword sensorResponseSetting contained rgb
      \ nextgroup=sensorRGBName1 skipwhite skipempty
syn match sensorRGBName1 contained "\<\h\w*\>"
      \ nextgroup=sensorRGBName2 skipwhite skipempty
syn match sensorRGBName2 contained "\<\h\w*\>"
      \ nextgroup=sensorRGBName3 skipwhite skipempty
syn match sensorRGBName3 contained "\<\h\w*\>"

" detector { ... }: what the body reads out with, every key one number with
" a generic default, so a block names only what differs.
syn keyword sensorDetectorSetting contained base_iso full_well read_noise
syn keyword sensorDetectorSetting contained dark_current
syn keyword sensorDetectorSetting contained reference_temperature
syn keyword sensorDetectorSetting contained doubling_temperature
syn keyword sensorDetectorSetting contained black_level bits gain max_iso
"--}

"--{ Blocks
syn cluster sensorCommon
      \ contains=sensorComment,sensorString,sensorNumber,sensorBadWord

syn region sensorBlock contained matchgroup=sensorDelim start="{" end="}"
      \ contains=@sensorCommon,sensorSetting

syn region sensorResponseBlock contained matchgroup=sensorDelim
      \ start="{" end="}"
      \ contains=@sensorCommon,sensorResponseSetting

" The knots: numbers only, so a stray word is flagged.
syn region sensorBandBlock contained matchgroup=sensorDelim start="{" end="}"
      \ contains=sensorComment,sensorNumber,sensorBadWord

syn region sensorCFABlock contained matchgroup=sensorDelim start="{" end="}"
      \ contains=sensorComment,sensorCFABad,sensorCFARow,sensorCFAName

syn region sensorDetectorBlock contained matchgroup=sensorDelim
      \ start="{" end="}"
      \ contains=@sensorCommon,sensorDetectorSetting
"--}

" What a word means depends on the blocks around it, and a band's knots can
" run to hundreds of lines, so a sync point found a fixed distance back can
" land inside a band and misread everything after it. A sensor file is small
" enough to parse from the top.
syn sync fromstart

"--{ Highlight links
hi def link sensorTodo             Todo
hi def link sensorComment          Comment

hi def link sensorString           String
hi def link sensorNumber           Number

hi def link sensorStatement        Statement

hi def link sensorSetting          Label
hi def link sensorReadoutDirection Constant
hi def link sensorResponseSetting  Label
hi def link sensorResponseKind     Constant
hi def link sensorBandName         Identifier
hi def link sensorCFARow           Keyword
hi def link sensorCFAName          Identifier
hi def link sensorRGBName1         Identifier
hi def link sensorRGBName2         Identifier
hi def link sensorRGBName3         Identifier
hi def link sensorDetectorSetting  Label

hi def link sensorDelim            Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:sensor_no_error_highlight")
  hi def link sensorBadWord        Error
  hi def link sensorCFABad         Error
  hi def link sensorBandBadName    Error
endif
"--}

let b:current_syntax = "sensor"

let &cpo = s:cpo_save
unlet s:cpo_save
