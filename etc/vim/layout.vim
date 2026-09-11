" Vim syntax file
" Language:    smdl-toy layout
" Filenames:   *.layout
"
" The scene layout format that `smdl-toy` reads: `asset` and `light`
" declarations, reusable `group` arrangements, `place` and `import`
" statements, and the `material`, `medium`, `sky`, and `haze` directives.
" The viewpoint, the lens, and the render clock are not here: they live in a
" `.camera` file, which `camera.vim` covers, and in the `.lens` that camera
" names, which `lens.vim` covers.
" This file is derived directly from the parser in
" `programs/smdl-toy/Layout/LayoutParser.cc`, so the words it knows inside a
" block are exactly the ones that block accepts, and anything else there is
" flagged the way the parser flags it.
"
" A layout file is identified by its `.layout` extension.
"
" Install (drop-in, single file):
"
"   mkdir -p ~/.vim/syntax ~/.vim/ftdetect
"   cp layout.vim camera.vim lens.vim ~/.vim/syntax/
"   echo 'au BufRead,BufNewFile *.layout setf layout' > ~/.vim/ftdetect/layout.vim
"   echo 'au BufRead,BufNewFile *.camera,*.sensor setf camera' > ~/.vim/ftdetect/camera.vim
"   echo 'au BufRead,BufNewFile *.lens setf lens' > ~/.vim/ftdetect/lens.vim
"
" For Neovim, use ~/.config/nvim/syntax and ~/.config/nvim/ftdetect instead.
"
" Options:
"
"   g:layout_no_error_highlight  Do not highlight a word that is not an
"                                operation of the block it sits in as an error.
"   g:layout_no_buffer_options   Do not `setlocal` any buffer options. (Set
"                                this if you keep them in an ftplugin.)

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

"--{ Buffer options
if !exists("g:layout_no_buffer_options")
  setlocal comments=:#
  setlocal commentstring=#\ %s
  setlocal formatoptions-=t formatoptions+=croql
  setlocal suffixesadd=.layout

  " Braces are the only structure there is to indent, and `#` begins a comment
  " rather than a preprocessor directive, so neither `smartindent` nor
  " `cindent` will do: both snap a leading `#` to column zero.
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
" The order of the sections below therefore goes from the most general item to
" the most specific: the catch-all that flags an unknown word comes first, and
" every name that a `nextgroup` chain reaches is defined after it.

"--{ Comments
syn keyword layoutTodo contained TODO FIXME XXX HACK NOTE BUG

" A comment runs to the end of the line, and also ends the bare word in front
" of it, so `rock#note` is the word `rock` and a comment.
syn match layoutComment display "#.*$" contains=layoutTodo,@Spell
"--}

"--{ Literals
" A quoted string may not span lines and has no escape sequences: the lexer
" takes every character up to the closing quote verbatim.
syn region layoutString display oneline start=+"+ end=+"+

" Numbers are whatever the parser's `std::stof` accepts in full, which in
" practice is a signed decimal with an optional exponent.
syn match layoutNumber display
      \ "\w\@<![-+]\=\%(\d\+\%(\.\d*\)\=\|\.\d\+\)\%([eE][-+]\=\d\+\)\=\w\@!"
"--}

"--{ The catch-all
" A word that is not a directive, and not an operation of the block it sits in,
" is an error in the parser. Defined before everything else so that every
" keyword and every `nextgroup` name below overrides it.
syn match layoutBadWord display "\<\h\w*\>"
"--}

"--{ Declarations
" asset <name> = "<path>" { ... }
" asset <name> = sphere|box|disk|cylinder|cone { ... }
syn keyword layoutStatement asset nextgroup=layoutAssetName skipwhite skipempty
syn match layoutAssetName contained display "\h\w*"
      \ nextgroup=layoutAssetEq skipwhite skipempty
syn match layoutAssetEq contained display "="
      \ nextgroup=layoutAssetPath,layoutShape skipwhite skipempty
syn match layoutAssetPath contained display +"[^"]*"+
      \ nextgroup=layoutAssetBlock skipwhite skipempty

" light <name> = point|spot|rect|disk { ... }
" light <name> = profile "<path>" { ... }
" `light` is both the declaration keyword and a mark (see the marks under
" Operations), told apart by what follows: a declaration is `light <name> =`.
syn match layoutLightStmt display "\<light\>\%(\s\+\h\w*\s*=\)\@="
      \ nextgroup=layoutLightName skipwhite skipempty
syn match layoutLightName contained display "\h\w*"
      \ nextgroup=layoutLightEq skipwhite skipempty
syn match layoutLightEq contained display "="
      \ nextgroup=layoutLightKind,layoutLightProfile skipwhite skipempty
syn match layoutProfilePath contained display +"[^"]*"+
      \ nextgroup=layoutLightBlock skipwhite skipempty

" group <name> { place ... }
syn keyword layoutStatement group nextgroup=layoutGroupName skipwhite skipempty
syn match layoutGroupName contained display "\h\w*"
      \ nextgroup=layoutGroupBlock skipwhite skipempty

" import "<path>" { ... }
syn keyword layoutStatement import nextgroup=layoutImportPath skipwhite skipempty
syn match layoutImportPath contained display +"[^"]*"+
      \ nextgroup=layoutImportBlock skipwhite skipempty

" sky { ... } and haze { ... }, merged per field, last one wins.
syn keyword layoutStatement sky nextgroup=layoutSkyBlock skipwhite skipempty
syn keyword layoutStatement haze nextgroup=layoutHazeBlock skipwhite skipempty

" medium <material>
syn keyword layoutStatement medium nextgroup=layoutMaterialName skipwhite skipempty

" The built-in analytic shapes and the light kinds, which are bare because they
" are vocabulary; a path is quoted because it is a name out of the filesystem.
syn keyword layoutShape contained sphere box disk cylinder cone
      \ nextgroup=layoutAssetBlock skipwhite skipempty
syn keyword layoutLightKind contained point spot rect disk
      \ nextgroup=layoutLightBlock skipwhite skipempty
syn keyword layoutLightProfile contained profile
      \ nextgroup=layoutProfilePath skipwhite skipempty
"--}

"--{ Placements
" place <name> [* "<file>"] [as <id>] { ... }, or with everything on the
" `place` keyword's own line, which is the form a machine writes one instance
" per line in.
syn keyword layoutPlaceStmt place nextgroup=layoutPlaceName skipwhite skipempty
syn match layoutPlaceName contained display "\h\w*"
      \ nextgroup=layoutStar,layoutAs,layoutPlaceBlock skipwhite skipempty
syn match layoutStar contained display "\*"
      \ nextgroup=layoutPlacesPath skipwhite skipempty
syn match layoutPlacesPath contained display +"[^"]*"+
      \ nextgroup=layoutPlaceBlock skipwhite skipempty
syn keyword layoutAs contained as nextgroup=layoutAsName skipwhite skipempty
syn match layoutAsName contained display "\h\w*"
      \ nextgroup=layoutPlaceBlock skipwhite skipempty
"--}

"--{ Materials
" material "<name>" = <material>   the file's alias table, at the top level
" material <material>              shades every slot of an asset or import
" material "<slot>" = <material>   shades one slot of an asset or import
" material <from> = <to>           renames what a place or a variant resolves
"
" This is not `contained`, because it is a top-level directive and an operation
" of the one-line `place` form as well as a block operation.
syn keyword layoutMaterialOp material
      \ nextgroup=layoutMaterialSlot,layoutMaterialName skipwhite skipempty
syn match layoutMaterialSlot contained display +"[^"]*"+
      \ nextgroup=layoutMaterialEq skipwhite skipempty
syn match layoutMaterialName contained display "\h\w*"
      \ nextgroup=layoutMaterialEq skipwhite skipempty
syn match layoutMaterialEq contained display "="
      \ nextgroup=layoutMaterialName skipwhite skipempty
"--}

"--{ Operations
" The transform operations and `variant` are not `contained` for the same
" reason `material` is not: a one-line `place` writes them at the top level of
" the file. The cost is that a stray transform at the top level reads as an
" operation rather than as the error the parser makes of it.
syn keyword layoutTransform
      \ translate scale rotate rotate_x rotate_y rotate_z matrix
syn keyword layoutVariant variant nextgroup=layoutVariantBlock skipwhite skipempty

" motion { at <seconds> ... } on a place: a track of keys at absolute times,
" each holding transform operations only. Top level like `variant`, since a
" one-line place writes it outside any block.
syn keyword layoutMotionOp motion nextgroup=layoutMotionBlock skipwhite skipempty
syn keyword layoutMotionAt contained at

" Asset operations. `radius`, `height`, and `size` belong to a shape,
" `radius_scale` to a `.curves` file, and the rest to a mesh file.
" `animation` takes a clip and its settings (`offset`, `speed`, `once`),
" or `off`.
syn keyword layoutAssetOp contained select recenter subdivide displace tube ribbon
syn keyword layoutAnimationOp contained animation nextgroup=layoutCasterOff skipwhite
syn keyword layoutAssetSetting contained radius height size radius_scale speed once

" offset <seconds> on a place: the seconds the placement adds to the clock
" of what it places. Top level like `motion`, since a one-line place writes
" it outside any block; inside an asset block it is `animation`'s setting.
syn keyword layoutOffsetOp offset

" The caustic caster and light marks: bare on an asset, `caster` or `caster
" off` (`light`, `light off`) on a place or an import. Top level like
" `material`, since a one-line place carries them outside any block. The
" `caustic` mark on an emissive asset or a light is spelled the same way,
" bare, asset level only. The light mark is a match rather than a keyword
" so that the `light` declaration, which is the same word followed by a
" name and `=`, keeps its own group.
syn keyword layoutCasterOp caster nextgroup=layoutCasterOff skipwhite
syn match layoutLightMark display "\<light\>\%(\s\+\h\w*\s*=\)\@!"
      \ nextgroup=layoutCasterOff skipwhite
syn keyword layoutCasterOff contained off
syn keyword layoutCausticOp caustic

" The optional trailing words of `subdivide <level>`, in either order.
syn keyword layoutSubdivMod contained loop linear

" Light settings. Defined after the transform operations so that `scale`, which
" is the profile multiplier here, wins inside a light block. `size` and
" `radius` are the rect's and the disk's extents.
syn keyword layoutLightSetting contained power temperature color angle blend scale caustic size radius

syn keyword layoutSkySetting contained none sun_zenith sun_azimuth visibility
syn keyword layoutSkySetting contained water_vapor scale moon moon_distance
syn keyword layoutSkySetting contained ibl ibl_scale

" The haze settings.
syn keyword layoutHazeSetting contained none visibility scale_height
syn keyword layoutHazeSetting contained base_height albedo angstrom droplet
"--}

"--{ Blocks
" Every block is reached by the `nextgroup` chain of the statement that
" introduces it, so what each one contains is exactly what the parser accepts
" there and nothing else.
syn cluster layoutCommon
      \ contains=layoutComment,layoutString,layoutNumber,layoutBadWord

syn region layoutAssetBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutAssetOp,layoutAnimationOp,layoutAssetSetting,
      \ layoutSubdivMod,layoutTransform,layoutMaterialOp,layoutCasterOp,layoutOffsetOp

syn region layoutLightBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutLightSetting,layoutTransform

" A group holds `place` statements only, so that it stays an arrangement and
" never becomes a scope.
syn region layoutGroupBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutPlaceStmt,layoutTransform,layoutMaterialOp,
      \ layoutVariant,layoutCasterOp,layoutMotionOp,layoutOffsetOp

syn region layoutPlaceBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutTransform,layoutMaterialOp,layoutVariant,
      \ layoutCasterOp,layoutMotionOp,layoutOffsetOp

" A variant holds `material <from> = <to>` overrides and nothing else.
syn region layoutVariantBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutMaterialOp

" A motion block holds `at <seconds>` keys and transform operations.
syn region layoutMotionBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutMotionAt,layoutTransform

syn region layoutImportBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutTransform,layoutMaterialOp,layoutCasterOp

syn region layoutSkyBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutSkySetting

syn region layoutHazeBlock contained matchgroup=layoutDelim start="{" end="}"
      \ contains=@layoutCommon,layoutHazeSetting
"--}

" Blocks nest at most three deep (group, place, variant) and are short, so
" looking a couple of screens back is always enough to place one.
syn sync minlines=200

"--{ Highlight links
hi def link layoutTodo            Todo
hi def link layoutComment         Comment

hi def link layoutString          String
hi def link layoutAssetPath       String
hi def link layoutProfilePath     String
hi def link layoutImportPath      String
hi def link layoutPlacesPath      String
hi def link layoutMaterialSlot    String
hi def link layoutNumber          Number

hi def link layoutStatement       Statement
hi def link layoutPlaceStmt       Statement
hi def link layoutMaterialOp      Statement

hi def link layoutTransform       Keyword
hi def link layoutAssetOp         Keyword
hi def link layoutCasterOp        Keyword
hi def link layoutLightMark       Keyword
hi def link layoutLightStmt       Statement
hi def link layoutCausticOp       Keyword
hi def link layoutCasterOff       Keyword
hi def link layoutVariant         Keyword
hi def link layoutMotionOp        Keyword
hi def link layoutMotionAt        Keyword
hi def link layoutAnimationOp     Keyword
hi def link layoutOffsetOp        Keyword
hi def link layoutAs              Keyword

hi def link layoutAssetSetting    Label
hi def link layoutLightSetting    Label
hi def link layoutSkySetting      Label
hi def link layoutHazeSetting     Label

hi def link layoutShape           Constant
hi def link layoutLightKind       Constant
hi def link layoutLightProfile    Constant
hi def link layoutSubdivMod       Constant

hi def link layoutAssetName       Identifier
hi def link layoutGroupName       Identifier
hi def link layoutLightName       Identifier
hi def link layoutPlaceName       Identifier
hi def link layoutAsName          Identifier
hi def link layoutMaterialName    Type

hi def link layoutAssetEq         Operator
hi def link layoutLightEq         Operator
hi def link layoutMaterialEq      Operator
hi def link layoutStar            Operator
hi def link layoutDelim           Delimiter

" Left unlinked, and so uncolored, when the option turns it off.
if !exists("g:layout_no_error_highlight")
  hi def link layoutBadWord       Error
endif
"--}

let b:current_syntax = "layout"

let &cpo = s:cpo_save
unlet s:cpo_save
