" Vim syntax file
" Language:    MDL / SMDL (the SpectralMDL dialect of NVIDIA's Material
"              Definition Language)
" Filenames:   *.mdl, *.smdl
"
" This file is derived directly from the SpectralMDL parser, so it recognizes
" both plain MDL and the SMDL syntax extensions that are enabled by the `#smdl`
" pragma on the first line of a file. Groups named `mdl*` are core MDL; groups
" named `smdl*` are SpectralMDL extensions, so a color scheme may distinguish
" the two if it wants to.
"
" Install (drop-in, single file):
"
"   mkdir -p ~/.vim/syntax ~/.vim/ftdetect
"   cp etc/mdl.vim ~/.vim/syntax/mdl.vim
"   echo 'au BufRead,BufNewFile *.mdl,*.smdl setf mdl' > ~/.vim/ftdetect/mdl.vim
"
" Options:
"
"   g:mdl_no_error_highlight     Do not highlight unknown intrinsics, unknown
"                                declaration attributes, or malformed string
"                                escape sequences as errors.
"   g:mdl_no_function_highlight  Do not highlight `name(` as a function.
"   g:mdl_no_buffer_options      Do not `setlocal` any buffer options. (Set
"                                this if you keep them in an ftplugin.)

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

"--{ Buffer options
if !exists("g:mdl_no_buffer_options")
  setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,:///,://
  setlocal commentstring=//\ %s
  setlocal formatoptions-=t formatoptions+=croql
  setlocal suffixesadd=.smdl,.mdl

  " MDL is close enough to C to reuse `cindent()`, with two adjustments:
  "   - `#` begins an intrinsic like `#assert(...)`, not a preprocessor
  "     directive, so it must not be snapped to column zero. (This is why
  "     `smartindent` and plain `cindent` are both wrong here.)
  "   - A declaration attribute such as `@(pure)` on a line by itself is
  "     neither terminated by `;` nor opens a block, so `cindent()` would
  "     treat the whole declaration after it as a continuation line.
  " Note that `cinoptions=N-s` keeps `namespace ... { ... }` bodies flush, but
  " cindent only recognizes `namespace` at the start of a line, so the body of
  " an `export namespace ... {` still gets indented one level.
  if !exists("*MdlIndent")
    function MdlIndent() abort
      let l:prev = prevnonblank(v:lnum - 1)
      if l:prev > 0 && getline(l:prev) =~# '^\s*@\s*(.*)\s*$'
        return indent(l:prev)
      endif
      return cindent(v:lnum)
    endfunction
  endif
  setlocal nosmartindent nolisp
  setlocal autoindent
  setlocal cinoptions=l1,:0,g0,t0,(0,W1s,m1,j1,J1,N-s
  setlocal indentexpr=MdlIndent()
  setlocal indentkeys=0{,0},0),0],:,!^F,o,O,e
endif
"--}

" NOTE: When two `syn match`/`syn region` items can start at the same position,
" Vim gives priority to the one defined LAST. The order of the sections below
" therefore goes from the most general item to the most specific, so that (for
" example) `//` is a comment and not a division operator.

"--{ Operators
" One alternation, ordered longest-first, so that multi-character operators are
" never shadowed by their prefixes. Note that `\%d124` is a literal `|`; an
" unescaped one would terminate the `:syntax` command, and that `~` must be
" escaped because it is magic in a search pattern.
syn match mdlOperator display "\%(>>>=\|<<=\|>>=\|>>>\|\.\.\.\|\~==\|\~!=\|&&\|==\|!=\|<=\|>=\|<<\|>>\|++\|--\|+=\|-=\|\*=\|/=\|%=\|&=\|\^=\|:=\|<:\|::\|[-+*/%!~&^<>?:=.,]\)"
syn match mdlOperator display "\%(\%d124\%d124\|\%d124=\|\%d124\)"

" The `[EPSILON]` that must follow the approximate comparisons `~==` and `~!=`
" is an ordinary expression, so it needs no special treatment here.
"--}

"--{ Comments
syn keyword mdlTodo contained TODO FIXME XXX HACK NOTE BUG

" `// smdl format off` / `// smdl format on` are honored by `smdl format`.
syn match mdlFormatDirective contained display "\<smdl\s\+format\s\+\%(on\|off\)\>"

" Doxygen-style tags inside documentation comments.
syn match mdlDocTag contained display "[\\@]\%(\a\|-\)\+"

syn region mdlLineComment start="//" end="$" keepend
      \ contains=mdlTodo,mdlFormatDirective,@Spell
syn region mdlComment start="/\*" end="\*/" extend
      \ contains=mdlTodo,mdlFormatDirective,@Spell

" Documentation comments are defined after the ordinary ones so that they win
" the tie at the same starting position.
syn region mdlDocLineComment start="///" end="$" keepend
      \ contains=mdlTodo,mdlDocTag,@Spell
syn region mdlDocComment start="/\*\*\%(/\)\@!" end="\*/" extend
      \ contains=mdlTodo,mdlDocTag,@Spell
"--}

"--{ Literals
syn keyword mdlBoolean true false

" Escape sequences. The broad (error) match is defined first so that the
" well-formed sequences defined after it take priority. The parser accepts
" `\<char>` verbatim for any character it does not recognize, but it *requires*
" exactly 3 octal digits, 2 hex digits after `\x`, 4 after `\u`, and 8 after
" `\U`.
syn match mdlStringEscapeError contained display "\\[0-7xuU]"
syn match mdlStringEscape contained display
      \ "\\\%([abfnrtv]\|[0-7]\{3}\|x\x\{2}\|u\x\{4}\|U\x\{8}\)"
syn match mdlStringEscape contained display "\\[^0-7xuU]"

" Literal strings may not span lines.
syn region mdlString start=+"+ skip=+\\.+ end=+"+ oneline
      \ contains=mdlStringEscape,mdlStringEscapeError,@Spell

" Integer literals. Every radix admits `'` digit separators.
syn match mdlNumber display "\<0[xX]\x\%(['[:xdigit:]]*\x\)\=\>"
syn match mdlNumber display "\<0[bB][01]\%(['01]*[01]\)\=\>"
syn match mdlNumber display "\<0\o\%(['0-7]*\o\)\=\>"
syn match mdlNumber display "\<\%(0\|[1-9]\%(['0-9]*\d\)\=\)\>"

" Floating point literals, with the optional imaginary suffix `j` (an SMDL
" extension) and the optional precision suffix `f`, `F`, `d`, or `D`. Note that
" the parser requires a leading digit, so `.5` is *not* a literal.
syn match mdlFloat display "\<\d\%(['0-9]*\d\)\=\.\%(\d\%(['0-9]*\d\)\=\)\=\%([eE][-+]\=\d\%(['0-9]*\d\)\=\)\=j\=[dDfF]\=\w\@!"
syn match mdlFloat display "\<\d\%(['0-9]*\d\)\=[eE][-+]\=\d\%(['0-9]*\d\)\=j\=[dDfF]\=\w\@!"
syn match mdlFloat display "\<\d\%(['0-9]*\d\)\=\%(j[dDfF]\=\|[dDfF]\)\w\@!"
"--}

"--{ Keywords
syn keyword mdlConditional if else switch case default
syn keyword mdlRepeat for while do
syn keyword mdlStatement return break continue
syn keyword mdlKeyword let in cast
syn keyword mdlStorageClass const uniform varying
syn keyword mdlInclude module import using export package
syn keyword mdlStructure typedef

" The `mdl X.Y;` version declaration at the top of a file. `mdl` is not a
" reserved word, so it is only recognized in that position.
syn match mdlInclude display "^\s*mdl\>\ze\s\+\d"

" SMDL extensions.
syn keyword smdlStatement return_from unreachable defer preserve visit finalize
syn keyword smdlStorageClass static inline
syn keyword smdlKeyword exec unit_test

" Declarations that name the thing they declare.
syn keyword mdlStructure struct enum annotation
      \ nextgroup=mdlDeclName skipwhite skipempty
syn keyword smdlStructure tag
      \ nextgroup=mdlDeclName skipwhite skipempty
syn keyword smdlKeyword namespace
      \ nextgroup=mdlDeclName skipwhite skipempty
syn match mdlDeclName contained display "\h\w*\%(\s*::\s*\h\w*\)*"
"--}

"--{ Types
syn keyword mdlType auto void bool bool2 bool3 bool4
syn keyword mdlType int int2 int3 int4
syn keyword mdlType float float2 float3 float4
syn keyword mdlType float2x2 float2x3 float2x4
syn keyword mdlType float3x2 float3x3 float3x4
syn keyword mdlType float4x2 float4x3 float4x4
syn keyword mdlType double double2 double3 double4
syn keyword mdlType double2x2 double2x3 double2x4
syn keyword mdlType double3x2 double3x3 double3x4
syn keyword mdlType double4x2 double4x3 double4x4
syn keyword mdlType string color

" Types provided by the builtin `api` module, which the compiler installs as
" global keywords.
syn keyword mdlType material material_surface material_emission
syn keyword mdlType material_volume material_geometry
syn keyword mdlType bsdf edf vdf hair_bsdf intensity_mode
syn keyword mdlType texture_2d texture_3d texture_cube texture_ptex
syn keyword mdlType bsdf_measurement light_profile spectral_curve
syn keyword smdlType complex

" Enum types from the builtin standard library modules.
syn keyword mdlType scatter_mode coordinate_space gamma_mode wrap_mode

" C interoperability types. These are SMDL extensions.
syn keyword smdlType char long size_t ptrdiff_t
syn keyword smdlType intptr_t intmax_t int8_t int16_t int32_t int64_t

" Inferred array size, e.g. `auto[<N>] values`.
syn match smdlSizeName display "\[\s*<\s*\h\w*\s*>\s*\]"
"--}

"--{ Constants
" Constants exported by the builtin standard library modules.
syn keyword mdlConstant PI TWO_PI HALF_PI
syn keyword mdlConstant INT_MIN INT_MAX FLOAT_MIN FLOAT_MAX DOUBLE_MIN DOUBLE_MAX
syn keyword mdlConstant intensity_radiant_exitance intensity_power
syn keyword mdlConstant scatter_none scatter_reflect
syn keyword mdlConstant scatter_transmit scatter_reflect_transmit
syn keyword mdlConstant coordinate_internal coordinate_object coordinate_world
syn keyword mdlConstant gamma_default gamma_linear gamma_srgb
syn keyword mdlConstant wrap_clamp wrap_repeat wrap_mirrored_repeat wrap_clip

" The null pointer / empty optional union.
syn keyword smdlConstant none

" Compiler-provided `$` values, e.g. `$PI`, `$FLOAT_MAX`, `$stdout`.
syn match smdlDollarName display "\$\h\w*"

" The implicit per-shading-point state, which is passed to every non-`pure`
" function. Defined after `smdlDollarName` so that it wins.
syn match smdlState display "\$state\>"

" `$(...)` forces compile-time evaluation of the parenthesized expression. Only
" the sigil is highlighted; the expression inside is ordinary code.
syn match smdlComptime display "\$\ze\s*("
"--}

"--{ Intrinsics
" The generic match is defined first so that the recognized names defined after
" it take priority.
syn match smdlIntrinsicUnknown display "#\h\w*"
syn match smdlIntrinsic display "#\%(abs\|alignof\|all\|allocate\|any\|assert\|atan2\|albedo_lut\)\>"
syn match smdlIntrinsic display "#\%(bitcast\|bitreverse\|breakpoint\|bump\|bump_allocate\)\>"
syn match smdlIntrinsic display "#\%(ctlz\|ctpop\|cttz\|conj\|imag\|norm\|real\)\>"
syn match smdlIntrinsic display "#\%(floor\|ceil\|trunc\|round\|sqrt\|pow\|sign\|select\)\>"
syn match smdlIntrinsic display "#\%(sin\|cos\|tan\|asin\|acos\|atan\|sinh\|cosh\|tanh\)\>"
syn match smdlIntrinsic display "#\%(exp\|exp2\|exp10\|log\|log2\|log10\)\>"
syn match smdlIntrinsic display "#\%(sum\|prod\|min\|max\|min_value\|max_value\)\>"
syn match smdlIntrinsic display "#\%(num\|num_rows\|num_cols\|sizeof\|transpose\)\>"
syn match smdlIntrinsic display "#\%(free\|memcpy\|memset\|isfpclass\|rotl\|rotr\)\>"
syn match smdlIntrinsic display "#\%(print\|println\|fprint\|fprintln\|panic\)\>"
syn match smdlIntrinsic display "#\%(typeof\|typename\|type_int\|type_float\|type_vector\|type_matrix\)\>"
syn match smdlIntrinsic display "#\%(unpack_float4\|unsigned_to_fp\|tabulate_albedo\)\>"
syn match smdlIntrinsic display "#is_\%(array\|enum\|pointer\|struct\|tag\|union\|void\|default\|optional_union\)\>"
syn match smdlIntrinsic display "#is_arithmetic\%(_integral\|_floating_point\|_scalar\|_vector\|_matrix\)\=\>"
syn match smdlIntrinsic display "#load_\%(texture_2d\|texture_3d\|texture_cube\|texture_ptex\)\>"
syn match smdlIntrinsic display "#load_\%(bsdf_measurement\|light_profile\|spectral_curve\)\>"

" The `#smdl` pragma opts a file into the SMDL syntax extensions. It must be
" the very first token in the file.
syn match smdlPragma display "#smdl\>"
"--}

"--{ Annotations, attributes, named arguments
" Annotation blocks `[[ ... ]]` are metadata. They are dimmed like comments,
" but the annotation names and literals inside them stay legible.
syn match mdlAnnotationName contained display
      \ "\h\w*\%(\s*::\s*\h\w*\)*\ze\s*("
syn region mdlAnnotationBlock matchgroup=mdlAnnotationDelim
      \ start="\[\[" end="\]\]" extend keepend
      \ contains=mdlAnnotationName,mdlString,mdlNumber,mdlFloat,mdlBoolean,
      \ mdlConstant,mdlOperator,mdlLineComment,mdlComment,smdlDollarName,
      \ mdlNamedArg

" Declaration attributes `@(...)`. Anything that is not a recognized attribute
" is a hard error in the parser.
syn keyword smdlAttribute contained
      \ alwaysinline cold fastmath foreign hot macro
      \ noinline optnone optsize pure visible
syn match smdlAttributeError contained display "\h\w*"
syn region smdlAttributeBlock matchgroup=smdlAttributeDelim
      \ start="@\s*(" end=")" extend keepend oneline
      \ contains=smdlAttribute,smdlAttributeError

" Named arguments, e.g. `f(tint: color(1))`. A named argument can only begin an
" argument, so requiring `(`, `,`, or a line start in front of it keeps the
" `then` clause of a conditional expression from being mistaken for one.
syn match mdlNamedArg display
      \ "\%(\%(^\|[(,]\)\s*\)\@60<=\h\w*\ze\s*:[:=]\@!"
"--}

"--{ Functions
if !exists("g:mdl_no_function_highlight")
  syn match mdlFunction display "\<\h\w*\ze\s*("
endif
"--}

syn sync ccomment mdlComment minlines=25

"--{ Highlight links
hi def link mdlTodo             Todo
hi def link mdlFormatDirective  PreProc
hi def link mdlDocTag           Special
hi def link mdlLineComment      Comment
hi def link mdlComment          Comment
hi def link mdlDocLineComment   SpecialComment
hi def link mdlDocComment       SpecialComment

hi def link mdlBoolean          Boolean
hi def link mdlNumber           Number
hi def link mdlFloat            Float
hi def link mdlString           String
hi def link mdlStringEscape     SpecialChar

hi def link mdlOperator         Operator

hi def link mdlConditional      Conditional
hi def link mdlRepeat           Repeat
hi def link mdlStatement        Statement
hi def link mdlKeyword          Keyword
hi def link mdlStorageClass     StorageClass
hi def link mdlInclude          Include
hi def link mdlStructure        Structure
hi def link mdlDeclName         Identifier
hi def link mdlType             Type
hi def link mdlConstant         Constant
hi def link mdlFunction         Function
hi def link mdlNamedArg         Label

hi def link mdlAnnotationBlock  Comment
hi def link mdlAnnotationDelim  Comment
hi def link mdlAnnotationName   PreProc

hi def link smdlStatement       Statement
hi def link smdlStorageClass    StorageClass
hi def link smdlKeyword         Keyword
hi def link smdlStructure       Structure
hi def link smdlType            Type
hi def link smdlSizeName        Special
hi def link smdlConstant        Constant
hi def link smdlDollarName      Constant
hi def link smdlState           Identifier
hi def link smdlComptime        PreProc
hi def link smdlIntrinsic       Special
hi def link smdlPragma          PreProc
hi def link smdlAttribute       StorageClass
hi def link smdlAttributeDelim  PreProc

if exists("g:mdl_no_error_highlight")
  hi def link mdlStringEscapeError  mdlStringEscape
  hi def link smdlIntrinsicUnknown  smdlIntrinsic
  hi def link smdlAttributeError    smdlAttribute
else
  hi def link mdlStringEscapeError  Error
  hi def link smdlIntrinsicUnknown  Error
  hi def link smdlAttributeError    Error
endif
"--}

let b:current_syntax = "mdl"

let &cpo = s:cpo_save
unlet s:cpo_save
