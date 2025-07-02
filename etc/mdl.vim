if exists("b:current_syntax") && b:current_syntax == "mdl"
  finish
endif

" Smart formatting
set smartindent
set smarttab
set comments=s1:/*,mb:*,ex:*/,:///,://
set formatoptions=tcroql

" Literals
syn keyword mdlBool     true false
syn match   mdlInt      display "\<\(0\|[1-9]\d*\)"
syn match   mdlInt      display "\<0\o\+"
syn match   mdlInt      display "\<0[xX]\x\+"
syn match   mdlInt      display "\<0[bB][01]\+"
syn match   mdlFloat    display "\<\d\+\.\([eE][+-]\=\d\+\)\=\(f\|F\|d\|D\)\="
syn match   mdlFloat    display "\<\.\d\+\([eE][+-]\=\d\+\)\=\(f\|F\|d\|D\)\="
syn match   mdlFloat    display "\<\d\+[eE][+-]\=\d\+\(f\|F\|d\|D\)\="
syn match   mdlFloat    display "\<\d\+\.\d\+\([eE][+-]\=\d\+\)\=\(f\|F\|d\|D\)\="
syn region  mdlString   start="\"" skip="\\\"" end="\"" keepend
syn keyword mdlConstant PI TWO_PI HALF_PI FLOAT_MIN FLOAT_MAX DOUBLE_MIN DOUBLE_MAX INT_MIN INT_MAX
syn keyword mdlConstant intensity_radiant_exitance intensity_power
syn keyword mdlConstant gamma_default gamma_linear gamma_srgb
syn keyword mdlConstant scatter_none scatter_reflect scatter_transmit scatter_reflect_transmit
syn keyword mdlConstant coordinate_internal coordinate_object coordinate_world
syn keyword mdlConstant wrap_clamp wrap_repeat wrap_mirrored_repeat wrap_clip
hi def link mdlBool     Boolean
hi def link mdlInt      Number
hi def link mdlFloat    Number
hi def link mdlString   String
hi def link mdlConstant Constant

" Operator symbols
syn match mdlOperator "[.,=+\-*/%!~&|^<>?:]" display
hi def link mdlOperator Operator

" Comments and annotations
syn keyword mdlTodo         contained TODO FIXME XXX NOTE
syn region mdlLineComment   start="//" skip="\\$" end="$" keepend contains=mdlTodo
syn region mdlComment       start="/\*" end="\*/" extend contains=mdlTodo matchgroup=mdlCommentStart 
syn region mdlAnnotation    start="\[\[" end="\]\]" keepend contains=mdlBool,mdlInt,mdlFloat,mdlString
syn region mdlAttributes    start="@(" end=")" extend keepend
syn region mdlSizeDeferred  start="\[<" end=">\]" extend keepend
hi def link mdlTodo         Todo
hi def link mdlLineComment  mdlComment
hi def link mdlCommentStart mdlComment
hi def link mdlComment      Comment
hi def link mdlAnnotation   Comment
hi def link mdlAttributes   Comment
hi def link mdlSizeDeferred Constant

" Labels. This is context independent and can trigger both false positives and false negatives!
"   foo(arg: bar + baz);  // arg IS highlighted and SHOULD be
"   foo(arg : bar + baz); // arg IS NOT highlighted but SHOULD be
"   foo(hmm ? bar : baz); // bar IS NOT highlighted and SHOULD NOT be
"   foo(hmm ? bar: baz);  // bar IS highlighted but SHOULD NOT be
syn match mdlLabel "\<[a-zA-Z]\w*:\ze[^:]"me=e-1 display
hi def link mdlLabel Label

" Identifiers
syn match mdlIdentifier    "[a-zA-Z_][a-zA-Z0-9_]*" display contained
hi def link mdlIdentifier  Identifier

" Keywords
syn keyword mdlModule      mdl module export import using
syn keyword mdlRepeat      for while do
syn keyword mdlConditional if else switch case default
syn keyword mdlStatement   return break continue let in typedef cast
syn keyword mdlQualifier   uniform varying const static inline
hi def link mdlModule      Macro
hi def link mdlRepeat      Repeat
hi def link mdlConditional Conditional
hi def link mdlStatement   Statement
hi def link mdlQualifier   StorageClass

" Types
syn keyword mdlStructure struct nextgroup=mdlIdentifier skipwhite skipempty
syn keyword mdlStructure enum   nextgroup=mdlIdentifier skipwhite skipempty
syn keyword mdlType auto
syn keyword mdlType bool bool2 bool3 bool4
syn keyword mdlType int int2 int3 int4
syn keyword mdlType float float2 float3 float4 float2x2 float2x3 float2x4 float3x2 float3x3 float3x4 float4x2 float4x3 float4x4
syn keyword mdlType double double2 double3 double4 double2x2 double2x3 double2x4 double3x2 double3x3 double3x4 double4x2 double4x3 double4x4
syn keyword mdlType string color
syn keyword mdlType texture_2d texture_3d texture_cube texture_ptex bsdf_measurement light_profile
syn keyword mdlType material material_surface material_emission material_volume material_geometry bsdf edf vdf hair_bsdf intensity_mode
hi def link mdlStructure Structure
hi def link mdlType      Type

syn keyword smdlStructure   tag nextgroup=mdlIdentifier skipwhite skipempty
syn match   smdlOperator    "[$]" display
syn match   smdlSpecialName "[$][a-zA-Z]\w*" display
syn match   smdlIntrinsic   "[#][a-zA-Z]\w*" display
syn keyword smdlModule      namespace exec unit_test
syn keyword smdlConstant    none
syn keyword smdlStatement   defer preserve return_from unreachable visit finalize
syn keyword smdlType        void i8 i16 i32 i64 f16 f32 f64
hi def link smdlModule      Macro
hi def link smdlSpecialName Preproc
hi def link smdlIntrinsic   Function
hi def link smdlConstant    Constant
hi def link smdlOperator    Operator
hi def link smdlStatement   Statement
hi def link smdlStructure   Structure
hi def link smdlType        Type

if !exists("b:current_syntax")
  let b:current_syntax = "mdl"
endif
