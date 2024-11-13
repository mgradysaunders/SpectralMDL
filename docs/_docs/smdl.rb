#!/usr/bin/env ruby
require 'rouge'
require 'minify_html'

module Rouge
  module Lexers
    class SMDL < RegexLexer
      tag 'smdl'
      filenames '*.mdl', '*.smdl'
      mimetypes 'text/x-chdr', 'text/x-csrc'
      title "SMDL"
      desc "The MDL programming language with SMDL extensions"
      ws = %r((?:\s|//.*?\n|/[*].*?[*]/)+)
      id = /[a-zA-Z_][a-zA-Z0-9_]*/
      def self.keywords
        @keywords ||= Set.new %w(
          break case const continue default defer do else enum export
          for if import in inline let return return_from static struct 
          switch tag typedef unit_test using visit while
        )
      end

      def self.keywords_type
        @keywords_type ||= Set.new %w(
          auto bool int float double void
          bool2 bool3 bool4
          int2 int3 int4
          float2 float2x2 float2x3 float2x4
          float3 float3x2 float3x3 float3x4
          float4 float4x2 float4x3 float4x4
          double2 double2x2 double2x3 double2x4
          double3 double3x2 double3x3 double3x4
          double4 double4x2 double4x3 double4x4
          image_t
          texture_2d
          texture_3d
          texture_cube
          texture_ptex
          state_t
          string
          source_location_t
        )
      end

      def self.reserved
        @reserved ||= []
      end

      def self.builtins
        @builtins ||= []
      end

      start { push :bol }

      state :expr_bol do
        mixin :inline_whitespace
        rule(//) { pop! }
      end

      # :expr_bol is the same as :bol but without labels, since
      # labels can only appear at the beginning of a statement.
      state :bol do
        rule %r/#{id}:(?!:)/, Name::Label
        mixin :expr_bol
      end

      state :inline_whitespace do
        rule %r/[ \t\r]+/, Text
        rule %r/\\\n/, Text # line continuation
        rule %r(/(\\\n)?[*].*?[*](\\\n)?/)m, Comment::Multiline
      end

      state :whitespace do
        rule %r/\n+/m, Text, :bol
        rule %r(//(\\.|.)*?$), Comment::Single, :bol
        mixin :inline_whitespace
      end

      state :expr_whitespace do
        rule %r/\n+/m, Text, :expr_bol
        mixin :whitespace
      end

      state :statements do
        mixin :whitespace
        rule %r/"/, Str, :string
        rule %r((\d+[.]\d*|[.]?\d+)(e[+-]?\d+)?[fd]*)i, Num::Float
        rule %r(\d+e[+-]?\d+[fd]*)i, Num::Float
        rule %r/0x[0-9a-f]+/i, Num::Hex
        rule %r/0b[0-1]+/i, Num::Hex
        rule %r/0[0-7]+/i, Num::Oct
        rule %r/\d+/i, Num::Integer
        #rule %r(\*/), Error
        rule %r([$~!%^&*+=\|?:<>/-]), Operator
        rule %r/[@]\([\w\s]+\)/, Operator
        rule %r/[()\[\],.;]/, Punctuation
        rule %r/\bcase\b/, Keyword, :case
        rule %r/(?:null|true|false)/, Num::Integer
        rule %r/[$]\w+\b/, Name::Builtin
        rule %r/[#]\w+\b/, Keyword::Reserved
        rule id do |m|
          name = m[0]
          if self.class.keywords.include? name
            token Keyword
          elsif self.class.keywords_type.include? name
            token Keyword::Type
          elsif self.class.reserved.include? name
            token Keyword::Reserved
          elsif self.class.builtins.include? name
            token Name::Builtin
          else
            token Name
          end
        end
      end

      state :case do
        rule %r/:/, Punctuation, :pop!
        mixin :statements
      end

      state :root do
        mixin :expr_whitespace
        rule %r(
          ([\w*\s]+?[\s*]) # return arguments
          (#{id})          # function name
          (\s*\([^;]*?\))  # signature
          (#{ws}?)({|;)    # open brace or semicolon
        )mx do |m|
          # TODO: do this better.
          recurse m[1]
          token Name::Function, m[2]
          recurse m[3]
          recurse m[4]
          token Punctuation, m[5]
          if m[5] == ?{
            push :function
          end
        end
        rule %r/\{/, Punctuation, :function
        mixin :statements
      end

      state :function do
        mixin :whitespace
        mixin :statements
        rule %r/;/, Punctuation
        rule %r/{/, Punctuation, :function
        rule %r/}/, Punctuation, :pop!
      end

      state :string do
        rule %r/"/, Str, :pop!
        rule %r/\\([\\abfnrtv"']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})/, Str::Escape
        rule %r/[^\\"\n]+/, Str
        rule %r/\\\n/, Str
        rule %r/\\/, Str # stray backslash
      end

      state :macro do
        mixin :include
        rule %r([^/\n\\]+), Comment::Preproc
        rule %r/\\./m, Comment::Preproc
        mixin :inline_whitespace
        rule %r(/), Comment::Preproc
        # NB: pop! goes back to :bol
        rule %r/\n/, Comment::Preproc, :pop!
      end

      state :include do
        rule %r/(include)(\s*)(<[^>]+>)([^\n]*)/ do
          groups Comment::Preproc, Text, Comment::PreprocFile, Comment::Single
        end
        rule %r/(include)(\s*)("[^"]+")([^\n]*)/ do
          groups Comment::Preproc, Text, Comment::PreprocFile, Comment::Single
        end
      end

      state :if_0 do
        # NB: no \b here, to cover #ifdef and #ifndef
        rule %r/^\s*#if/, Comment, :if_0
        rule %r/^\s*#\s*el(?:se|if)/, Comment, :pop!
        rule %r/^\s*#\s*endif\b.*?(?<!\\)\n/m, Comment, :pop!
        rule %r/.*?\n/, Comment
      end
    end
  end
end

class DocPostprocessor < Asciidoctor::Extensions::Postprocessor
  def process document, output
    if document.basebackend? 'html'
      output.gsub! /<div id="toctitle">.*?<\/div>/m, '<div id="toctitle">SpectralMDL <a class="link" href="index.html"><i class="fa fa-home"></i></a></div>'
      output.gsub! /<h2 id="(.*?)">(.*?)<\/h2>/m, '<h2 id="\1">\2 <a class="link" href="#\1"><i class="fa fa-anchor"></i></a></h2>'
      output.gsub! /<h3 id="(.*?)">(.*?)<\/h3>/m, '<h3 id="\1">\2 <a class="link" href="#\1"><i class="fa fa-anchor"></i></a></h3>'
      output.gsub! /<h4 id="(.*?)">(.*?)<\/h4>/m, '<h4 id="\1">\2 <a class="link" href="#\1"><i class="fa fa-anchor"></i></a></h4>'
      output.gsub! /<div id="footer-text">(.*?)<\/div>/m, '<div id="footer-text">\1</div>'
      output = minify_html(output, {:keep_spaces_between_attributes => true, :minify_js => true, :minify_css => true})
    end
    output
  end
end


class ExtGroup < Asciidoctor::Extensions::Group
  def activate registry
    registry.postprocessor DocPostprocessor
  end
end

Asciidoctor::Extensions.register ExtGroup

