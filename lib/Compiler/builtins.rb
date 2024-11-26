#!/usr/bin/ruby 

f = File.open 'builtins.h', 'w'
f.write <<STR
#pragma once

namespace smdl::Compiler::builtins {

STR

fnames = ['df', 'debug', 'limits', 'math', 'scene', 'state', 'std', 'tex', 'microfacet', 'monte_carlo', 'rgb', 'specular']

for fname in fnames 
  text = File.read "builtins/#{fname}.smdl"
  text = text.gsub /\/\/.*$/, ''
  text = text.gsub /\n(\s*\n)+/, "\n"
  f.write "static const char *#{fname} = R\"*(#{text})*\";\n\n"
end

f.write <<STR
[[nodiscard]] static const char *get_src(auto name) {
STR
for fname in fnames
  f.write <<STR
  if (name == "#{fname}")
    return #{fname};
STR
end
f.write <<STR
  return nullptr;
}

} // namespace smdl::Compiler::builtins
STR


