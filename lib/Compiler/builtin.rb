#!/usr/bin/ruby 

f = File.open 'builtin.h', 'w'
f.write <<STR
#pragma once

namespace smdl::Compiler::builtin {

STR

fnames = ['anno', 'df', 'debug', 'limits', 'math', 'scene', 'state', 'std', 'tex', 'monte_carlo', 'quat', 'rgb', 'specular']

for fname in fnames 
  text = File.read "builtin/#{fname}.smdl"
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

} // namespace smdl::Compiler::builtin
STR

