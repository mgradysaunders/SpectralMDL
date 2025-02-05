#!/usr/bin/ruby 

f = File.open 'builtin.h', 'w'
f.write <<STR
#pragma once

#include <string_view>

namespace smdl::builtin {

STR

fnames = ['anno', 'api', 'debug', 'df', 'limits', 'math', 'monte_carlo', 'scene', 'specular', 'state', 'std', 'tex']

for fname in fnames 
  text = File.read "builtin/#{fname}.smdl"
  #text = text.gsub /\/\/.*$/, ''
  #text = text.gsub /\n(\s*\n)+/, "\n"
  f.write "static const char *#{fname} = R\"*(#{text})*\";\n\n"
end

f.write <<STR
[[nodiscard]] static const char *get_source_code(std::string_view name) {
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

} // namespace smdl::builtin
STR

