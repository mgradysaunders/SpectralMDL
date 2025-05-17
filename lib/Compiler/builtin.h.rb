#!/usr/bin/ruby 

f = File.open 'builtin.h', 'w'
f.write <<STR
#pragma once

#include <string_view>

namespace smdl::builtin {

STR

fnames = ['anno', 'api', 'debug', 'df', 'limits', 'math', 'scene', 'state', 'std', 'tex']

for fname in fnames 
  text = `smdl format -c --no-comments builtin/#{fname}.smdl`
  text = File.read "builtin/#{fname}.smdl" unless $?.success?
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

