#!/usr/bin/ruby 

f = File.open 'Builtin.h', 'w'
f.write <<STR
#pragma once

#include <array>
#include <string_view>

namespace smdl::builtin {

STR

# Each entry is the path under 'Builtin/' without the '.smdl' extension.
# The lookup key replaces '/' with '::' (so 'models/prospect' is addressed
# as '::models::prospect'), and the generated variable replaces '/' with '_'.
fnames = ['anno', 'api', 'debug', 'df', 'limits', 'math', 'scene', 'state', 'std', 'tex', 'extras/io', 'extras/pcg32', 'models/prospect', 'models/marmit']
for fname in fnames
  text = `smdl format -c --no-comments Builtin/#{fname}.smdl`
  text = File.read "Builtin/#{fname}.smdl" unless $?.success?
  f.write "static const char *const #{fname.gsub('/', '_')} = R\"*(#{text})*\";\n\n"
end
f.write <<STR
[[nodiscard]] static const char *get_source_code(std::string_view name) {
STR
for fname in fnames
  f.write <<STR
  if (name == "#{fname.gsub('/', '::')}")
    return #{fname.gsub('/', '_')};
STR
end
f.write <<STR
  return nullptr;
}
STR

fnames = ['diffuse_reflection_bsdf', 'microfacet_ggx_smith_bsdf', 'microfacet_beckmann_smith_bsdf', 'sheen_bsdf', 'simple_glossy_bsdf', 'ward_geisler_moroder_bsdf']
for fname in fnames
  f.write "#include \"Builtin/Albedo/#{fname}.inl\"\n"
end
f.write <<STR
[[nodiscard]] static const AlbedoLUT *get_albedo(std::string_view name) {
STR
for fname in fnames
  f.write <<STR
  if (name == "#{fname}")
    return &#{fname};
STR
end
f.write <<STR
  return nullptr;
}

} // namespace smdl::builtin
STR

