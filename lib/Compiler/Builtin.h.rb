#!/usr/bin/ruby

require 'zlib'

f = File.open 'Builtin.h', 'w'
f.write <<STR
#pragma once

#include <array>
#include <cstddef>
#include <string_view>

namespace smdl::builtin {

/// The zlib-deflated source code of a builtin module, decompressed
/// with miniz in `Context.cc` when the module is first loaded.
struct CompressedSourceCode final {
  const unsigned char *compressed{};
  size_t compressedSize{};
  size_t uncompressedSize{};
};

STR

# Each entry is the path under 'Builtin/' without the '.smdl' extension.
# The lookup key replaces '/' with '::' (so 'models/prospect' is addressed
# as '::models::prospect'), and the generated variable replaces '/' with '_'.
fnames = ['api', 'anno', 'debug', 'df', 'limits', 'math', 'scene', 'state', 'std', 'tex',
          'extras/io',
          'extras/pcg32',
          'models/illuminant',
          'models/prospect',
          'models/marmit',
          'models/metal_ior']
for fname in fnames
  text = `smdlc format -c --no-comments --keep-doc-comments Builtin/#{fname}.smdl`
  text = File.read "Builtin/#{fname}.smdl" unless $?.success?
  name = fname.gsub('/', '_')
  f.write "static const unsigned char #{name}_compressed[]{\n"
  Zlib::Deflate.deflate(text, Zlib::BEST_COMPRESSION).bytes.each_slice(24) do |row|
    f.write "    #{row.join(',')},\n"
  end
  f.write "};\n"
  f.write "static const CompressedSourceCode #{name}{"
  f.write "#{name}_compressed, sizeof(#{name}_compressed), #{text.bytesize}};\n\n"
end
f.write "static const std::string_view all_names[]{\n"
for fname in fnames
  f.write "    \"#{fname.gsub('/', '::')}\",\n"
end
f.write "};\n\n"
f.write <<STR
[[nodiscard]] static const CompressedSourceCode *get_source_code(std::string_view name) {
STR
for fname in fnames
  f.write <<STR
  if (name == "#{fname.gsub('/', '::')}")
    return &#{fname.gsub('/', '_')};
STR
end
f.write <<STR
  return nullptr;
}
STR

fnames = ['microfacet_ggx_smith_bsdf', 'microfacet_beckmann_smith_bsdf', 'simple_glossy_bsdf', 'ward_geisler_moroder_bsdf']
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

