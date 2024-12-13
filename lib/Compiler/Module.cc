#include "Module.h"

#include "Context.h"
#include "Emitter.h"
#include "Formatter.h"
#include "Parser.h"

#include <fstream>

namespace smdl::Compiler {

void Module::parse(Context &context) {
  auto parser{Parser(context.bumpAllocator, (!filenameStr.empty() ? filenameStr : name), text)};
  root = parser.parse();
  isSmdlSyntax = parser.is_smdl_syntax();
}

void Module::format_source() {
  if (!is_builtin()) {
    sanity_check(is_parse_finished());
    // Format before opening the file for writing, so that if anything 
    // goes wrong we don't wipe out the original source.
    auto formatter{Formatter(text)};
    formatter.write(root); 
    // No errors or sanity check crashes, so open the file and write the string.
    auto ofs{std::ofstream(filename)};
    if (!ofs.is_open())
      throw Error(std::format("can't open '{}'", filenameStr));
    ofs << formatter.outSrc;
  }
}

static constexpr auto src_evalGeometry = R"(
@(visible) float {0}__evaluate_geometry(const &float3 displacement) {{
  auto geometry(#inline({0}()).geometry);
  *displacement = geometry.displacement;
  auto opacity(geometry.cutout_opacity);
  opacity = #max(opacity, 0.0);
  opacity = #min(opacity, 1.0);
  return opacity;
}}
)";

static constexpr auto src_evalBsdf = R"(
@(visible) int {0}__evaluate_bsdf(
  const &float3 wo, const &float3 wi, 
  const &float pdf_fwd, const &float pdf_rev, const &color f) {{
  auto this({0}());
  return ::df::material__evaluate_bsdf(&this, wo, wi, pdf_fwd, pdf_rev, f);
}}
)";

static constexpr auto src_evalBsdfSample = R"(
@(visible) int {0}__evaluate_bsdf_sample(
  const &float4 xi, 
  const &float3 wo, const &float3 wi, 
  const &float pdf_fwd, const &float pdf_rev, const &color f, const &int is_delta) {{
  auto this({0}());
  return ::df::material__evaluate_bsdf_sample(&this, xi, wo, wi, pdf_fwd, pdf_rev, f, is_delta);
}}
)";

void Module::emit(Context &context) {
  if (status != Status::Finished) {
    if (status == Status::InProgress)
      throw Error(std::format("module '{}' reached through cyclic 'import' sequence", name));
    status = Status::InProgress;
    Emitter emitter{context, this, /*crumb=*/nullptr};
    emitter.emit(*root);
    lastCrumb = emitter.crumb;
    for (auto crumb{lastCrumb}; crumb; crumb = crumb->prev) {
      if (!lastImportCrumb && crumb->value.is_compile_time_module())
        lastImportCrumb = crumb;
      if (crumb->value.is_compile_time_function()) {
        if (auto func{crumb->value.get_compile_time_function()}; func->represents_material()) {
          auto &material{context.mdl.materials.emplace_back()};
          material.moduleName = name;
          material.name = func->get_name().str();
          auto emitMaterialFunction{[&](std::string src) -> std::string {
            auto func{context.get_function(emitter, src)};
            sanity_check_nonnull(func);
            sanity_check(func->has_unique_concrete_instance());
            return func->get_unique_concrete_instance()->get_link_name().str();
          }};
          material.evalGeometry.name = emitMaterialFunction(std::format(src_evalGeometry, material.name));
          material.evalBsdf.name = emitMaterialFunction(std::format(src_evalBsdf, material.name));
          material.evalBsdfSample.name = emitMaterialFunction(std::format(src_evalBsdfSample, material.name));
        }
      }
    }
    status = Status::Finished;
  }
}

} // namespace smdl::Compiler
