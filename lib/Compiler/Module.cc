#include "Module.h"

#include "Context.h"
#include "Emitter.h"
#include "Parser.h"

namespace smdl::Compiler {

void Module::parse(Context &context) {
  sanity_check(text != nullptr);
  root = Parser(context.bumpAllocator, path, text->getBuffer()).parse();
}

static constexpr auto src_evalOpacity = R"(
@(visible) float {0}__opacity() {{
  auto opacity(#inline({0}()).geometry.cutout_opacity);
  opacity = #max(opacity, 0.0);
  opacity = #min(opacity, 1.0);
  return opacity;
}}
)";

static constexpr auto src_evalBsdf = R"(
@(visible) int {0}__eval_bsdf(
  const &float3 wo, const &float3 wi, 
  const &float pdf_fwd, const &float pdf_rev, const &color f) {{
  auto this({0}());
  return ::df::material__eval_bsdf(&this, wo, wi, pdf_fwd, pdf_rev, f);
}}
)";

static constexpr auto src_evalBsdfSample = R"(
@(visible) int {0}__eval_bsdf_sample(
  const &float4 xi, 
  const &float3 wo, const &float3 wi, 
  const &float pdf_fwd, const &float pdf_rev, const &color f, const &int is_delta) {{
  auto this({0}());
  return ::df::material__eval_bsdf_sample(&this, xi, wo, wi, pdf_fwd, pdf_rev, f, is_delta);
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

    // Find every function that represents a material and emit the relevant functions.
    for (auto crumb{lastCrumb}; crumb; crumb = crumb->prev) {
      if (!lastImportCrumb && crumb->value.is_compile_time_module())
        lastImportCrumb = crumb;
      if (crumb->value.is_compile_time_function()) {
        if (auto func{crumb->value.get_compile_time_function()}; func->represents_material()) {
          auto material{Material{func}};
          auto materialName{material.material->get_name()};
          material.evalOpacity = context.get_function(emitter, std::format(src_evalOpacity, materialName));
          material.evalBsdf = context.get_function(emitter, std::format(src_evalBsdf, materialName));
          material.evalBsdfSample = context.get_function(emitter, std::format(src_evalBsdfSample, materialName));
          materials.push_back(material);
        }
      }
    }

    status = Status::Finished;
  }
}

} // namespace smdl::Compiler
