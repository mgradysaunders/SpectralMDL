#include "Value.h"
#include "Context.h"

namespace smdl::Compiler {

Value Value::zero(Type *type) {
  return RValue(
      sanity_check_nonnull(type),
      type->is_void() ? nullptr : llvm::Constant::getNullValue(sanity_check_nonnull(type->llvmType)));
}

bool Value::is_void() const { return !type || !llvmValue || type->is_void(); }

bool Value::is_compile_time_string() const { return is_compile_time() || type != type->context.get_string_type(); }

bool Value::is_compile_time_function() const { return is_compile_time() && type->is_compiler_function(); }

bool Value::is_compile_time_intrinsic() const { return is_compile_time() && type->is_compiler_intrinsic(); }

bool Value::is_compile_time_module() const { return is_compile_time() && type->is_compiler_module(); }

bool Value::is_compile_time_type() const { return is_compile_time() && type->is_compiler_type(); }

llvm::StringRef Value::get_compile_time_string() const {
  sanity_check(is_compile_time_string());
  auto ptr{llvm::IRBuilder<>(type->context.llvmContext).CreateExtractValue(llvmValue, {0u})};
  auto globalVar{sanity_check_nonnull(llvm::dyn_cast<llvm::GlobalVariable>(ptr))};
  auto dataArray{llvm::dyn_cast<llvm::ConstantDataArray>(sanity_check_nonnull(globalVar->getInitializer()))};
  return sanity_check_nonnull(dataArray)->getAsCString();
}

bool Crumb::is_exported_ast_decl() const {
  if (node)
    if (auto decl{llvm::dyn_cast<AST::Decl>(node)})
      return decl->isExport;
  return false;
}

bool Crumb::matches_name(llvm::StringRef name0) const {
  return (name.size() == 1 && name[0] == name0) || //
         (name.size() >= 2 && name.back() == name0 && is_ast_using_import() && value);
}

bool Crumb::matches_name(llvm::ArrayRef<llvm::StringRef> path) const {
  return path.size() == 1 ? matches_name(path[0]) : !path.empty() && name == path;
}

Crumb *Crumb::find(Crumb *crumb, llvm::ArrayRef<llvm::StringRef> name, llvm::Function *llvmFunc, int depth) {
  if (name.empty())
    return nullptr;
  for (; crumb; crumb = crumb->prev) {
    // If this is not usable, don't consider it.
    if (!crumb->value.is_usable_in_llvm_function(llvmFunc))
      continue;
    // If this is not exported and we recursed into a module, don't consider it.
    if (!crumb->is_exported_ast_decl() && depth > 0)
      continue;
    // If this matches the name sequence of the identifier, we're done!
    if (crumb->matches_name(name))
      return crumb;
    if (crumb->value.is_compile_time_module()) {
      // Look inside the module if it is either
      // 1. A universal import declaration in qualified form like 'import foo::bar::*' OR
      // 2. A universal import declaration in unqualified form like 'using foo::bar import *'
      if ((name.size() >= 2 && crumb->matches_name(name.drop_back(1))) || // Case 1
          (name.size() == 1 && crumb->is_ast_using_import())) {           // Case 2
        if (auto subCrumb{Crumb::find(crumb->value.get_compile_time_module()->lastCrumb, name.back(), llvmFunc, depth + 1)})
          return subCrumb;
      }
    }
  }
  return nullptr;
}

Param::Param(Context &context, const AST::Param &astParam)
    : name(astParam.name->name), type(astParam.type->type), isConst(bool(astParam.type->attrs.isConst)),
      isInline(bool(astParam.type->attrs.isInline)), init(astParam.init.get()), srcLoc(astParam.name->srcLoc) {
  context.validate_decl_name("parameter", *astParam.name);
  if (astParam.type->attrs.isStatic)
    astParam.name->srcLoc.report_error(std::format("parameter '{}' must not be declared 'static'", astParam.name->name));
  if (astParam.type->attrs.isInline && !astParam.type->type->get_inline_struct_type())
    astParam.name->srcLoc.report_error(std::format("parameter '{}' must not be declared 'inline'", astParam.name->name));
}

Param::Param(Context &context, const AST::Struct::Field &astField)
    : name(astField.name->name), type(astField.type->type), isVoid(astField.isVoid),
      isConst(bool(astField.type->attrs.isConst)), isInline(bool(astField.type->attrs.isInline)), init(astField.init.get()),
      srcLoc(astField.name->srcLoc) {
  context.validate_decl_name("field", *astField.name);
  if (astField.type->attrs.isStatic)
    astField.name->srcLoc.report_error(std::format("field '{}' must not be declared 'static'", astField.name->name));
  if (astField.type->attrs.isInline && !astField.type->type->get_inline_struct_type())
    astField.name->srcLoc.report_error(std::format("field '{}' must not be declared 'inline'", astField.name->name));
}

const ParamList *Param::get_inline_parameters() const {
  if (!isInline)
    return nullptr;
  return &type->get_inline_struct_type()->fields;
}

ParamList::ParamList(Context &context, const AST::Struct &decl) : crumb(decl.crumb) {
  for (auto &field : decl.fields)
    params.push_back(Param(context, field));
  guarantee_no_ambiguous_inlining(decl.srcLoc);
}

ParamList::ParamList(Context &context, const AST::Function &decl) : crumb(decl.crumb) {
  for (auto &param : decl.params)
    params.push_back(Param(context, param));
  guarantee_no_ambiguous_inlining(decl.srcLoc);
}

bool ParamList::has_any_abstract() const {
  return is_any_true([](const auto &param) { return param.type->is_abstract(); });
}

bool ParamList::has_any_inline() const {
  return is_any_true([](const auto &param) { return param.isInline; });
}

template <typename T> [[nodiscard]] static llvm::SmallVector<T> get_vector_of_member(auto &params, auto &&getter) {
  auto result{llvm::SmallVector<T>{}};
  for (auto &param : params)
    result.push_back(std::invoke(getter, param));
  return result;
}

llvm::SmallVector<llvm::StringRef> ParamList::get_names() const {
  return get_vector_of_member<llvm::StringRef>(params, [](auto &param) { return param.name; });
}

llvm::SmallVector<Type *> ParamList::get_types() const {
  return get_vector_of_member<Type *>(params, [](auto &param) { return param.type; });
}

llvm::SmallVector<llvm::Type *> ParamList::get_llvm_types() const {
  auto llvmTypes{get_vector_of_member<llvm::Type *>(params, [](auto &param) { return param.type->llvmType; })};
  for (auto itr{llvmTypes.begin()}; auto &param : params) {
    if (param.isVoid)
      itr = llvmTypes.erase(itr);
    else
      itr++;
  }
  return llvmTypes;
}

bool ParamList::get_inline_path(llvm::StringRef name, llvm::SmallVector<std::pair<const Param *, unsigned>> &path) const {
  for (unsigned i = 0; auto &param : params) {
    path.push_back({&param, i});
    if (param.name == name || (param.isInline && param.type->get_inline_struct_type()->fields.get_inline_path(name, path)))
      return true;
    path.pop_back();
    if (!param.isVoid)
      i++;
  }
  return false;
}

void ParamList::guarantee_no_ambiguous_inlining(const AST::SourceLocation &srcLoc) const {
  llvm::SmallVector<const Param *> todo{};
  for (auto &param : params)
    todo.push_back(&param);
  llvm::StringSet<> names{};
  while (!todo.empty()) {
    auto param{todo.back()};
    todo.pop_back();
    if (!names.insert(param->name).second)
      srcLoc.report_error(std::format("ambiguous name '{}' after inlining", param->name));
    if (param->isInline)
      for (auto &field : param->type->get_inline_struct_type()->fields)
        todo.push_back(&field);
  }
}

llvm::SmallVector<Type *> ArgList::get_types() const {
  return get_vector_of_member<Type *>(args, [](auto &arg) { return arg.value.type; });
}

llvm::SmallVector<llvm::Type *> ArgList::get_llvm_types() const {
  return get_vector_of_member<llvm::Type *>(args, [](auto &arg) { return arg.value.type->llvmType; });
}

void ArgList::guarantee_valid_names(const AST::SourceLocation &srcLoc) const {
  llvm::StringSet<> names{};
  for (auto &arg : args) {
    if (arg.is_named()) {
      if (!names.insert(arg.name).second)
        srcLoc.report_error(std::format("ambiguous name '{}' in argument list", arg.name));
    } else if (!names.empty()) {
      srcLoc.report_error("unnamed arguments must appear before named arguments in argument list");
    }
  }
}

ArgList::operator std::string() const {
  std::string str{};
  str.reserve(256);
  for (size_t i{}; const auto &arg : args) {
    if (arg.is_named()) {
      str += arg.name.str();
      str += ": ";
    }
    str += arg.value.type->name;
    if (i + 1 < args.size())
      str += ", ";
    i++;
  }
  return str;
}

} // namespace smdl::Compiler
