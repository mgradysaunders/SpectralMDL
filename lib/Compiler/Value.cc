#include "Value.h"

#include "Context.h"

namespace smdl {

Value Value::zero(Type *type) {
  SMDL_SANITY_CHECK(type);
  SMDL_SANITY_CHECK(type->llvmType);
  return RValue(type, llvm::Constant::getNullValue(type->llvmType));
}

bool Value::is_void() const { return !type || type->is_void(); }

bool Value::is_comptime_string() const {
  return is_comptime() && type->is_string();
}

std::string_view Value::get_comptime_string() const {
  if (!is_comptime_string())
    return {};
  auto globalVar{llvm::dyn_cast_if_present<llvm::GlobalVariable>(llvmValue)};
  if (!globalVar)
    return {};
  auto dataArray{llvm::dyn_cast_if_present<llvm::ConstantDataArray>(
      globalVar->getInitializer())};
  if (!dataArray)
    return {};
  return dataArray->getAsCString();
}

bool Value::is_comptime_meta_module(Context &context) const {
  return is_comptime() && type == context.get_meta_module_type();
}

bool Value::is_comptime_meta_type(Context &context) const {
  return is_comptime() && type == context.get_meta_type_type();
}

bool Value::is_comptime_meta_intrinsic(Context &context) const {
  return is_comptime() && type == context.get_meta_intrinsic_type();
}

Crumb *Crumb::find(Context &context, Span<std::string_view> name,
                   llvm::Function *llvmFunc, Crumb *crumb,
                   bool ignoreIfNotExported) {
  if (name.empty())
    return nullptr;
  for (; crumb; crumb = crumb->prev) {
    // If this is not usable, don't consider it.
    // If this is not exported and we recursed into a module, don't consider it.
    if (!crumb->value || //
        (!crumb->value.is_usable_in_llvm_function(llvmFunc)) ||
        (!crumb->is_exported() && ignoreIfNotExported))
      continue;
    // If the crumb value is a `Module` ...
    if (crumb->value.is_comptime_meta_module(context)) {
      // Look inside the module if it is either
      // 1. A universal import in unqualified form `using foo::bar import *`
      // 2. A universal import in qualified form `import foo::bar::*` or
      if ((name.size() == 1 && crumb->is_ast_using_import()) ||
          (name.size() >= 2 && crumb->is_ast_import() &&
           crumb->name == name.subspan(0, name.size() - 1))) {
        if (auto innerCrumb{
                Crumb::find(context, name.back(), llvmFunc,
                            crumb->value
                                .get_comptime_meta_module(
                                    context, crumb->get_source_location())
                                ->lastCrumb,
                            /*ignoreIfNotExported=*/true)}) {
          return innerCrumb;
        }
      }
    }
    // TODO Handle specific using imports (match identifier or simple name)
    if (crumb->name == name) {
      return crumb;
    }
  }
  return nullptr;
}

bool ParameterList::is_abstract() const {
  return is_any_true([](auto &param) { return param.type->is_abstract(); });
}

std::vector<std::string_view> ParameterList::get_names() const {
  return transform<std::string_view>([](auto &param) { return param.name; });
}

std::vector<Type *> ParameterList::get_types() const {
  return transform<Type *>([](auto &param) { return param.type; });
}

std::vector<llvm::Type *> ParameterList::get_llvm_types() const {
  return transform<llvm::Type *>(
      [](auto &param) { return param.type->llvmType; });
}

bool ParameterList::get_lookup_sequence(std::string_view name,
                                        LookupSeq &seq) const {
  unsigned i = 0;
  for (auto &param : *this) {
    seq.push_back({&param, i});
    if (param.name == name)
      return true;
    if (param.is_inline()) {
      if (auto structType{llvm::dyn_cast<StructType>(
              param.type->get_first_non_pointer_type())};
          structType && structType->params.get_lookup_sequence(name, seq)) {
        return true;
      }
    }
    seq.pop_back();
    i++;
  }
  return false;
}

bool Argument::is_visited() const {
  return ((astArg && astArg->is_visited()) || impliedVisit) &&
         value.type->is_union_or_pointer_to_union();
}

std::vector<std::string_view> ArgumentList::get_names() const {
  return transform<std::string_view>([](auto &arg) { return arg.name; });
}

std::vector<Type *> ArgumentList::get_types() const {
  return transform<Type *>([](auto &arg) { return arg.value.type; });
}

std::vector<llvm::Type *> ArgumentList::get_llvm_types() const {
  return transform<llvm::Type *>(
      [](auto &arg) { return arg.value.type->llvmType; });
}

std::vector<Value> ArgumentList::get_values() const {
  return transform<Value>([](auto &arg) { return arg.value; });
}

void ArgumentList::validate_names() {
  auto uniqueNames{llvm::StringSet<>()};
  for (auto &arg : elems) {
    if (arg.is_named()) {
      auto [itr, inserted] = uniqueNames.insert(arg.name);
      if (!inserted)
        get_source_location().throw_error(
            concat("ambiguous name '", arg.name, "' in argument list"));
    } else if (!uniqueNames.empty()) {
      get_source_location().throw_error(
          "unnamed arguments must appear before named "
          "arguments in argument list");
    }
  }
}

ArgumentList::operator std::string() const {
  std::string str{};
  str += '(';
  for (size_t i = 0; i < size(); i++) {
    if (elems[i].is_named()) {
      str += std::string(elems[i].name);
      str += ": ";
    }
    str += elems[i].value.type->displayName;
    if (i + 1 < size())
      str += ", ";
  }
  str += ')';
  return str;
}

} // namespace smdl
