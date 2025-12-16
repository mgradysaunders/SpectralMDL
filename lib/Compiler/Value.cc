#include "Value.h"

#include "Context.h"

namespace smdl {

Value Value::zero(Type *type) {
  SMDL_SANITY_CHECK(type);
  SMDL_SANITY_CHECK(type->llvmType);
  return RValue(type, llvm::Constant::getNullValue(type->llvmType));
}

bool Value::isVoid() const { return !type || type->isVoid(); }

bool Value::isComptimeString() const {
  return isComptime() && type->isString();
}

std::string_view Value::getComptimeString() const {
  if (!isComptimeString())
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

bool Value::isComptimeMetaModule(Context &context) const {
  return isComptime() && type == context.getMetaModuleType();
}

bool Value::isComptimeMetaType(Context &context) const {
  return isComptime() && type == context.getMetaTypeType();
}

bool Value::isComptimeMetaIntrinsic(Context &context) const {
  return isComptime() && type == context.getMetaIntrinsicType();
}

bool Value::isComptimeMetaNamespace(Context &context) const {
  return isComptime() && type == context.getMetaNamespaceType();
}

Crumb *Crumb::find(Context &context, Span<const std::string_view> name,
                   llvm::Function *llvmFunc, Crumb *crumb, Crumb *stopCrumb,
                   bool ignoreIfNotExported) {
  if (name.empty()) {
    return nullptr;
  }
  for (; crumb && crumb != stopCrumb; crumb = crumb->prev) {
    // If this is not usable, don't consider it.
    // If this is not exported and we recursed into a module, don't consider it.
    if ( // !crumb->value || //
        (!crumb->value.isUsableInLLVMFunction(llvmFunc)) ||
        (!crumb->isExported() && ignoreIfNotExported))
      continue;
    // If the crumb value is a `Module` ...
    if (crumb->value.isComptimeMetaModule(context)) {
      auto module_{crumb->value.getComptimeMetaModule(
          context, crumb->getSourceLocation())};
      // Look inside the module if it is either
      // 1. A universal import in unqualified form `using foo::bar import *`
      // 2. A universal import in qualified form `import foo::bar::*`
      if (crumb->isASTUsingImport() ||
          (crumb->isASTImport() && name.size() > crumb->name.size() &&
           name.starts_with(crumb->name))) {
        // Search for qualified names `foo::bar::baz`
        if (auto subCrumb{Crumb::find(context, name.subspan(crumb->name.size()),
                                      llvmFunc, module_->mLastCrumb, nullptr,
                                      /*ignoreIfNotExported=*/true)}) {
          return subCrumb;
        }
        // Search for unqualified names `baz` if universal unqualified
        // import `using foo::bar import *`
        if (crumb->isASTUsingImport())
          if (auto subCrumb{Crumb::find(context, name, llvmFunc,
                                        module_->mLastCrumb, nullptr,
                                        /*ignoreIfNotExported=*/true)}) {
            return subCrumb;
          }
      }
    } else if (crumb->isASTUsingImport() && crumb->name.back() == name.back() &&
               name.size() == 1) {
      return crumb->isUsed = 1, crumb;
    }
    // If the crumb value is an `AST::Namespace` ...
    if (crumb->value.isComptimeMetaNamespace(context)) {
      auto astNamespace{crumb->value.getComptimeMetaNamespace(
          context, crumb->getSourceLocation())};
      if (name.size() > crumb->name.size() && //
          name.starts_with(crumb->name)) {
        if (auto subCrumb{Crumb::find(context, name.subspan(crumb->name.size()),
                                      llvmFunc, astNamespace->lastCrumb, crumb,
                                      /*ignoreIfNotExported=*/true)}) {
          return subCrumb;
        }
      }
    }
    if (crumb->name == name) {
      return crumb->isUsed = 1, crumb;
    }
  }
  return nullptr;
}

bool ParameterList::isAbstract() const {
  return isAnyTrue([](auto &param) { return param.type->isAbstract(); });
}

std::vector<std::string_view> ParameterList::getNames() const {
  return transform<std::string_view>([](auto &param) { return param.name; });
}

std::vector<Type *> ParameterList::getTypes() const {
  return transform<Type *>([](auto &param) { return param.type; });
}

std::vector<llvm::Type *> ParameterList::getLLVMTypes() const {
  return transform<llvm::Type *>([](auto &param) {
    auto llvmType = param.type->llvmType;
    // Account for `void` fields!
    if (llvmType && llvmType->isVoidTy())
      llvmType = llvm::Type::getInt8Ty(llvmType->getContext());
    return llvmType;
  });
}

bool ParameterList::getLookupSequence(std::string_view name,
                                      LookupSeq &seq) const {
  unsigned i = 0;
  for (auto &param : *this) {
    seq.push_back({&param, i});
    if (param.name == name)
      return true;
    if (param.isInline()) {
      if (auto structType{
              llvm::dyn_cast<StructType>(param.type->getFirstNonPointerType())};
          structType && structType->params.getLookupSequence(name, seq)) {
        return true;
      }
    }
    seq.pop_back();
    i++;
  }
  return false;
}

bool Argument::isVisited() const {
  return ((astArg && astArg->isVisited()) || impliedVisit) &&
         value.type->isUnionOrPointerToUnion();
}

std::vector<std::string_view> ArgumentList::getNames() const {
  return transform<std::string_view>([](auto &arg) { return arg.name; });
}

std::vector<Type *> ArgumentList::getTypes() const {
  return transform<Type *>([](auto &arg) { return arg.value.type; });
}

std::vector<llvm::Type *> ArgumentList::getLLVMTypes() const {
  return transform<llvm::Type *>(
      [](auto &arg) { return arg.value.type->llvmType; });
}

std::vector<Value> ArgumentList::getValues() const {
  return transform<Value>([](auto &arg) { return arg.value; });
}

void ArgumentList::validateNames() {
  auto uniqueNames{llvm::StringSet<>()};
  for (auto &arg : elems) {
    if (arg.isNamed()) {
      auto [itr, inserted] = uniqueNames.insert(arg.name);
      if (!inserted)
        getSourceLocation().throwError("ambiguous name '", arg.name,
                                       "' in argument list");
    } else if (!uniqueNames.empty()) {
      getSourceLocation().throwError(
          "unnamed arguments must appear before named "
          "arguments in argument list");
    }
  }
}

ArgumentList::operator std::string() const {
  std::string str{};
  str += '(';
  for (size_t i = 0; i < size(); i++) {
    if (elems[i].isNamed()) {
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
