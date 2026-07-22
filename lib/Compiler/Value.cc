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

Declaration *Declaration::findThroughImport(Context &context,
                                            Span<const std::string_view> name,
                                            llvm::Function *llvmFunc,
                                            Declaration *declaration) {
  // If the declaration value is a `Module` ...
  if (declaration->value.isComptimeMetaModule(context)) {
    auto module_{declaration->value.getComptimeMetaModule(
        context, declaration->getSourceLocation())};
    // Look inside the module if it is either
    // 1. A universal import in unqualified form `using foo::bar import *`
    // 2. A universal import in qualified form `import foo::bar::*`
    if (declaration->isASTUsingImport() || declaration->isASTImport()) {
      // Search for qualified names `foo::bar::baz`, but only if the
      // name actually begins with the imported module name `foo::bar`.
      // The size check also keeps the `subspan` below in range.
      if (name.size() > declaration->name.size() &&
          name.startsWith(declaration->name)) {
        if (auto subDeclaration{
                findInModule(context, name.subspan(declaration->name.size()),
                             llvmFunc, module_)}) {
          return subDeclaration;
        }
      }
      // Search for unqualified names `baz` if universal unqualified
      // import `using foo::bar import *`
      if (declaration->isASTUsingImport())
        if (auto subDeclaration{
                findInModule(context, name, llvmFunc, module_)}) {
          return subDeclaration;
        }
    }
  } else if (declaration->isASTUsingImport() &&
             declaration->name.back() == name.back() && name.size() == 1) {
    // The unqualified match of a specific import `using foo import bar`,
    // whose declaration is named `foo::bar`.
    return declaration->isUsed = 1, declaration;
  }
  // The exact-name match of the declaration itself, e.g. the universal import
  // `import foo::*` resolving the name `foo`.
  if ((declaration->name.data() == name.data() &&
       declaration->name.size() == name.size()) ||
      declaration->name == name) {
    return declaration->isUsed = 1, declaration;
  }
  return nullptr;
}

Declaration *Declaration::resolveInScope(Context &context,
                             Span<const std::string_view> name,
                             llvm::Function *llvmFunc, Scope *scope,
                             bool ignoreIfNotExported, uint64_t seqLimit,
                             Declaration **unusableMatch) {
  if (!scope || name.empty())
    return nullptr;
  name = context.internName(name);
  // Gather the candidates for every prefix of the name — an exact-name
  // match for the full name, a namespace descent for a proper prefix — and
  // try them newest-first, mirroring the chain walk's declaration order.
  struct Candidate final {
    Declaration *declaration{};
    size_t prefixSize{};
  };
  auto candidates{llvm::SmallVector<Candidate, 4>{}};
  for (size_t k = 1; k <= name.size(); k++) {
    auto prefix{k == name.size() ? name
                                 : context.internName(name.subspan(0, k))};
    if (auto itr{scope->decls.find(prefix.data())}; itr != scope->decls.end())
      for (auto c{itr->second}; c; c = c->prevSameNameInScope)
        candidates.push_back({c, k});
  }
  if (candidates.size() > 1)
    std::sort(candidates.begin(), candidates.end(),
              [](const Candidate &candA, const Candidate &candB) {
                return candA.declaration->seq > candB.declaration->seq;
              });
  for (const auto &[c, prefixSize] : candidates) {
    if (c->seq > seqLimit)
      continue;
    if (ignoreIfNotExported && !c->isExported())
      continue;
    if (prefixSize == name.size()) {
      if (!c->value.isUsableInLLVMFunction(llvmFunc)) {
        if (unusableMatch && !*unusableMatch)
          *unusableMatch = c;
        continue;
      }
      return c->isUsed = 1, c;
    }
    // Descend into namespaces for proper prefixes, propagating the export
    // filter: 'export' gates access across module boundaries only.
    if (c->value.isComptimeMetaNamespace(context)) {
      auto astNamespace{
          c->value.getComptimeMetaNamespace(context, c->getSourceLocation())};
      if (astNamespace->scope)
        if (auto found{resolveInScope(
                context, name.subspan(prefixSize), llvmFunc,
                astNamespace->scope, ignoreIfNotExported,
                std::numeric_limits<uint64_t>::max(), nullptr)})
          return found;
    }
  }
  // Then the imports, newest-first. Imports always precede every other
  // declaration in their scope, so trying them after `decls` preserves the
  // chain's newest-first order.
  for (auto itr{scope->imports.rbegin()}; itr != scope->imports.rend();
       ++itr) {
    auto importDeclaration{*itr};
    if (importDeclaration->seq > seqLimit)
      continue;
    if (ignoreIfNotExported && !importDeclaration->isExported())
      continue;
    if (auto found{
            findThroughImport(context, name, llvmFunc, importDeclaration)})
      return found;
  }
  return nullptr;
}

Declaration *Declaration::findInModule(Context &context,
                                       Span<const std::string_view> name,
                                       llvm::Function *llvmFunc,
                                       Module *module_,
                                       bool ignoreIfNotExported) {
  return resolveInScope(context, name, llvmFunc, module_->mRootScope,
                        ignoreIfNotExported,
                        std::numeric_limits<uint64_t>::max(), nullptr);
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
                                      LookupSequence &seq) const {
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
