/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

namespace AST {

class File;

} // namespace AST

class Context;

class Declaration;

class Scope;

/// \addtogroup Main
/// \{

/// An MDL module.
class SMDL_EXPORT Module final {
public:
  /// The compile status, primarily used to detect cyclic imports.
  enum CompileStatus {
    COMPILE_STATUS_NOT_STARTED, ///< Not started yet.
    COMPILE_STATUS_IN_PROGRESS, ///< Currently in progress.
    COMPILE_STATUS_FINISHED,    ///< Finished!
    COMPILE_STATUS_FAILED,      ///< Failed with an error.
  };

public:
  Module() = default;

  /// Non-copyable!
  Module(const Module &) = delete;

  /// The builtin module constructor.
  ///
  /// \param[in] name
  /// The builtin lookup key, which may be nested under builtin
  /// packages, e.g., `models::prospect`. The last component becomes
  /// the module name and the components together form the qualified
  /// name, e.g., `::models::prospect`.
  ///
  /// \param[in] sourceCode
  /// The source code.
  ///
  explicit Module(std::string name, std::string sourceCode);

  ~Module();

  /// Load from file.
  ///
  /// \param[in] fileName
  /// The file name. This is passed directly to `std::ifstream` to load the
  /// file. This assumes any file path handling or lookup with `FileLocator`
  /// has already occurred.
  ///
  /// \param[in] searchRoot
  /// The canonical directory path of the search root the file was found
  /// under, used to derive the qualified module name. If empty, the parent
  /// directory of `fileName` is understood to be the search root, so the
  /// qualified name is just `::stem`.
  ///
  /// \note
  /// This is not exception-safe, and there are no guarantees about
  /// what it may or may not throw on failure.
  ///
  [[nodiscard]] static std::unique_ptr<Module>
  loadFromFile(const std::string &fileName, const std::string &searchRoot = {});

  /// Load from file extracted from archive.
  ///
  /// \param[in] archiveFileName
  /// The file name of the `.mdr` archive.
  ///
  /// \param[in] entryName
  /// The file name within the archive, e.g., `vendor/metals/steel.mdl`.
  ///
  /// \param[in] file
  /// The file extracted and decompressed from the archive.
  ///
  /// \param[in] searchRoot
  /// The search root, as in `loadFromFile()`. Per the MDL specification
  /// the archive sits at the top level of the search root and the entry
  /// paths encode the package structure, so the qualified name is
  /// derived from `entryName` alone, as if the archive were extracted
  /// in place: `vendor/metals/steel.mdl` becomes
  /// `::vendor::metals::steel`. The semantic file name reported by
  /// `getFileName()` is the archive file name concatenated with the
  /// entry name, as if the archive were a directory.
  ///
  [[nodiscard]] static std::unique_ptr<Module> loadFromFileExtractedFromArchive(
      const std::string &archiveFileName, const std::string &entryName,
      const std::string &file, const std::string &searchRoot = {});

  /// Load the `main.mdl` module from an MDLE container.
  ///
  /// \param[in] mdleFileName
  /// The file name of the `.mdle` container.
  ///
  /// \param[in] file
  /// The `main.mdl` source extracted from the container.
  ///
  /// \param[in] qualifiedName
  /// The content-based qualified name, e.g., `::mdle::<md5>`.
  ///
  /// \param[in] resourceDirectory
  /// The directory the container resources were extracted to, used as
  /// the anchor for locating resources referenced by the module (see
  /// `getResourceAnchor()`).
  ///
  [[nodiscard]] static std::unique_ptr<Module>
  loadFromMDLE(const std::string &mdleFileName, const std::string &file,
               const std::string &qualifiedName,
               const std::string &resourceDirectory);

public:
  /// Is a builtin module?
  [[nodiscard]] bool isBuiltin() const noexcept { return mFileName.empty(); }

  /// Is extracted from an archive?
  [[nodiscard]] bool isExtractedFromArchive() const noexcept {
    return mIsExtractedFromArchive;
  }

  /// Get the file name. This is empty if the module is builtin.
  [[nodiscard]] std::string_view getFileName() const noexcept {
    return mFileName;
  }

  /// Get the directory. This is empty if the module is builtin.
  [[nodiscard]] std::string getDirectory() const {
    return parentPathOf(mFileName);
  }

  /// Get the name.
  [[nodiscard]] std::string_view getName() const noexcept { return mName; }

  /// Get the canonical search root directory this module was found under.
  /// This is empty if the module is builtin.
  [[nodiscard]] std::string_view getSearchRoot() const noexcept {
    return mSearchRoot;
  }

  /// Get the qualified name derived from the path relative to the search
  /// root, e.g., `::vendor::metals::steel` for
  /// `<searchRoot>/vendor/metals/steel.mdl`. For builtin modules this is
  /// derived from the builtin lookup key instead, e.g.,
  /// `::models::prospect`.
  [[nodiscard]] std::string_view getQualifiedName() const noexcept {
    return mQualifiedName;
  }

  /// Is shadowed?
  ///
  /// This is true if another module with the same qualified name was
  /// added under an earlier search root, which makes this module
  /// unreachable by qualified name. It still compiles, and relative
  /// imports within its own directory tree still resolve to it.
  [[nodiscard]] bool isShadowed() const noexcept { return mIsShadowed; }

  /// Get the path used as the anchor when locating resources referenced
  /// by this module (textures, spectra, measurements, ...).
  ///
  /// This is ordinarily the module file name, so resources resolve
  /// relative to the containing directory. For modules loaded from MDLE
  /// containers it is the directory the container resources were
  /// extracted to.
  [[nodiscard]] std::string_view getResourceAnchor() const noexcept {
    return mResourceDirectory.empty() ? std::string_view(mFileName)
                                      : std::string_view(mResourceDirectory);
  }

  /// Get the source code.
  [[nodiscard]] std::string_view getSourceCode() const noexcept {
    return mSourceCode;
  }

  /// Parse the source code.
  [[nodiscard]] std::optional<Error>
  parse(BumpPtrAllocator &allocator) noexcept;

  /// Compile the source code to LLVM IR.
  [[nodiscard]] std::optional<Error> compile(Context &context) noexcept;

  /// Format the source code and write or overwrite the file on disk.
  [[nodiscard]] std::optional<Error>
  formatSourceCode(const FormatOptions &formatOptions) noexcept;

  /// Is parsed yet?
  [[nodiscard]] bool isParsed() const noexcept { return mRoot; }

  /// Is SMDL syntax? Only known after the module is parsed.
  ///
  /// This is true if the module file begins with the pragma `#smdl`
  /// used to indicate to the parser that the source code contains
  /// SMDL-specific syntax.
  [[nodiscard]] bool isSMDLSyntax() const noexcept;

  void reset() noexcept;

private:
  /// Is extracted from an archive?
  bool mIsExtractedFromArchive{};

  /// The file name if applicable. This is empty if the module is builtin.
  std::string mFileName{};

  /// The name of the module.
  std::string mName{};

  /// The canonical search root directory. This is empty if the module is
  /// builtin.
  std::string mSearchRoot{};

  /// The qualified name, e.g., `::vendor::metals::steel`. This is empty
  /// if the module is builtin.
  std::string mQualifiedName{};

  /// Is shadowed by an equally named module under an earlier search
  /// root? Maintained by `Compiler::add()`.
  bool mIsShadowed{};

  /// The resource anchor directory if it differs from the directory of
  /// `mFileName`. See `getResourceAnchor()`.
  std::string mResourceDirectory{};

  /// The source code.
  std::string mSourceCode{};

  /// The AST root node parsed from the source code.
  BumpPtr<AST::File> mRoot{};

  /// The compile status.
  CompileStatus mCompileStatus{COMPILE_STATUS_NOT_STARTED};

  /// If `mCompileStatus` is `COMPILE_STATUS_FAILED`, the original error
  /// message, rethrown on later references so the failure is not
  /// misreported as a cyclic import.
  std::string mCompileErrorMessage{};

  /// The root scope: the starting point for searches of this module's
  /// top-level declarations (see `Declaration::findInModule`).
  Scope *mRootScope{};

  friend class Compiler;

  friend class Context;

  friend class Declaration;

  friend class Emitter;

  friend class SourceLocation;

  friend class FunctionType;

  friend class MetaType;
};

/// \}

} // namespace smdl
