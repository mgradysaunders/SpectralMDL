/// \file
#pragma once

#include "smdl/common.h"

namespace smdl {

namespace AST {

class Node;

} // namespace AST

class Context;

class Crumb;

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
  };

public:
  Module() = default;

  /// The builtin module constructor.
  ///
  /// \param[in] name         The name of the module.
  /// \param[in] sourceCode   The source code.
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
  /// \note
  /// This is not exception-safe, and there are no guarantees about
  /// what it may or may not throw on failure.
  ///
  [[nodiscard]] static std::unique_ptr<Module>
  load_from_file(const std::string &fileName);

  /// Load from file extracted from archive.
  ///
  /// \param[in] fileName
  /// The semantic file name. This is the file name of the archive itself
  /// concatenated to the file name within the archive, as if the archive
  /// were a directory.
  ///
  /// \param[in] file
  /// The file extracted and decompressed from the archive.
  ///
  [[nodiscard]] static std::unique_ptr<Module>
  load_from_file_extracted_from_archive(const std::string &fileName,
                                        const std::string &file);

public:
  /// Is a builtin module?
  [[nodiscard]] bool is_builtin() const noexcept { return fileName.empty(); }

  /// Is extracted from an archive?
  [[nodiscard]] bool is_extracted_from_archive() const noexcept {
    return isExtractedFromArchive;
  }

  /// Get the file name. This is empty if the module is builtin.
  [[nodiscard]] std::string_view get_file_name() const noexcept {
    return fileName;
  }

  /// Get the name.
  [[nodiscard]] std::string_view get_name() const noexcept { return name; }

  /// Get the source code.
  [[nodiscard]] std::string_view get_source_code() const noexcept {
    return sourceCode;
  }

  /// Is SMDL syntax?
  [[nodiscard]] bool is_smdl_syntax() const noexcept {
    return starts_with(sourceCode, "#smdl\n");
  }

  /// Parse the source code.
  [[nodiscard]] std::optional<Error>
  parse(BumpPtrAllocator &allocator) noexcept;

  /// Compile the source code to LLVM IR.
  [[nodiscard]] std::optional<Error> compile(Context &context) noexcept;

  /// Format the source code and write or overwrite the file on disk.
  [[nodiscard]] std::optional<Error>
  format_source_code(const FormatOptions &formatOptions) noexcept;

  /// Is parsed yet?
  [[nodiscard]] bool is_parsed() const noexcept { return root; }

  void reset() noexcept;

private:
  /// Is extracted from an archive?
  bool isExtractedFromArchive{};

  /// The file name if applicable. This is empty if the module is builtin.
  std::string fileName{};

  /// The name of the module.
  std::string name{};

  /// The source code.
  std::string sourceCode{};

  /// The AST root node parsed from the source code.
  BumpPtr<AST::Node> root{};

  /// The compile status.
  CompileStatus compileStatus{COMPILE_STATUS_NOT_STARTED};

  /// The last crumb. This is the starting point to search for exported
  /// declarations.
  Crumb *lastCrumb{};

  friend class Compiler;

  friend class Context;

  friend class Crumb;

  friend class Emitter;

  friend class SourceLocation;

  friend class FunctionType;

  friend class MetaType;
};

/// \}

} // namespace smdl
