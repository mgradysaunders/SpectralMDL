/// \file
#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "smdl/Support/Error.h"

namespace smdl {

/// \addtogroup support
/// \{

/// A document in the strict flat subset of YAML that this project's
/// manifests use, such as the renderer's `.asset` files.
///
/// The subset:
/// - UTF-8 text, `#` comments (full-line, or trailing after whitespace),
/// - `key: value` scalars, where a value is a bare string ending at the
///   comment or end of line, or a double-quoted string with `\"` and `\\`
///   escapes,
/// - inline lists `[a, b]`, whose items are scalars or, one level deep,
///   inline lists of scalars,
/// - one level of block nesting under a key with no value: either
///   consistently indented `key: value` lines (a block map), or `- key:
///   value` items, each continued by `key: value` lines indented to the
///   column of the item's first key (a block sequence of maps), and
/// - no anchors, tags, multi-document streams, multiline strings, flow
///   maps, or tab indentation.
///
/// Every conforming document is valid YAML, so external tooling can read
/// it with any stock parser; this implementation accepts only the subset,
/// so documents stay portable by construction. Everything outside the
/// subset is a specific, line-numbered error, and keys are unique within
/// their map. The typed readers below turn a value into what a consumer
/// expects, or throw the same kind of error naming the key, so every
/// manifest reports its problems in one voice.
class SMDL_EXPORT FlatYAML final {
public:
  class Entry;

  /// A block map: the entries in document order.
  using Map = std::vector<Entry>;

  /// A value.
  class Node final {
  public:
    /// The kind of value.
    enum Kind : int {
      SCALAR = 0, ///< A bare or quoted string.
      LIST,       ///< An inline list of scalars or of lists of scalars.
      MAP,        ///< A block map.
      SEQUENCE    ///< A block sequence of maps.
    };

    /// The kind.
    Kind kind{SCALAR};

    /// The line number, for diagnostics.
    int lineNo{};

    /// The text of a scalar, unescaped.
    std::string text{};

    /// Was the scalar double-quoted? A quoted scalar is a string even when
    /// it spells a number.
    bool quoted{};

    /// The items of a list, each a `SCALAR` or a `LIST` of scalars.
    std::vector<Node> items{};

    /// The entries of a block map.
    Map map{};

    /// The maps of a block sequence.
    std::vector<Map> sequence{};
  };

  /// A `key: value` entry.
  class Entry final {
  public:
    /// The key.
    std::string key{};

    /// The line number of the key, for diagnostics.
    int lineNo{};

    /// The value.
    Node value{};
  };

public:
  /// Parse the source.
  ///
  /// \param[in] source      The document text.
  /// \param[in] sourceName  The name error messages blame.
  ///
  /// \throw Error On anything outside the subset, blaming the line.
  ///
  [[nodiscard]] static FlatYAML parse(std::string_view source,
                                      std::string sourceName);

  /// Throw an `Error` blaming the given line of this document.
  [[noreturn]] void fail(int lineNo, std::string_view message) const;

  /// Throw an `Error` blaming the given entry.
  [[noreturn]] void fail(const Entry &entry, std::string_view message) const {
    fail(entry.lineNo, message);
  }

  /// The value as a string, bare or quoted.
  [[nodiscard]] const std::string &toString(const Entry &entry) const;

  /// The value as a real number. A quoted scalar is not a number.
  [[nodiscard]] float toFloat(const Entry &entry) const;

  /// The list item as a real number, blaming `entry` if it is not one.
  [[nodiscard]] float toFloat(const Entry &entry, const Node &item) const;

  /// The value as an integer.
  [[nodiscard]] long toInt(const Entry &entry) const;

  /// The value as an inline list.
  [[nodiscard]] const std::vector<Node> &toList(const Entry &entry) const;

  /// The value as an inline list of exactly `count` real numbers.
  [[nodiscard]] std::vector<float> toFloats(const Entry &entry,
                                            size_t count) const;

  /// The value as a block map.
  [[nodiscard]] const Map &toMap(const Entry &entry) const;

  /// The value as a block sequence of maps.
  [[nodiscard]] const std::vector<Map> &toSequence(const Entry &entry) const;

  /// Find the entry with the given key in a map, or null.
  [[nodiscard]] static const Entry *find(const Map &map,
                                         std::string_view key) noexcept;

  /// Parse the whole text as a real number, or nothing.
  [[nodiscard]] static std::optional<double>
  parseNumber(std::string_view text) noexcept;

  /// Does the whole text spell a real number?
  [[nodiscard]] static bool isNumber(std::string_view text) noexcept {
    return parseNumber(text).has_value();
  }

public:
  /// The name error messages blame.
  std::string sourceName{};

  /// The top-level map.
  Map root{};
};

/// \}

} // namespace smdl
