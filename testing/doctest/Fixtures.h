/// \file
/// The vocabulary both suites share: scratch directories, environment
/// variables set for a scope, assertions that say what went wrong, and
/// comparisons over the library's vector types.
/// Everything here needs the public library and nothing else, so the
/// renderer suite includes it too. `smdl/Fixtures.h` adds what only the
/// library suite needs and `smdl-toy/Fixtures.h` what only the renderer
/// suite does.
#pragma once

#include "doctest.h"

#include <cmath>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>
#include <string_view>

#include "smdl/Support/Error.h"
#include "smdl/Support/VectorMath.h"

//--{ Assertions

/// Anything string-shaped as a `doctest::String`, so that an assertion can
/// report the text it was given rather than a pointer or a `false`.
[[nodiscard]] inline doctest::String asText(const std::string &value) {
  return doctest::String(value.data(), unsigned(value.size()));
}

/// See `asText`.
[[nodiscard]] inline doctest::String asText(std::string_view value) {
  return doctest::String(value.data(), unsigned(value.size()));
}

/// See `asText`.
[[nodiscard]] inline doctest::String asText(const char *value) {
  return doctest::String(value);
}

/// See `asText`.
[[nodiscard]] inline doctest::String asText(char value) {
  return doctest::String(&value, 1u);
}

/// Assert that `TEXT` contains `NEEDLE`.
///
/// Unlike `CHECK(text.find(needle) != std::string::npos)`, which reports
/// only `false`, this reports the text that was actually produced, which
/// is the whole point of asserting on a message. Either side may be a
/// `std::string`, a `std::string_view`, a C string, or a single character.
#define CHECK_CONTAINS(TEXT, NEEDLE) \
  CHECK(asText(TEXT) == doctest::Contains(asText(NEEDLE)))

/// Require that `TEXT` contains `NEEDLE`. See `CHECK_CONTAINS`.
#define REQUIRE_CONTAINS(TEXT, NEEDLE) \
  REQUIRE(asText(TEXT) == doctest::Contains(asText(NEEDLE)))

/// Assert that `TEXT` does not contain `NEEDLE`. See `CHECK_CONTAINS`.
#define CHECK_NOT_CONTAINS(TEXT, NEEDLE) \
  CHECK_FALSE(asText(TEXT) == doctest::Contains(asText(NEEDLE)))

/// Assert that an `std::optional<smdl::Error>` expression holds no error,
/// reporting the message if it does. The expression is evaluated once.
#define CHECK_OK(EXPR)                                                       \
  do {                                                                       \
    const auto smdlTestError{EXPR};                                          \
    CHECK_MESSAGE(!smdlTestError,                                            \
                  (smdlTestError ? smdlTestError->message : std::string())); \
  } while (false)

/// Require that an `std::optional<smdl::Error>` expression holds no error.
/// See `CHECK_OK`.
#define REQUIRE_OK(EXPR)                                                       \
  do {                                                                         \
    const auto smdlTestError{EXPR};                                            \
    REQUIRE_MESSAGE(!smdlTestError,                                            \
                    (smdlTestError ? smdlTestError->message : std::string())); \
  } while (false)

/// Assert that an `std::optional<smdl::Error>` expression holds an error
/// whose message contains `NEEDLE`. The expression is evaluated once.
#define CHECK_ERROR(EXPR, NEEDLE)                             \
  do {                                                        \
    const auto smdlTestError{EXPR};                           \
    REQUIRE_MESSAGE(smdlTestError.has_value(),                \
                    "expected an error containing ", NEEDLE); \
    CHECK_CONTAINS(smdlTestError->message, NEEDLE);           \
  } while (false)

/// Assert that two vectors or matrices agree to the bit, reporting both if
/// they do not.
///
/// The library's `==` on a vector is componentwise, so it yields a vector of
/// bools rather than something an assertion can take. That is why comparing
/// aggregates goes through `isSame` and `isNear` rather than through `CHECK`
/// directly.
#define CHECK_SAME(A, B) CHECK_MESSAGE(isSame(A, B), (A), " is not ", (B))

/// Assert that two vectors or matrices agree within `TOLERANCE`, reporting
/// both if they do not. See `CHECK_SAME`.
#define CHECK_NEAR(A, B, TOLERANCE)                                         \
  CHECK_MESSAGE(isNear(A, B, TOLERANCE), (A), " is not within ", TOLERANCE, \
                " of ", (B))

//--}

//--{ Scratch directories

/// A scratch directory that removes itself.
///
/// The destructor is what makes this worth having: a failing `REQUIRE`
/// throws, so a cleanup statement at the end of a test body never runs.
/// The constructor also clears the directory, so re-entering a test case
/// once per subcase starts from nothing every time.
///
/// The stem must be unique among the test cases in one binary, since the
/// path is derived from it alone; two instances of the same stem alive at
/// once would share a directory.
class TempDir final {
public:
  explicit TempDir(std::string_view stem)
      : mPath(std::filesystem::temp_directory_path() /
              ("smdl-test-" + std::string(stem))) {
    std::filesystem::remove_all(mPath);
    std::filesystem::create_directories(mPath);
  }

  TempDir(const TempDir &) = delete;

  TempDir &operator=(const TempDir &) = delete;

  ~TempDir() {
    auto ignored{std::error_code()};
    std::filesystem::remove_all(mPath, ignored);
  }

  /// The directory itself.
  [[nodiscard]] const std::filesystem::path &path() const noexcept {
    return mPath;
  }

  /// The path of `name` within the directory, which need not exist.
  [[nodiscard]] std::filesystem::path operator/(std::string_view name) const {
    return mPath / std::filesystem::path(name);
  }

  /// Write `text` to `name`, creating any intervening directories, and
  /// return the path written. The stream is binary, so the bytes on disk
  /// are the bytes given.
  std::filesystem::path write(std::string_view name,
                              std::string_view text) const {
    auto path{operator/(name)};
    std::filesystem::create_directories(path.parent_path());
    auto stream{std::ofstream(path, std::ios::binary | std::ios::trunc)};
    stream.write(text.data(), std::streamsize(text.size()));
    REQUIRE_MESSAGE(bool(stream), "cannot write ", path.string());
    return path;
  }

  /// Read `name` back.
  [[nodiscard]] std::string read(std::string_view name) const {
    auto stream{std::ifstream(operator/(name), std::ios::binary)};
    return std::string(std::istreambuf_iterator<char>(stream),
                       std::istreambuf_iterator<char>());
  }

private:
  std::filesystem::path mPath{};
};

//--}

//--{ Environment

/// An environment variable set for the duration of a scope, and put back
/// the way it was found, set or unset, when the scope ends. The suites
/// share one process and run in no fixed order, so a variable left
/// changed would change what a later test resolves.
class ScopedEnv final {
public:
  ScopedEnv(const char *name, const std::string &value) : mName(name) {
    if (const char *previous{std::getenv(name)}) mPrevious = previous;
    set(value.c_str());
  }

  ScopedEnv(const ScopedEnv &) = delete;

  ScopedEnv &operator=(const ScopedEnv &) = delete;

  ~ScopedEnv() {
    if (mPrevious) {
      set(mPrevious->c_str());
    } else {
      unset();
    }
  }

private:
  void set(const char *value) {
#if defined(_WIN32)
    _putenv_s(mName, value);
#else
    setenv(mName, value, 1);
#endif
  }

  void unset() {
#if defined(_WIN32)
    _putenv_s(mName, "");
#else
    unsetenv(mName);
#endif
  }

  const char *mName{};

  std::optional<std::string> mPrevious{};
};

//--}

//--{ Comparison

/// Are two floats the same, counting NaN as the same as NaN?
///
/// This is the predicate for anything that must survive a round trip
/// exactly, where a NaN payload is part of what survives.
[[nodiscard]] inline bool hasSameBits(float a, float b) {
  return (std::isnan(a) && std::isnan(b)) || a == b;
}

/// Are two vectors equal to the bit?
template <typename T, size_t N>
[[nodiscard]] inline bool isSame(const smdl::Vector<T, N> &a,
                                 const smdl::Vector<T, N> &b) {
  for (size_t i = 0; i < N; i++)
    if (!(a[i] == b[i])) return false;
  return true;
}

/// Is every component of `a` within `tolerance` of `b`'s?
template <typename T, size_t N>
[[nodiscard]] inline bool isNear(const smdl::Vector<T, N> &a,
                                 const smdl::Vector<T, N> &b,
                                 T tolerance = T(1e-5)) {
  for (size_t i = 0; i < N; i++)
    if (!(std::abs(a[i] - b[i]) <= tolerance)) return false;
  return true;
}

/// Are two matrices equal to the bit?
template <typename T, size_t N, size_t M>
[[nodiscard]] inline bool isSame(const smdl::Matrix<T, N, M> &a,
                                 const smdl::Matrix<T, N, M> &b) {
  for (size_t j = 0; j < N; j++)
    if (!isSame(a[j], b[j])) return false;
  return true;
}

/// Is every element of `a` within `tolerance` of `b`'s?
template <typename T, size_t N, size_t M>
[[nodiscard]] inline bool isNear(const smdl::Matrix<T, N, M> &a,
                                 const smdl::Matrix<T, N, M> &b,
                                 T tolerance = T(1e-5)) {
  for (size_t j = 0; j < N; j++)
    if (!isNear(a[j], b[j], tolerance)) return false;
  return true;
}

//--}

//--{ Stringification

namespace doctest {

/// So that a failing comparison prints the vectors instead of `false`.
template <typename T, size_t N> struct StringMaker<smdl::Vector<T, N>> {
  static String convert(const smdl::Vector<T, N> &value) {
    auto text{std::string("(")};
    for (size_t i = 0; i < N; i++) {
      if (i > 0) text += ", ";
      text += std::to_string(value[i]);
    }
    return String((text += ")").c_str());
  }
};

/// So that a failing comparison prints the matrices, column by column.
template <typename T, size_t N, size_t M>
struct StringMaker<smdl::Matrix<T, N, M>> {
  static String convert(const smdl::Matrix<T, N, M> &value) {
    auto text{std::string("[")};
    for (size_t j = 0; j < N; j++) {
      if (j > 0) text += ", ";
      text += StringMaker<smdl::Vector<T, M>>::convert(value[j]).c_str();
    }
    return String((text += "]").c_str());
  }
};

} // namespace doctest

//--}
