#pragma once

#include <atomic>
#include <chrono>
#include <cstdint>
#include <mutex>
#include <string>
#include <string_view>

#include "smdl/Support/Logger.h"

/// How a `ProgressBar` draws.
enum class ProgressStyle {
  /// Draw the block drawing characters when the environment claims
  /// UTF-8, and the ASCII bar when it does not.
  AUTO,

  /// Always draw the ASCII bar. The escape hatch for a terminal that
  /// claims UTF-8 and then draws the block characters as boxes.
  PLAIN,

  /// Never draw at all.
  NONE
};

/// Parse the `-progress` name, which is the one place it is spelled.
/// The bar is not constructed until everything has loaded and compiled,
/// so leaving this to the constructor would report a misspelling only
/// once the render is about to start.
///
/// \throws smdl::Error  If the name is not recognized.
///
[[nodiscard]] ProgressStyle parseProgressStyle(std::string_view name);

/// What a `ProgressBar` measures and how it draws, resolved from the
/// command line into plain values.
struct ProgressOptions final {
  /// The text before the bar, e.g. `"Rendering"`.
  std::string label{"Working"};

  /// The counter suffix, e.g. `"px"`. Empty prints bare counters.
  std::string units{};

  /// How the bar draws.
  ProgressStyle style{ProgressStyle::AUTO};

  /// The total work units. Zero disables the bar.
  uint64_t total{};

  /// The work units per displayed unit, which is how a bar counting
  /// samples still shows a person the pixel count they pictured.
  uint64_t displayScale{1};

  /// What to log on the way out when the bar never drew, without the
  /// elapsed time, e.g. `"Rendered 1280x720 at 64 spp"`. Empty logs
  /// nothing. This is the one line a captured or redirected run gets in
  /// place of the bar.
  std::string summary{};

  /// Where to write the machine-readable progress line, or empty for
  /// nowhere. This is the channel a tool watching the render reads:
  /// stderr belongs to the person at the terminal, and the bar draws
  /// nothing into a pipe on purpose.
  std::string filePath{};
};

/// Format seconds as `M:SS`, or `H:MM:SS` past an hour. This is how the
/// bar prints elapsed time and the ETA, and how the render times recorded
/// in the spectral output's header are logged.
[[nodiscard]] std::string formatDuration(double seconds);

/// A progress bar drawn in place on stderr, in the style of a package
/// manager's download bar.
///
/// The bar is only ever for a person watching a terminal, so it draws
/// nothing at all unless stderr is one (and the style asks for it). That
/// is not cosmetic: the tooling around this renderer captures stderr and
/// reads it back (the thumbnail batch greps it, the Blender add-on
/// reports its last line on failure), and a line of carriage returns and
/// percentages is worse than useless to both. Those runs get
/// `ProgressOptions::summary` at the end instead.
///
/// Progress is measured in whatever unit the caller counts, which for the
/// render loop is *samples* rather than pixels, so that the geometrically
/// growing passes of `-guide` read as one monotone bar. The counters are
/// displayed in units of `displayScale` of those.
///
/// `advance()` is callable from every worker thread: it is one relaxed
/// atomic add plus a clock read, and whichever thread first crosses the
/// redraw deadline claims it and draws while the others carry straight
/// on. No worker ever blocks on the terminal.
class ProgressBar final {
public:
  explicit ProgressBar(ProgressOptions options);

  ProgressBar(const ProgressBar &) = delete;

  ProgressBar &operator=(const ProgressBar &) = delete;

  /// Calls `finish()`, so that an exception thrown out of the loop being
  /// measured unwinds the bar off the terminal before anything catches
  /// it and prints.
  ~ProgressBar();

  /// Add completed work units. Callable from any thread.
  void advance(uint64_t amount = 1);

  /// Set the parenthesized note after the label, e.g. `"pass 3/7"`.
  void setNote(std::string note);

  /// Draw the final state and terminate the line, after which the bar is
  /// inert. Idempotent, and implied by the destructor.
  ///
  /// When the bar never drew, this is where `ProgressOptions::summary`
  /// is logged, so that a run whose output is captured still records how
  /// long the work took.
  void finish();

  /// Erase the bar, run `printer`, then redraw it, holding the bar's lock
  /// throughout so that the three steps cannot interleave with a redraw
  /// from a worker thread. This is how anything else gets to print while
  /// a render is running; see `ProgressLogSink`.
  template <typename Printer> void printThrough(Printer &&printer) {
    std::lock_guard<std::mutex> guard{mMutex};
    eraseLocked();
    printer();
    redrawLocked();
  }

  /// The bar currently on screen, or null if there is none. Null whenever
  /// the bar is disabled, so callers take their ordinary path.
  [[nodiscard]] static ProgressBar *active();

private:
  using Clock = std::chrono::steady_clock;

  /// Draw at `mDone`. Assumes the lock is held.
  void redrawLocked();

  /// Erase the bar from the line. Assumes the lock is held.
  void eraseLocked();

  /// The damped seconds remaining, negative until the estimate is worth
  /// anything and once the work is complete. Assumes the lock is held.
  [[nodiscard]] double
  secondsRemainingLocked(uint64_t done, double elapsedSeconds, double fraction);

  /// Format the ETA field, empty once the work is complete. Assumes the
  /// lock is held.
  [[nodiscard]] std::string etaLocked(uint64_t done, double elapsedSeconds,
                                      double fraction);

  /// Write `ProgressOptions::filePath`, through a temporary and a rename
  /// so that a reader never sees half a line. Assumes the lock is held.
  void reportLocked(uint64_t done);

  /// Build and write the line. Assumes the lock is held.
  void drawLocked(uint64_t done);

  /// Serializes drawing against `printThrough()`.
  std::mutex mMutex{};

  /// What to measure and how to draw it.
  ProgressOptions mOptions{};

  /// The note after the label (without parentheses).
  std::string mNote{};

  /// The completed work units.
  std::atomic<uint64_t> mDone{};

  /// The `Clock` tick at which the bar may next be drawn, as the raw
  /// representation so that it stays lock free. Whichever thread swaps it
  /// forward owns the redraw.
  std::atomic<int64_t> mNextDraw{};

  /// When the bar was constructed, for the elapsed time.
  Clock::time_point mStartTime{};

  /// The damped seconds remaining, negative before there is an estimate
  /// and once the work is done. Guarded by the mutex, since only a thread
  /// that is drawing or reporting touches it.
  double mRemaining{-1.0};

  /// Is stderr an interactive terminal, and is drawing therefore on?
  bool mEnabled{};

  /// Is there a file to write progress into?
  bool mReporting{};

  /// Does the environment claim UTF-8, and are the block drawing
  /// characters therefore safe?
  bool mUnicode{};

  /// Is there currently a bar on the line to erase?
  bool mOnScreen{};

  /// Has `finish()` run?
  bool mFinished{};
};

/// The stderr log sink `smdl-toy` installs in place of
/// `smdl::LogSinks::print_to_cerr`.
///
/// Identical to it when no bar is on screen. When there is one, the
/// message is printed through the bar, which is what keeps a warning
/// logged mid-render (from a worker thread, or from JIT'd material code)
/// both readable and above an intact bar.
class ProgressLogSink final : public smdl::LogSink {
public:
  void logMessage(smdl::LogLevel level, std::string_view message) final;
};
