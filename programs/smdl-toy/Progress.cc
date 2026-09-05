#include "Progress.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>

#include "llvm/Support/Process.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"

namespace {

// How often the bar may redraw. About 10 Hz, which reads as continuous
// motion and costs nothing. Deliberately a time and not a count of work
// units: the same render loop covers a 64x64 thumbnail and a 4K frame.
constexpr auto REDRAW_INTERVAL{std::chrono::milliseconds(100)};

// The narrowest bar worth drawing. Below this the ladder in `drawLocked()`
// drops a field instead.
constexpr size_t MIN_BAR_WIDTH{10};

// The widest bar worth drawing, so that a very wide terminal gets a
// readable line rather than a runway.
constexpr size_t MAX_BAR_WIDTH{48};

// The columns between fields.
constexpr size_t SEPARATOR_WIDTH{2};

// The columns assumed when the terminal will not say how wide it is.
constexpr size_t FALLBACK_COLUMNS{80};

// The weight each estimate carries in the smoothed time remaining.
// Work does not complete evenly within a pass (whole pixels land at
// once, worker threads finish in bursts, and a preview checkpoint stops
// completions entirely while it writes), so the estimate is damped
// rather than shown raw. Measured over a 25 second render, this takes
// the mean change between readings from 0.14 to 0.12 seconds and the
// largest from 1.11 to 0.37.
constexpr double REMAINING_SMOOTHING{0.25};

// How far in before the ETA is shown at all. An estimate from the first
// few samples of a render is worse than no estimate, and reads as one
// because it moves the moment it appears.
constexpr double ETA_MIN_ELAPSED{1.0};
constexpr double ETA_MIN_FRACTION{0.02};

constexpr const char *COLOR_BAR{"\033[36m"};
constexpr const char *COLOR_DIM{"\033[2m"};
constexpr const char *COLOR_OFF{"\033[0m"};

// Is stderr a terminal that can act on carriage returns and ANSI escape
// codes? `cerrSupportsANSIColors()` is the `isatty()` test; `TERM` rules
// out the terminals that are interactive but cannot erase a line, where
// a bar would leave one line of wreckage per redraw.
[[nodiscard]] bool stderrIsInteractive() {
  if (!smdl::cerrSupportsANSIColors()) return false;
  const char *term{std::getenv("TERM")};
  return term && *term && std::strcmp(term, "dumb") != 0;
}

// Does the environment claim UTF-8? Tests the locale variables in the
// order the C library resolves them, first non-empty wins, so that a
// specific `LC_CTYPE` is not overruled by a stale `LANG`.
[[nodiscard]] bool localeIsUTF8() {
  for (const char *name : {"LC_ALL", "LC_CTYPE", "LANG"}) {
    const char *value{std::getenv(name)};
    if (!value || !*value) continue;
    auto str{std::string(value)};
    for (auto &ch : str)
      ch = char(std::tolower(static_cast<unsigned char>(ch)));
    return str.find("utf-8") != std::string::npos ||
           str.find("utf8") != std::string::npos;
  }
  return false;
}

// Round a duration to a step a person can read without watching it
// twitch. The larger the estimate, the less a second of it matters and
// the coarser the step it moves in.
[[nodiscard]] double coarsenDuration(double seconds) {
  const double step{seconds < 20.0    ? 1.0
                    : seconds < 120.0 ? 5.0
                    : seconds < 600.0 ? 15.0
                                      : 60.0};
  return std::round(seconds / step) * step;
}

// Format a count in at most 5 columns: `999`, `436k`, `8.3M`, `1.2G`.
[[nodiscard]] std::string formatCount(uint64_t value) {
  const auto scaled{[](uint64_t whole, uint64_t tenths, char suffix) {
    auto str{std::to_string(whole)};
    if (whole < 10) {
      str += '.';
      str += char('0' + tenths);
    }
    str += suffix;
    return str;
  }};
  if (value < 1000) return std::to_string(value);
  if (value < 1000000) return scaled(value / 1000, (value / 100) % 10, 'k');
  if (value < 1000000000)
    return scaled(value / 1000000, (value / 100000) % 10, 'M');
  return scaled(value / 1000000000, (value / 100000000) % 10, 'G');
}

// The bar itself, drawn to half a cell, colored.
[[nodiscard]] std::string renderBar(double fraction, size_t width,
                                    bool unicode) {
  // The ASCII form spends two of its columns on the brackets that give it
  // an edge to fill up to; the Unicode form draws the unfilled run as a
  // dim rule instead, so all of its columns are bar.
  const size_t cells{unicode ? width : width - 2};
  const double filled{std::clamp(fraction, 0.0, 1.0) * double(cells)};
  const auto full{std::min(size_t(filled), cells)};
  const bool half{filled - double(full) >= 0.5 && full < cells};
  const auto empty{cells - full - size_t(half)};
  auto str{std::string()};
  if (unicode) {
    str += COLOR_BAR;
    for (size_t i = 0; i < full; i++) str += "━";
    if (half) str += "╸";
    str += COLOR_OFF;
    str += COLOR_DIM;
    for (size_t i = 0; i < empty; i++) str += "━";
    str += COLOR_OFF;
  } else {
    str += '[';
    str += COLOR_BAR;
    str.append(full, '#');
    if (half) str += '>';
    str += COLOR_OFF;
    str.append(empty, ' ');
    str += ']';
  }
  return str;
}

// The bar currently on screen. There is at most one: nesting two would
// mean two things drawing to the same line.
std::atomic<ProgressBar *> sActive{nullptr};

} // namespace

[[nodiscard]] std::string formatDuration(double seconds) {
  auto ticks{uint64_t(std::max(seconds, 0.0))};
  const auto hours{ticks / 3600};
  const auto minutes{(ticks / 60) % 60};
  const auto secs{ticks % 60};
  auto str{std::string()};
  if (hours > 0) {
    str += std::to_string(hours);
    str += ':';
    if (minutes < 10) str += '0';
  }
  str += std::to_string(minutes);
  str += ':';
  if (secs < 10) str += '0';
  str += std::to_string(secs);
  return str;
}

void validateProgressOptions(const ProgressOptions &options) {
  if (options.style != "auto" && options.style != "plain" &&
      options.style != "none")
    throw smdl::Error(
        smdl::concat("expected the progress style to be 'auto', 'plain', or "
                     "'none', not ",
                     smdl::Quoted(options.style)));
}

ProgressBar::ProgressBar(ProgressOptions options)
    : mOptions(std::move(options)), mStartTime(Clock::now()) {
  mOptions.displayScale = std::max<uint64_t>(mOptions.displayScale, 1);
  mEnabled =
      mOptions.total > 0 && mOptions.style != "none" && stderrIsInteractive();
  // 'plain' is the escape hatch for a terminal that claims UTF-8 and then
  // draws the block characters as boxes.
  mUnicode = mOptions.style == "auto" && localeIsUTF8();
  mReporting = !mOptions.filePath.empty() && mOptions.total > 0;
  if (mReporting) {
    std::lock_guard<std::mutex> guard{mMutex};
    reportLocked(0);
  }
  if (!mEnabled) return;
  sActive.store(this, std::memory_order_release);
  // Draw the empty bar immediately, so that the terminal says something
  // is happening before the first unit of work completes, which for one
  // pixel of a heavy scene can be a while.
  std::lock_guard<std::mutex> guard{mMutex};
  drawLocked(0);
  mNextDraw.store((Clock::now() + REDRAW_INTERVAL).time_since_epoch().count(),
                  std::memory_order_relaxed);
}

ProgressBar::~ProgressBar() { finish(); }

void ProgressBar::advance(uint64_t amount) {
  mDone.fetch_add(amount, std::memory_order_relaxed);
  if (!mEnabled && !mReporting) return;
  const int64_t now{Clock::now().time_since_epoch().count()};
  int64_t nextDraw{mNextDraw.load(std::memory_order_relaxed)};
  if (now < nextDraw) return;
  // Whichever thread swaps the deadline forward owns this redraw; every
  // other one is already past this point and back at work.
  const int64_t interval{
      std::chrono::duration_cast<Clock::duration>(REDRAW_INTERVAL).count()};
  if (!mNextDraw.compare_exchange_strong(nextDraw, now + interval,
                                         std::memory_order_relaxed))
    return;
  std::lock_guard<std::mutex> guard{mMutex};
  redrawLocked();
  reportLocked(mDone.load(std::memory_order_relaxed));
}

void ProgressBar::setNote(std::string note) {
  if (!mEnabled && !mReporting) return;
  std::lock_guard<std::mutex> guard{mMutex};
  mNote = std::move(note);
  redrawLocked();
  reportLocked(mDone.load(std::memory_order_relaxed));
}

void ProgressBar::finish() {
  auto summary{std::string()};
  {
    std::lock_guard<std::mutex> guard{mMutex};
    if (mFinished) return;
    mFinished = true;
    // The last word to whatever is watching the file, so that a reader
    // polling it sees the run land rather than inferring it from silence.
    reportLocked(mDone.load(std::memory_order_relaxed));
    if (mEnabled) {
      // The last state is drawn rather than a forced 100%: on the way out
      // of an exception this is the honest number, and on the way out of
      // a completed loop it is 100% anyway.
      drawLocked(mDone.load(std::memory_order_relaxed));
      std::cerr << '\n';
      mOnScreen = false;
      sActive.store(nullptr, std::memory_order_release);
    } else if (mOptions.total > 0 && !mOptions.summary.empty()) {
      summary = smdl::concat(mOptions.summary, " in ",
                             formatDuration(std::chrono::duration<double>(
                                                Clock::now() - mStartTime)
                                                .count()));
    }
  }
  // Logged outside the lock, because a log message is entitled to reach
  // for the bar it is stepping around.
  if (!summary.empty()) SMDL_LOG_INFO(summary);
}

ProgressBar *ProgressBar::active() {
  return sActive.load(std::memory_order_acquire);
}

void ProgressBar::redrawLocked() {
  if (!mFinished) drawLocked(mDone.load(std::memory_order_relaxed));
}

void ProgressBar::eraseLocked() {
  if (!mOnScreen) return;
  std::cerr << "\r\033[K";
  mOnScreen = false;
}

double ProgressBar::secondsRemainingLocked(uint64_t done, double elapsedSeconds,
                                           double fraction) {
  if (done >= mOptions.total || done == 0 || elapsedSeconds <= 0.0 ||
      elapsedSeconds < ETA_MIN_ELAPSED || fraction < ETA_MIN_FRACTION) {
    if (done >= mOptions.total) mRemaining = -1.0;
    return -1.0;
  }
  // Nothing here is sampled adaptively: every pass sweeps every pixel, so
  // the work left is the work already done over again, and the whole
  // run's own average predicts it. That average is the estimate that
  // keeps improving as the render goes, where a trailing rate measures
  // mostly which burst of completions it happened to catch. Scored
  // against the truth on a plain and a guided render, it was at least as
  // accurate as a trailing rate and simpler by a whole mechanism.
  const double remaining{elapsedSeconds * double(mOptions.total - done) /
                         double(done)};
  mRemaining = mRemaining < 0.0 ? remaining
                                : (1.0 - REMAINING_SMOOTHING) * mRemaining +
                                      REMAINING_SMOOTHING * remaining;
  return mRemaining;
}

std::string ProgressBar::etaLocked(uint64_t done, double elapsedSeconds,
                                   double fraction) {
  const double remaining{
      secondsRemainingLocked(done, elapsedSeconds, fraction)};
  if (done >= mOptions.total) return {};
  if (remaining < 0.0) return "eta --:--";
  return "eta " + formatDuration(coarsenDuration(remaining));
}

void ProgressBar::reportLocked(uint64_t done) {
  if (!mReporting) return;
  done = std::min(done, mOptions.total);
  const auto now{Clock::now()};
  const double elapsed{std::chrono::duration<double>(now - mStartTime).count()};
  const double fraction{double(done) / double(mOptions.total)};
  const double remaining{secondsRemainingLocked(done, elapsed, fraction)};
  // Written whole and renamed into place, because the reader is polling:
  // a truncate-and-write would hand it half a line often enough to see.
  const auto partPath{mOptions.filePath + ".part"};
  {
    auto stream{std::ofstream(partPath, std::ios::trunc)};
    if (!stream) return;
    stream << "done=" << done << " total=" << mOptions.total
           << " elapsed=" << std::fixed << std::setprecision(2) << elapsed
           << " eta=" << remaining << " note=" << mNote << '\n';
  }
  // A progress line is not worth failing a render over, so a rename
  // that loses a race with a reader is dropped rather than reported.
  (void)smdl::tryRenameOnto(partPath, mOptions.filePath);
}

void ProgressBar::drawLocked(uint64_t done) {
  if (!mEnabled) return;
  done = std::min(done, mOptions.total);
  const double fraction{double(done) / double(mOptions.total)};
  auto label{mOptions.label};
  if (!mNote.empty()) label += " (" + mNote + ")";
  auto percent{std::to_string(uint64_t(fraction * 100.0)) + "%"};
  percent.insert(percent.begin(), 4 - std::min<size_t>(percent.size(), 4), ' ');
  // The completed counter is padded to the width of the total, so that
  // the fields to its left keep still as it counts up instead of drifting
  // a column at every power of ten.
  auto doneText{formatCount(done / mOptions.displayScale)};
  const auto totalText{formatCount(mOptions.total / mOptions.displayScale)};
  if (doneText.size() < totalText.size())
    doneText.insert(doneText.begin(), totalText.size() - doneText.size(), ' ');
  const auto counts{
      doneText + "/" + totalText +
      (mOptions.units.empty() ? std::string() : " " + mOptions.units)};
  const auto now{Clock::now()};
  const double elapsedSeconds{
      std::chrono::duration<double>(now - mStartTime).count()};
  const auto elapsed{formatDuration(elapsedSeconds)};
  const auto eta{etaLocked(done, elapsedSeconds, fraction)};
  // The terminal width is re-read on every draw, which is what makes a
  // resize mid-render correct itself. One column is held back because a
  // write into the last one wraps in some terminals, and a bar that wraps
  // leaves a trail of dead lines behind it.
  const unsigned columns{llvm::sys::Process::StandardErrColumns()};
  const size_t budget{(columns == 0 ? FALLBACK_COLUMNS : size_t(columns)) - 1};
  // Fields drop in order of what a person can most afford to lose, and
  // the bar keeps a floor, so that a narrow window degrades instead of
  // wrapping.
  bool showLabel{true}, showCounts{true}, showElapsed{true}, showETA{true};
  const auto solveBarWidth{[&]() -> size_t {
    size_t used{percent.size() + SEPARATOR_WIDTH};
    if (showLabel) used += label.size() + SEPARATOR_WIDTH;
    if (showCounts) used += counts.size() + SEPARATOR_WIDTH;
    if (showElapsed) used += elapsed.size() + SEPARATOR_WIDTH;
    if (showETA && !eta.empty()) used += eta.size() + SEPARATOR_WIDTH;
    return used + MIN_BAR_WIDTH <= budget ? budget - used : 0;
  }};
  size_t barWidth{solveBarWidth()};
  if (barWidth == 0) {
    showETA = false;
    barWidth = solveBarWidth();
  }
  if (barWidth == 0) {
    showCounts = false;
    barWidth = solveBarWidth();
  }
  if (barWidth == 0) {
    showElapsed = false;
    barWidth = solveBarWidth();
  }
  if (barWidth == 0) {
    showLabel = false;
    barWidth = solveBarWidth();
  }
  barWidth = std::min(barWidth, MAX_BAR_WIDTH);
  auto line{std::string("\r")};
  if (barWidth > 0) {
    if (showLabel) line += label + "  ";
    line += renderBar(fraction, barWidth, mUnicode);
    line += "  ";
  } else if (budget < percent.size()) {
    // Narrower than "100%". Nothing legible fits, so leave the line to
    // whatever else wants it.
    eraseLocked();
    return;
  }
  line += percent;
  const auto dim{[&](const std::string &field) {
    line += "  " + std::string(COLOR_DIM) + field + COLOR_OFF;
  }};
  if (showCounts) dim(counts);
  if (showElapsed) dim(elapsed);
  if (showETA && !eta.empty()) dim(eta);
  // Erase to the end of the line on every draw, so that a bar which
  // shrinks (a narrower window, a dropped field) does not leave the tail
  // of the previous one behind.
  line += "\033[K";
  std::cerr << line;
  mOnScreen = true;
}

void ProgressLogSink::logMessage(smdl::LogLevel level,
                                 std::string_view message) {
  static const bool WithColors{smdl::cerrSupportsANSIColors()};
  const auto write{[&] {
    std::cerr << smdl::logLevelLabel(level, WithColors) << message << '\n';
  }};
  if (auto *bar{ProgressBar::active()}) {
    bar->printThrough(write);
  } else {
    write();
  }
}
