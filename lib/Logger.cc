#include "smdl/Logger.h"

#include <iostream>

namespace smdl {

Logger &Logger::get() {
  static Logger logger{};
  return logger;
}

void Logger::reset() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks) {
    sink->flush();
    sink->close();
  }
  sinks.clear();
}

void Logger::flush() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks)
    sink->flush();
}

void Logger::close() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks)
    sink->close();
}

void Logger::log_message(LogLevel level, std::string_view message) {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks)
    sink->log_message(level, message);
}

namespace LogSinks {

static const char *Labels[]{"\033[36m[debug]\033[0m ", "",
                            "\033[33m[warn]\033[0m ",
                            "\033[91m[error]\033[0m "};

void print_to_cerr::log_message(LogLevel level, std::string_view message) {
  std::cerr << Labels[int(level)] << message << '\n';
}

void print_to_cout::log_message(LogLevel level, std::string_view message) {
  std::cout << Labels[int(level)] << message << '\n';
}

void print_to_cout::flush() { std::cout.flush(); }

} // namespace LogSinks

} // namespace smdl
