#pragma once

#include <ostream>

using namespace std;

namespace shikimori {

struct Color {
  static constexpr const char *RESET = "\033[0m";
  static constexpr const char *RED = "\033[31m";
  static constexpr const char *GREEN = "\033[32m";
  static constexpr const char *YELLOW = "\033[33m";
  static constexpr const char *BLUE = "\033[34m";
  static constexpr const char *MAGENTA = "\033[35m";
  static constexpr const char *CYAN = "\033[36m";
  static constexpr const char *WHITE = "\033[37m";
  static constexpr const char *GRAY = "\033[90m";
  static constexpr const char *BOLD = "\033[1m";
  static constexpr const char *BOLD_RED = "\033[1;31m";
  static constexpr const char *BOLD_GREEN = "\033[1;32m";
  static constexpr const char *BOLD_YELLOW = "\033[1;33m";
  static constexpr const char *BOLD_BLUE = "\033[1;34m";
};

class ColoredOstream {
public:
  explicit ColoredOstream(ostream &os) : os(os), enabled(true) {}

  template <typename T> ColoredOstream &operator<<(const T &val) {
    if (enabled)
      os << val;
    return *this;
  }

  ColoredOstream &operator<<(const char *val) {
    if (enabled)
      os << val;
    return *this;
  }

  ColoredOstream &flush() {
    os.flush();
    return *this;
  }

  ColoredOstream &enable() {
    enabled = true;
    return *this;
  }
  ColoredOstream &disable() {
    enabled = false;
    return *this;
  }
  bool is_enabled() const { return enabled; }
  ostream &underlying() { return os; }

private:
  ostream &os;
  bool enabled;
};

} // namespace shikimori
