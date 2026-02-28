#include "error_reporter.h"
#include "color.hpp"
#include <cstddef>
#include <iostream>
#include <string>
#include <string_view>

using namespace std;

namespace shikimori {

static pair<size_t, size_t> find_line_boundaries(string_view source,
                                                 size_t pos) {
  size_t line_start = 0;
  for (size_t i = pos; i > 0; i--) {
    if (source[i - 1] == '\n') {
      line_start = i;
      break;
    }
  }

  size_t line_end = source.size();
  for (size_t i = pos; i < source.size(); i++) {
    if (source[i] == '\n') {
      line_end = i;
      break;
    }
  }

  return {line_start, line_end};
}

static size_t get_line_number(string_view source, size_t pos) {
  size_t line = 1;
  for (size_t i = 0; i < pos && i < source.size(); i++) {
    if (source[i] == '\n') {
      line++;
    }
  }
  return line;
}

// 1 indexed
static size_t get_column_number(string_view source, size_t pos) {
  auto [line_start, _] = find_line_boundaries(source, pos);
  return pos - line_start + 1;
}

static string_view get_line(string_view source, size_t line_num) {
  size_t current_line = 1;
  size_t start = 0;

  for (size_t i = 0; i < source.size(); i++) {
    if (source[i] == '\n') {
      if (current_line == line_num) {
        return source.substr(start, i - start);
      }
      current_line++;
      start = i + 1;
    }
  }

  if (current_line == line_num) {
    return source.substr(start);
  }

  return "";
}

void report_error(string_view source, const Span &span,
                  string_view error_type, string_view message) {
  const size_t line_num = get_line_number(source, span.start);
  const size_t col_start = get_column_number(source, span.start);
  const size_t col_end = get_column_number(source, span.end);

  // Print error header
  cerr << Color::BOLD << Color::RED << "error" << Color::RESET << Color::BOLD
       << ": " << error_type << Color::RESET << ": " << message << "\n";

  if (!span.file.empty()) {
    cerr << "  --> " << span.file.string() << ":" << line_num << ":" << col_start
         << "\n";
  }

  const size_t line_num_width = to_string(line_num + 1).length();

  if (line_num > 1) {
    string_view prev_line = get_line(source, line_num - 1);
    cerr << Color::GRAY << "  " << string(line_num_width, ' ') << " |\n"
         << "  " << (line_num - 1) << " | " << Color::RESET
         << prev_line << "\n";
  }

  string_view error_line = get_line(source, line_num);
  cerr << Color::GRAY << "  " << string(line_num_width, ' ') << " |\n"
       << "  " << line_num << " | " << Color::RESET << error_line << "\n";

  cerr << Color::GRAY << "  " << string(line_num_width, ' ') << " | "
       << Color::RESET;

  string_view before_error = error_line.substr(0, col_start - 1);
  size_t visual_col = before_error.length();

  cerr << string(visual_col, ' ');

  size_t error_len = col_end - col_start;
  if (error_len == 0)
    error_len = 1;

  string_view error_text = error_line.substr(col_start - 1, error_len);
  size_t visual_len = error_text.length();

  cerr << Color::BOLD << Color::RED << string(visual_len, '^') << Color::RESET
       << "\n";

  size_t total_lines = get_line_number(source, source.size());
  if (line_num < total_lines) {
    string_view next_line = get_line(source, line_num + 1);
    cerr << Color::GRAY << "  " << string(line_num_width, ' ') << " |\n"
         << "  " << (line_num + 1) << " | " << Color::RESET
         << next_line << "\n";
  }

  cerr << "\n";
}

} // namespace shikimori
