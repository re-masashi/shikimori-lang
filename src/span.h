#pragma once

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <string>
#include <utility>
#include <vector>

namespace shikimori {

struct Token;

struct Span {
  size_t start;
  size_t end;
  std::filesystem::path file;

  Span() : start(0), end(0), file("") {}
  Span(size_t s, size_t e, std::filesystem::path f = "")
      : start(s), end(e), file(std::move(f)) {}

  static Span merge(const Span &a, const Span &b) {
    return Span(std::min(a.start, b.start), std::max(a.end, b.end),
                a.file.empty() ? b.file : a.file);
  }

  static Span from_token(const Token &token);

  static Span merge_vector(const std::vector<Span> &spans) {
    if (spans.empty())
      return Span();
    size_t min_start = spans[0].start;
    size_t max_end = spans[0].end;
    std::filesystem::path file = spans[0].file;
    for (const auto &s : spans) {
      min_start = std::min(min_start, s.start);
      max_end = std::max(max_end, s.end);
      if (file.empty() && !s.file.empty()) {
        file = s.file;
      }
    }
    return Span(min_start, max_end, file);
  }
};

template <typename T> struct Spanned {
  T value;
  Span span;

  Spanned() = default;
  Spanned(T v, Span s) : value(std::move(v)), span(s) {}

  template <typename U> static Spanned wrap(U &&v, const Span &s) {
    return Spanned(T(std::forward<U>(v)), s);
  }
};

} // namespace shikimori
