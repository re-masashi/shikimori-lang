#pragma once

#include <algorithm>
#include <cstddef>
#include <utility>
#include <vector>

namespace shikimori {

struct Token;

struct Span {
  size_t start;
  size_t end;

  Span() : start(0), end(0) {}
  Span(size_t s, size_t e) : start(s), end(e) {}

  static Span merge(const Span &a, const Span &b) {
    return Span(std::min(a.start, b.start), std::max(a.end, b.end));
  }

  static Span from_token(const Token &token);

  static Span merge_vector(const std::vector<Span> &spans) {
    if (spans.empty())
      return Span();
    size_t min_start = spans[0].start;
    size_t max_end = spans[0].end;
    for (const auto &s : spans) {
      min_start = std::min(min_start, s.start);
      max_end = std::max(max_end, s.end);
    }
    return Span(min_start, max_end);
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
