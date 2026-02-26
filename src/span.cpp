#include "span.h"
#include "parser/tokenizer.hpp"

namespace shikimori {

Span Span::from_token(const Token &token) {
  return Span(token.start, token.end);
}

} // namespace shikimori
