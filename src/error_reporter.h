#pragma once

#include <string>
#include <string_view>

#include "span.h"

namespace shikimori {

void report_error(std::string_view source, const Span &span,
                  std::string_view error_type, std::string_view message);

} // namespace shikimori
