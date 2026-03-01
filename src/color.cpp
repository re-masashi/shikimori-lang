#include "color.hpp"
#include <cctype>
#include <string>
#include <string_view>

using namespace std;

namespace shikimori {

static bool is_keyword(string_view s) {
  static const string_view keywords[] = {
      "fn",       "struct",    "union",     "interface", "extern",   "use",
      "macro",    "impl",      "let",       "return",    "defer",    "if",
      "else",     "where",     "match",     "loop",      "while",    "for",
      "in",       "comptime",  "as",        "break",     "continue",
      "true",     "false",     "null"};

  for (const auto &kw : keywords) {
    if (s == kw)
      return true;
  }
  return false;
}

static bool is_type_keyword(string_view s) {
  static const string_view types[] = {
      "i8",    "i16",   "i32",   "i64",   "u8",    "u16",
      "u32",   "u64",   "f32",   "f64",   "bool",  "usize",
      "string"};

  for (const auto &t : types) {
    if (s == t)
      return true;
  }
  return false;
}

static bool is_builtin(string_view s) {
  static const string_view builtins[] = {
      "@typename", "@typeid",   "@sizeof",  "@alignof",
      "@fields",   "@variants", "@has_field", "@field"};

  for (const auto &bi : builtins) {
    if (s == bi)
      return true;
  }
  return false;
}

string highlight_syntax(string_view source_line) {
  string result;
  size_t i = 0;
  const size_t len = source_line.size();

  while (i < len) {
    // Whitespace
    if (isspace(static_cast<unsigned char>(source_line[i]))) {
      result += source_line[i];
      i++;
      continue;
    }

    // Comments
    if (source_line[i] == '/' && i + 1 < len && source_line[i + 1] == '/') {
      result += Color::GRAY;
      while (i < len && source_line[i] != '\n') {
        result += source_line[i];
        i++;
      }
      result += Color::RESET;
      continue;
    }

    // Strings
    if (source_line[i] == '"') {
      result += Color::GREEN;
      result += source_line[i];
      i++;
      while (i < len && source_line[i] != '"') {
        if (source_line[i] == '\\' && i + 1 < len) {
          result += Color::CYAN;
          result += source_line[i];
          i++;
          result += Color::GREEN;
          if (i < len) {
            result += source_line[i];
            i++;
          }
        } else {
          result += source_line[i];
          i++;
        }
      }
      if (i < len) {
        result += source_line[i];
        i++;
      }
      result += Color::RESET;
      continue;
    }

    // Numbers (including hex, binary, octal prefixes)
    if (isdigit(static_cast<unsigned char>(source_line[i])) ||
        (source_line[i] == '0' && i + 1 < len &&
         (source_line[i + 1] == 'x' || source_line[i + 1] == 'b' ||
          source_line[i + 1] == 'o'))) {
      result += Color::CYAN;
      // Handle prefix (0x, 0b, 0o)
      if (source_line[i] == '0' && i + 1 < len &&
          (source_line[i + 1] == 'x' || source_line[i + 1] == 'b' ||
           source_line[i + 1] == 'o')) {
        result += source_line[i];
        i++;
        result += source_line[i];
        i++;
      }
      // Read the rest of the number
      while (i < len &&
             (isalnum(static_cast<unsigned char>(source_line[i])) ||
              source_line[i] == '_' || source_line[i] == '.')) {
        result += source_line[i];
        i++;
      }
      result += Color::RESET;
      continue;
    }

    // @ symbol (builtin prefix)
    if (source_line[i] == '@') {
      result += Color::MAGENTA;
      result += source_line[i];
      i++;
      // Read the identifier following @
      while (i < len &&
             (isalnum(static_cast<unsigned char>(source_line[i])) ||
              source_line[i] == '_')) {
        result += source_line[i];
        i++;
      }
      result += Color::RESET;
      continue;
    }

    // Identifiers and keywords
    if (isalpha(static_cast<unsigned char>(source_line[i])) ||
        source_line[i] == '_') {
      size_t start = i;
      while (i < len &&
             (isalnum(static_cast<unsigned char>(source_line[i])) ||
              source_line[i] == '_')) {
        i++;
      }
      string_view word = source_line.substr(start, i - start);

      if (is_keyword(word)) {
        result += Color::BOLD_BLUE;
        result += word;
        result += Color::RESET;
      } else if (is_type_keyword(word)) {
        result += Color::YELLOW;
        result += word;
        result += Color::RESET;
      } else if (is_builtin(word)) {
        result += Color::MAGENTA;
        result += word;
        result += Color::RESET;
      } else {
        result += word;
      }
      continue;
    }

    // Operators and punctuation
    switch (source_line[i]) {
    case '+':
    case '*':
    case '/':
    case '%':
    case '=':
    case '<':
    case '!':
    case '&':
    case '|':
    case '^':
    case '~':
      result += Color::BOLD_YELLOW;
      result += source_line[i];
      if (i + 1 < len && source_line[i] == source_line[i + 1]) {
        result += source_line[i + 1];
        i++;
      }
      result += Color::RESET;
      break;
    case '.':
      result += Color::WHITE;
      result += source_line[i];
      if (i + 1 < len && source_line[i + 1] == '.') {
        result += source_line[i + 1];
        i++;
        if (i + 1 < len && source_line[i + 1] == '=') {
          result += source_line[i + 1];
          i++;
        }
      }
      result += Color::RESET;
      break;
    case ':':
      result += Color::WHITE;
      result += source_line[i];
      if (i + 1 < len && source_line[i + 1] == ':') {
        result += source_line[i + 1];
        i++;
      }
      result += Color::RESET;
      break;
    case '?':
    case '#':
    case '-':
    case '>':
      result += source_line[i];
      break;
    default:
      result += source_line[i];
      break;
    }
    i++;
  }

  return result;
}

} // namespace shikimori
