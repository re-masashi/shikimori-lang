#include "tokenizer.hpp"
#include "color.hpp"
#include <cctype>
#include <unordered_map>

using namespace std;

#define DUMP_SIMPLE(tkn)                                                       \
  case TokenType::tkn: {                                                       \
    os << Color::CYAN << #tkn << Color::RESET;                                 \
    break;                                                                     \
  }

#define DUMP_VALUED(tkn)                                                       \
  case TokenType::tkn: {                                                       \
    if (type == TokenType::ERROR)                                              \
      os << Color::RED << #tkn;                                                \
    else if (type == TokenType::INT_LIT || type == TokenType::FLOAT_LIT)       \
      os << Color::YELLOW << #tkn;                                             \
    else                                                                       \
      os << Color::GREEN << #tkn;                                              \
    os << "(" << lexeme << ")" << Color::RESET;                                \
    break;                                                                     \
  }

#define MAP(f, ...) EVAL(MAP1(f, __VA_ARGS__, ()()(), ()()(), ()()(), 0))

namespace shikimori {

void Token::dump(ostream &os) const {
  os << "Token: ";
  switch (type) {
    MAP(DUMP_SIMPLE, TOKEN_TYPES_SIMPLE)
    MAP(DUMP_VALUED, TOKEN_TYPES_VALUED)
  default:
    os << Color::RED << "UNKNOWN" << Color::RESET;
  }
  os << Color::GRAY << " SPAN " << start << ".." << end << Color::RESET << "\n";
}

Tokenizer::Tokenizer(string_view source)
    : source(source), position(0), line(1), column(1) {}

vector<Token> Tokenizer::tokenize() {
  vector<Token> tokens;
  while (!is_at_end()) {
    Token token = next_token();
    tokens.push_back(token);
    if (token.type == TokenType::EOF_TOKEN) {
      break;
    }
  }
  return tokens;
}

Token Tokenizer::next_token() {
  skip_whitespace();

  if (is_at_end()) {
    return Token(TokenType::EOF_TOKEN, "", position, position);
  }

  return read_next_token();
}

char Tokenizer::current() const {
  if (is_at_end())
    return '\0';
  return source[position];
}

char Tokenizer::peek(size_t offset) const {
  if (position + offset >= source.length())
    return '\0';
  return source[position + offset];
}

void Tokenizer::advance() {
  if (!is_at_end()) {
    if (source[position] == '\n') {
      line++;
      column = 1;
    } else {
      column++;
    }
    position++;
  }
}

void Tokenizer::skip_whitespace() {
  while (!is_at_end() && isspace(current())) {
    advance();
  }
}

void Tokenizer::skip_comment() {
  if (current() == '/' && peek() == '/') {
    // Line comment
    while (!is_at_end() && current() != '\n') {
      advance();
    }
    if (!is_at_end())
      advance(); // consume the newline
  } else if (current() == '/' && peek() == '*') {
    // Block comment
    advance(); // consume /
    advance(); // consume *
    while (!is_at_end()) {
      if (current() == '*' && peek() == '/') {
        advance(); // consume *
        advance(); // consume /
        break;
      }
      advance();
    }
  }
}

bool Tokenizer::is_at_end() const { return position >= source.length(); }

bool Tokenizer::is_digit(char c) { return c >= '0' && c <= '9'; }

bool Tokenizer::is_letter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool Tokenizer::is_identifier_start(char c) { return is_letter(c); }

bool Tokenizer::is_identifier_continue(char c) {
  return is_letter(c) || is_digit(c);
}

TokenType Tokenizer::get_keyword_type(string_view lexeme) {
  static const unordered_map<string_view, TokenType> keywords = {
      {"fn", TokenType::KW_FN},
      {"struct", TokenType::KW_STRUCT},
      {"union", TokenType::KW_UNION},
      {"interface", TokenType::KW_INTERFACE},
      {"extern", TokenType::KW_EXTERN},
      {"use", TokenType::KW_USE},
      {"where", TokenType::KW_WHERE},
      {"macro", TokenType::KW_MACRO},
      {"impl", TokenType::KW_IMPL},
      {"let", TokenType::KW_LET},
      {"return", TokenType::KW_RETURN},
      {"defer", TokenType::KW_DEFER},
      {"if", TokenType::KW_IF},
      {"else", TokenType::KW_ELSE},
      {"match", TokenType::KW_MATCH},
      {"loop", TokenType::KW_LOOP},
      {"while", TokenType::KW_WHILE},
      {"for", TokenType::KW_FOR},
      {"in", TokenType::KW_IN},
      {"comptime", TokenType::KW_COMPTIME},
      {"as", TokenType::KW_AS},
      {"break", TokenType::KW_BREAK},
      {"continue", TokenType::KW_CONTINUE},
      {"null", TokenType::KW_NULL},
      {"true", TokenType::BOOL_LIT},
      {"false", TokenType::BOOL_LIT},
      {"i8", TokenType::KW_I8},
      {"i16", TokenType::KW_I16},
      {"i32", TokenType::KW_I32},
      {"i64", TokenType::KW_I64},
      {"u8", TokenType::KW_U8},
      {"u16", TokenType::KW_U16},
      {"u32", TokenType::KW_U32},
      {"u64", TokenType::KW_U64},
      {"f32", TokenType::KW_F32},
      {"f64", TokenType::KW_F64},
      {"bool", TokenType::KW_BOOL},
      {"usize", TokenType::KW_USIZE},
      {"string", TokenType::KW_STRING},
  };

  auto it = keywords.find(lexeme);
  return it != keywords.end() ? it->second : TokenType::IDENT;
}

Token Tokenizer::read_identifier() {
  size_t start_pos = position;

  while (!is_at_end() && is_identifier_continue(current())) {
    advance();
  }

  string lexeme(source.substr(start_pos, position - start_pos));
  TokenType type = get_keyword_type(lexeme);

  return Token(type, lexeme, start_pos, position);
}

Token Tokenizer::read_number() {
  size_t start_pos = position;
  bool is_float = false;

  // Check for prefixed integers: 0b, 0x, 0o
  if (current() == '0' && !is_at_end()) {
    char next = peek();
    if (next == 'b' || next == 'B') {
      advance(); // consume '0'
      advance(); // consume 'b'
      while (!is_at_end() &&
             (current() == '0' || current() == '1' || current() == '_')) {
        advance();
      }
      string lexeme(source.substr(start_pos, position - start_pos));
      Token token(TokenType::INT_LIT, lexeme, start_pos, position);
      // Strip underscores and parse
      string clean;
      for (size_t i = 2; i < lexeme.size(); i++) {
        if (lexeme[i] != '_')
          clean += lexeme[i];
      }
      token.int_value = static_cast<int64_t>(stoull(clean, nullptr, 2));
      return token;
    }
    if (next == 'x' || next == 'X') {
      advance(); // consume '0'
      advance(); // consume 'x'
      while (!is_at_end() &&
             (is_digit(current()) || (current() >= 'a' && current() <= 'f') ||
              (current() >= 'A' && current() <= 'F') || current() == '_')) {
        advance();
      }
      string lexeme(source.substr(start_pos, position - start_pos));
      Token token(TokenType::INT_LIT, lexeme, start_pos, position);
      string clean;
      for (size_t i = 2; i < lexeme.size(); i++) {
        if (lexeme[i] != '_')
          clean += lexeme[i];
      }
      token.int_value = static_cast<int64_t>(stoull(clean, nullptr, 16));
      return token;
    }
    if (next == 'o' || next == 'O') {
      advance(); // consume '0'
      advance(); // consume 'o'
      while (!is_at_end() &&
             ((current() >= '0' && current() <= '7') || current() == '_')) {
        advance();
      }
      string lexeme(source.substr(start_pos, position - start_pos));
      Token token(TokenType::INT_LIT, lexeme, start_pos, position);
      string clean;
      for (size_t i = 2; i < lexeme.size(); i++) {
        if (lexeme[i] != '_')
          clean += lexeme[i];
      }
      token.int_value = static_cast<int64_t>(stoull(clean, nullptr, 8));
      return token;
    }
  }

  // Decimal: read digits with underscore separators
  while (!is_at_end() && (is_digit(current()) || current() == '_')) {
    advance();
  }

  // Check for decimal point (not range operator ..)
  if (!is_at_end() && current() == '.' && peek() != '.' && is_digit(peek())) {
    is_float = true;
    advance(); // consume .
    while (!is_at_end() && (is_digit(current()) || current() == '_')) {
      advance();
    }
  }

  // Check for exponent
  if (!is_at_end() && (current() == 'e' || current() == 'E')) {
    is_float = true;
    advance(); // consume e/E
    if (!is_at_end() && (current() == '+' || current() == '-')) {
      advance();
    }
    while (!is_at_end() && (is_digit(current()) || current() == '_')) {
      advance();
    }
  }

  string lexeme(source.substr(start_pos, position - start_pos));
  // Strip underscores for parsing
  string clean;
  for (char c : lexeme) {
    if (c != '_')
      clean += c;
  }
  Token token(is_float ? TokenType::FLOAT_LIT : TokenType::INT_LIT, lexeme,
              start_pos, position);

  if (is_float) {
    token.float_value = stod(clean);
  } else {
    token.int_value = stoll(clean);
  }

  return token;
}

Token Tokenizer::read_string() {
  size_t start_pos = position;
  char quote_char = current();
  advance(); // consume opening quote

  // size_t start_pos = position;
  string result;

  while (!is_at_end() && current() != quote_char) {
    if (current() == '\\') {
      advance();
      if (!is_at_end()) {
        switch (current()) {
        case 'n':
          result += '\n';
          break;
        case 't':
          result += '\t';
          break;
        case 'r':
          result += '\r';
          break;
        case '\\':
          result += '\\';
          break;
        case '"':
          result += '"';
          break;
        case '\'':
          result += '\'';
          break;
        default:
          result += current();
          break;
        }
        advance();
      }
    } else {
      result += current();
      advance();
    }
  }

  if (is_at_end()) {
    return Token(TokenType::ERROR, "Unterminated string", start_pos, position);
  }

  advance(); // consume closing quote
  return Token(TokenType::STRING_LIT, result, start_pos, position);
}

Token Tokenizer::read_next_token() {
  size_t start_pos = position;
  char ch = current();

  // Skip comments
  if (ch == '/' && (peek() == '/' || peek() == '*')) {
    skip_comment();
    return next_token();
  }

  // String literals
  if (ch == '"' || ch == '\'') {
    return read_string();
  }

  // Identifiers and keywords
  if (is_identifier_start(ch)) {
    return read_identifier();
  }

  // Numbers
  if (is_digit(ch)) {
    return read_number();
  }

  // Single and multi character operators
  advance();

  switch (ch) {
  case '(':
    return Token(TokenType::LPAREN, "(", start_pos, position);
  case ')':
    return Token(TokenType::RPAREN, ")", start_pos, position);
  case '{':
    return Token(TokenType::LBRACE, "{", start_pos, position);
  case '}':
    return Token(TokenType::RBRACE, "}", start_pos, position);
  case '[':
    return Token(TokenType::LBRACKET, "[", start_pos, position);
  case ']':
    return Token(TokenType::RBRACKET, "]", start_pos, position);
  case ',':
    return Token(TokenType::COMMA, ",", start_pos, position);
  case ';':
    return Token(TokenType::SEMICOLON, ";", start_pos, position);
  case '?':
    return Token(TokenType::QUESTION, "?", start_pos, position);
  case '~':
    return Token(TokenType::TILDE, "~", start_pos, position);
  case '@':
    return Token(TokenType::AT, "@", start_pos, position);

  case ':':
    if (current() == ':') {
      advance();
      return Token(TokenType::DOUBLE_COLON, "::", start_pos, position);
    }
    return Token(TokenType::COLON, ":", start_pos, position);

  case '.':
    if (current() == '.') {
      advance();
      if (current() == '=') {
        advance();
        return Token(TokenType::DOT_DOT_EQ, "..=", start_pos, position);
      }
      return Token(TokenType::DOT_DOT, "..", start_pos, position);
    }
    return Token(TokenType::DOT, ".", start_pos, position);

  case '#':
    if (current() == '[') {
      advance();
      return Token(TokenType::HASH_BRACK, "#[", start_pos, position);
    }
    return Token(TokenType::ERROR, string(1, ch), start_pos, position);

  case '-':
    if (current() == '>') {
      advance();
      return Token(TokenType::ARROW, "->", start_pos, position);
    }
    return Token(TokenType::MINUS, "-", start_pos, position);

  case '+':
    return Token(TokenType::PLUS, "+", start_pos, position);

  case '*':
    return Token(TokenType::STAR, "*", start_pos, position);

  case '/':
    return Token(TokenType::SLASH, "/", start_pos, position);

  case '%':
    return Token(TokenType::PERCENT, "%", start_pos, position);

  case '=':
    if (current() == '=') {
      advance();
      return Token(TokenType::EQ, "==", start_pos, position);
    }
    if (current() == '>') {
      advance();
      return Token(TokenType::FAT_ARROW, "=>", start_pos, position);
    }
    return Token(TokenType::ASSIGN, "=", start_pos, position);

  case '!':
    if (current() == '=') {
      advance();
      return Token(TokenType::NEQ, "!=", start_pos, position);
    }
    return Token(TokenType::NOT, "!", start_pos, position);

  case '<':
    if (current() == '=') {
      advance();
      return Token(TokenType::LTE, "<=", start_pos, position);
    } else if (current() == '<') {
      advance();
      return Token(TokenType::LSHIFT, "<<", start_pos, position);
    }
    return Token(TokenType::LT, "<", start_pos, position);

  case '>':
    if (current() == '=') {
      advance();
      return Token(TokenType::GTE, ">=", start_pos, position);
    } else if (current() == '>') {
      advance();
      return Token(TokenType::RSHIFT, ">>", start_pos, position);
    }
    return Token(TokenType::GT, ">", start_pos, position);

  case '&':
    if (current() == '&') {
      advance();
      return Token(TokenType::AND, "&&", start_pos, position);
    }
    return Token(TokenType::BIT_AND, "&", start_pos, position);

  case '|':
    if (current() == '|') {
      advance();
      return Token(TokenType::OR, "||", start_pos, position);
    }
    return Token(TokenType::BIT_OR, "|", start_pos, position);

  case '^':
    return Token(TokenType::BIT_XOR, "^", start_pos, position);

  case '_':
    return Token(TokenType::UNDERSCORE, "_", start_pos, position);

  default:
    return Token(TokenType::ERROR, string(1, ch), start_pos, position);
  }
}

} // namespace shikimori
