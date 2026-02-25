#include "tokenizer.hpp"
#include <cctype>
#include <unordered_map>

namespace shikimori {

void Token::dump(std::ostream &os) const {
  os << "Token: ";
  switch (type) {
  case TokenType::INT_LIT:
    os << "INT_LIT(" << lexeme << ")";
    break;
  case TokenType::FLOAT_LIT:
    os << "FLOAT_LIT(" << lexeme << ")";
    break;
  case TokenType::STRING_LIT:
    os << "STRING_LIT(\"" << lexeme << "\")";
    break;
  case TokenType::BOOL_LIT:
    os << "BOOL_LIT(" << lexeme << ")";
    break;
  case TokenType::IDENT:
    os << "IDENT(" << lexeme << ")";
    break;
  case TokenType::KW_FN:
    os << "KW_FN";
    break;
  case TokenType::KW_STRUCT:
    os << "KW_STRUCT";
    break;
  case TokenType::KW_UNION:
    os << "KW_UNION";
    break;
  case TokenType::KW_INTERFACE:
    os << "KW_INTERFACE";
    break;
  case TokenType::KW_EXTERN:
    os << "KW_EXTERN";
    break;
  case TokenType::KW_USE:
    os << "KW_USE";
    break;
  case TokenType::KW_WHERE:
    os << "KW_USE";
    break;

  case TokenType::KW_MACRO:
    os << "KW_MACRO";
    break;
  case TokenType::KW_LET:
    os << "KW_LET";
    break;
  case TokenType::KW_RETURN:
    os << "KW_RETURN";
    break;
  case TokenType::KW_DEFER:
    os << "KW_DEFER";
    break;
  case TokenType::KW_IF:
    os << "KW_IF";
    break;
  case TokenType::KW_ELSE:
    os << "KW_ELSE";
    break;
  case TokenType::KW_MATCH:
    os << "KW_MATCH";
    break;
  case TokenType::KW_LOOP:
    os << "KW_LOOP";
    break;
  case TokenType::KW_WHILE:
    os << "KW_WHILE";
    break;
  case TokenType::KW_FOR:
    os << "KW_FOR";
    break;
  case TokenType::KW_IN:
    os << "KW_IN";
    break;
  case TokenType::KW_COMPTIME:
    os << "KW_COMPTIME";
    break;
  case TokenType::KW_AS:
    os << "KW_AS";
    break;
  case TokenType::KW_BREAK:
    os << "KW_BREAK";
    break;
  case TokenType::KW_CONTINUE:
    os << "KW_CONTINUE";
    break;
  case TokenType::KW_NULL:
    os << "KW_NULL";
    break;
  case TokenType::KW_TRUE:
    os << "KW_TRUE";
    break;
  case TokenType::KW_FALSE:
    os << "KW_FALSE";
    break;
  case TokenType::KW_IMPL:
    os << "KW_IMPL";
    break;
  case TokenType::KW_I8:
    os << "KW_I8";
    break;
  case TokenType::KW_I16:
    os << "KW_I16";
    break;
  case TokenType::KW_I32:
    os << "KW_I32";
    break;
  case TokenType::KW_I64:
    os << "KW_I64";
    break;
  case TokenType::KW_U8:
    os << "KW_U8";
    break;
  case TokenType::KW_U16:
    os << "KW_U16";
    break;
  case TokenType::KW_U32:
    os << "KW_U32";
    break;
  case TokenType::KW_U64:
    os << "KW_U64";
    break;
  case TokenType::KW_F32:
    os << "KW_F32";
    break;
  case TokenType::KW_F64:
    os << "KW_F64";
    break;
  case TokenType::KW_BOOL:
    os << "KW_BOOL";
    break;
  case TokenType::KW_USIZE:
    os << "KW_USIZE";
    break;
  case TokenType::KW_STRING:
    os << "KW_STRING";
    break;
  case TokenType::LPAREN:
    os << "LPAREN";
    break;
  case TokenType::RPAREN:
    os << "RPAREN";
    break;
  case TokenType::LBRACE:
    os << "LBRACE";
    break;
  case TokenType::RBRACE:
    os << "RBRACE";
    break;
  case TokenType::LBRACKET:
    os << "LBRACKET";
    break;
  case TokenType::RBRACKET:
    os << "RBRACKET";
    break;
  case TokenType::COMMA:
    os << "COMMA";
    break;
  case TokenType::SEMICOLON:
    os << "SEMICOLON";
    break;
  case TokenType::COLON:
    os << "COLON";
    break;
  case TokenType::DOT:
    os << "DOT";
    break;
  case TokenType::QUESTION:
    os << "QUESTION";
    break;
  case TokenType::TILDE:
    os << "TILDE";
    break;
  case TokenType::AT:
    os << "AT";
    break;
  case TokenType::ARROW:
    os << "ARROW";
    break;
  case TokenType::DOUBLE_COLON:
    os << "DOUBLE_COLON";
    break;
  case TokenType::HASH_BRACK:
    os << "HASH_BRACK";
    break;
  case TokenType::PLUS:
    os << "PLUS";
    break;
  case TokenType::MINUS:
    os << "MINUS";
    break;
  case TokenType::STAR:
    os << "STAR";
    break;
  case TokenType::SLASH:
    os << "SLASH";
    break;
  case TokenType::PERCENT:
    os << "PERCENT";
    break;
  case TokenType::EQ:
    os << "EQ";
    break;
  case TokenType::NEQ:
    os << "NEQ";
    break;
  case TokenType::LT:
    os << "LT";
    break;
  case TokenType::GT:
    os << "GT";
    break;
  case TokenType::LTE:
    os << "LTE";
    break;
  case TokenType::GTE:
    os << "GTE";
    break;
  case TokenType::AND:
    os << "AND";
    break;
  case TokenType::OR:
    os << "OR";
    break;
  case TokenType::NOT:
    os << "NOT";
    break;
  case TokenType::BIT_AND:
    os << "BIT_AND";
    break;
  case TokenType::BIT_OR:
    os << "BIT_OR";
    break;
  case TokenType::BIT_XOR:
    os << "BIT_XOR";
    break;
  case TokenType::LSHIFT:
    os << "LSHIFT";
    break;
  case TokenType::RSHIFT:
    os << "RSHIFT";
    break;
  case TokenType::ASSIGN:
    os << "ASSIGN";
    break;
  case TokenType::UNDERSCORE:
    os << "UNDERSCORE";
    break;
  case TokenType::EOF_TOKEN:
    os << "EOF";
    break;
  case TokenType::ERROR:
    os << "ERROR(" << lexeme << ")";
    break;
  default:
    os << "UNKNOWN";
  }
  os << " SPAN " << start << ".." << end << "\n";
}

Tokenizer::Tokenizer(std::string_view source)
    : source(source), position(0), line(1), column(1) {}

std::vector<Token> Tokenizer::tokenize() {
  std::vector<Token> tokens;
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
  while (!is_at_end() && std::isspace(current())) {
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

TokenType Tokenizer::get_keyword_type(std::string_view lexeme) {
  static const std::unordered_map<std::string_view, TokenType> keywords = {
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

  std::string lexeme(source.substr(start_pos, position - start_pos));
  TokenType type = get_keyword_type(lexeme);

  return Token(type, lexeme, start_pos, position);
}

Token Tokenizer::read_number() {
  size_t start_pos = position;
  bool is_float = false;

  // Read digits
  while (!is_at_end() && is_digit(current())) {
    advance();
  }

  // Check for decimal point
  if (!is_at_end() && current() == '.' && is_digit(peek())) {
    is_float = true;
    advance(); // consume .
    while (!is_at_end() && is_digit(current())) {
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
    while (!is_at_end() && is_digit(current())) {
      advance();
    }
  }

  std::string lexeme(source.substr(start_pos, position - start_pos));
  Token token(is_float ? TokenType::FLOAT_LIT : TokenType::INT_LIT, lexeme,
              start_pos, position);

  if (is_float) {
    token.float_value = std::stod(lexeme);
  } else {
    token.int_value = std::stoll(lexeme);
  }

  return token;
}

Token Tokenizer::read_string() {
  size_t start_pos = position;
  char quote_char = current();
  advance(); // consume opening quote

  // size_t start_pos = position;
  std::string result;

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
    return Token(TokenType::DOT, ".", start_pos, position);

  case '#':
    if (current() == '[') {
      advance();
      return Token(TokenType::HASH_BRACK, "#[", start_pos, position);
    }
    return Token(TokenType::ERROR, std::string(1, ch), start_pos, position);

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
    return Token(TokenType::ERROR, std::string(1, ch), start_pos, position);
  }
}

} // namespace shikimori
