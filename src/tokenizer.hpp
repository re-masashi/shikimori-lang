#pragma once

#include <cstdint>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace shikimori {

enum class TokenType {
  // Literals
  INT_LIT,
  FLOAT_LIT,
  STRING_LIT,
  BOOL_LIT,

  // Identifiers and keywords
  IDENT,

  // Keywords
  KW_FN,
  KW_STRUCT,
  KW_UNION,
  KW_INTERFACE,
  KW_EXTERN,
  KW_USE,
  KW_MACRO,
  KW_IMPL,
  KW_LET,
  KW_RETURN,
  KW_DEFER,
  KW_IF,
  KW_ELSE,
  KW_WHERE,
  KW_MATCH,
  KW_LOOP,
  KW_WHILE,
  KW_FOR,
  KW_IN,
  KW_COMPTIME,
  KW_AS,
  KW_BREAK,
  KW_CONTINUE,
  KW_NULL,
  KW_TRUE,
  KW_FALSE,

  // Type keywords
  KW_I8,
  KW_I16,
  KW_I32,
  KW_I64,
  KW_U8,
  KW_U16,
  KW_U32,
  KW_U64,
  KW_F32,
  KW_F64,
  KW_BOOL,
  KW_USIZE,
  KW_STRING,

  // Single character operators and punctuation
  LPAREN,    // (
  RPAREN,    // )
  LBRACE,    // {
  RBRACE,    // }
  LBRACKET,  // [
  RBRACKET,  // ]
  COMMA,     // ,
  SEMICOLON, // ;
  COLON,     // :
  DOT,       // .
  QUESTION,  // ?
  TILDE,     // ~
  AT,        // @

  // Multi character operators
  ARROW,        // ->
  FAT_ARROW,    // =>
  DOUBLE_COLON, // ::
  HASH_BRACK,   // #[

  // Arithmetic operators
  PLUS,    // +
  MINUS,   // -
  STAR,    // *
  SLASH,   // /
  PERCENT, // %

  // Comparison operators
  EQ,  // ==
  NEQ, // !=
  LT,  // <
  GT,  // >
  LTE, // <=
  GTE, // >=

  // Logical operators
  AND, // &&
  OR,  // ||
  NOT, // !

  // Bitwise operators
  BIT_AND, // &
  BIT_OR,  // |
  BIT_XOR, // ^
  LSHIFT,  // <<
  RSHIFT,  // >>

  // Assignment
  ASSIGN, // =

  // Label
  UNDERSCORE, // _ (wildcard)

  // Special
  EOF_TOKEN,
  ERROR,
};

struct Token {
  TokenType type;
  std::string lexeme;
  size_t start;
  size_t end;

  // For numeric literals
  std::optional<int64_t> int_value;
  std::optional<double> float_value;

  Token(TokenType t, std::string lex, size_t s, size_t e)
      : type(t), lexeme(std::move(lex)), start(s), end(e) {}

  /// Dump token information to a stream
  void dump(std::ostream &os = std::cout) const;
};

class Tokenizer {
public:
  explicit Tokenizer(std::string_view source);

  /// Tokenize the entire source and return a vector of tokens
  std::vector<Token> tokenize();

  /// Get the next token (for streaming)
  Token next_token();

private:
  std::string_view source;
  size_t position;
  int line;
  int column;

  // Helper methods
  char current() const;
  char peek(size_t offset = 1) const;
  void advance();
  void skip_whitespace();
  void skip_comment();
  bool is_at_end() const;

  // Token recognition
  Token read_identifier();
  Token read_number();
  Token read_string();
  Token read_next_token();

  static bool is_digit(char c);
  static bool is_letter(char c);
  static bool is_identifier_start(char c);
  static bool is_identifier_continue(char c);
  static TokenType get_keyword_type(std::string_view lexeme);
};

} // namespace shikimori
