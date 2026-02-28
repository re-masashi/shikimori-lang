#pragma once

#include <stddef.h>
#include <cstdint>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using namespace std;

#define TOKEN_TYPES_SIMPLE                                                     \
  KW_FN, KW_STRUCT, KW_UNION, KW_INTERFACE, KW_EXTERN, KW_USE, KW_MACRO,       \
      KW_IMPL, KW_LET, KW_RETURN, KW_DEFER, KW_IF, KW_ELSE, KW_WHERE,          \
      KW_MATCH, KW_LOOP, KW_WHILE, KW_FOR, KW_IN, KW_COMPTIME, KW_AS,          \
      KW_BREAK, KW_CONTINUE, KW_NULL, KW_TRUE, KW_FALSE, KW_I8, KW_I16,        \
      KW_I32, KW_I64, KW_U8, KW_U16, KW_U32, KW_U64, KW_F32, KW_F64, KW_BOOL,  \
      KW_USIZE, KW_STRING, LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, \
      COMMA, SEMICOLON, COLON, DOT, DOT_DOT, DOT_DOT_EQ, QUESTION, TILDE, AT,  \
      ARROW, FAT_ARROW, DOUBLE_COLON, HASH_BRACK, PLUS, MINUS, STAR, SLASH,    \
      PERCENT, EQ, NEQ, LT, GT, LTE, GTE, AND, OR, NOT, BIT_AND, BIT_OR,       \
      BIT_XOR, LSHIFT, RSHIFT, ASSIGN, UNDERSCORE, EOF_TOKEN

#define TOKEN_TYPES_VALUED                                                     \
  INT_LIT, FLOAT_LIT, STRING_LIT, BOOL_LIT, IDENT, ERROR

#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0(EVAL0(EVAL0(__VA_ARGS__)))
#define EVAL2(...) EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL4(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL5(...) EVAL4(EVAL4(EVAL4(__VA_ARGS__)))
#define EVAL(...) EVAL5(__VA_ARGS__)
#define MAP_END(...)
#define MAP_OUT
#define EMPTY()
#define DEFER(id) id EMPTY()
#define MAP_GET_END2() 0, MAP_END
#define MAP_GET_END1(...) MAP_GET_END2
#define MAP_GET_END(...) MAP_GET_END1
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) DEFER(MAP_NEXT0)(test, next, 0)
#define MAP_NEXT(test, next) MAP_NEXT1(MAP_GET_END test, next)
#define MAP_INC(X) MAP_INC_##X
#define MAP0(f, x, peek, ...)                                                  \
  f(x) DEFER(MAP_NEXT(peek, MAP1))(f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...)                                                  \
  f(x) DEFER(MAP_NEXT(peek, MAP0))(f, peek, __VA_ARGS__)

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
  LPAREN,     // (
  RPAREN,     // )
  LBRACE,     // {
  RBRACE,     // }
  LBRACKET,   // [
  RBRACKET,   // ]
  COMMA,      // ,
  SEMICOLON,  // ;
  COLON,      // :
  DOT,        // .
  DOT_DOT,    // ..
  DOT_DOT_EQ, // ..=
  QUESTION,   // ?
  TILDE,      // ~
  AT,         // @

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
  string lexeme;
  size_t start;
  size_t end;

  // For numeric literals
  optional<int64_t> int_value;
  optional<double> float_value;

  Token(TokenType t, string lex, size_t s, size_t e)
      : type(t), lexeme(std::move(lex)), start(s), end(e) {}

  /// Dump token information to a stream
  void dump(ostream &os = cout) const;
};

class Tokenizer {
public:
  explicit Tokenizer(string_view source);

  /// Tokenize the entire source and return a vector of tokens
  vector<Token> tokenize();

  /// Get the next token (for streaming)
  Token next_token();

private:
  string_view source;
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
  static TokenType get_keyword_type(string_view lexeme);
};

} // namespace shikimori
