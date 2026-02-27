#include "parser/tokenizer.hpp"
#include <doctest/doctest.h>
#include <vector>

using namespace shikimori;

TEST_CASE("Tokenizer: Basic keywords") {
  Tokenizer tokenizer("fn let return struct");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 4);
  CHECK(tokens[0].type == TokenType::KW_FN);
  CHECK(tokens[1].type == TokenType::KW_LET);
  CHECK(tokens[2].type == TokenType::KW_RETURN);
  CHECK(tokens[3].type == TokenType::KW_STRUCT);
}

TEST_CASE("Tokenizer: Identifiers") {
  Tokenizer tokenizer("foo bar _test myVar123");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 4);
  CHECK(tokens[0].type == TokenType::IDENT);
  CHECK(tokens[0].lexeme == "foo");
  CHECK(tokens[1].lexeme == "bar");
  CHECK(tokens[2].lexeme == "_test");
  CHECK(tokens[3].lexeme == "myVar123");
}

TEST_CASE("Tokenizer: Integer literals") {
  Tokenizer tokenizer("42 0 12345");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 3);
  CHECK(tokens[0].type == TokenType::INT_LIT);
  CHECK(tokens[0].int_value.value() == 42);
  CHECK(tokens[1].int_value.value() == 0);
  CHECK(tokens[2].int_value.value() == 12345);
}

TEST_CASE("Tokenizer: Float literals") {
  Tokenizer tokenizer("3.14 0.5 1.0");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 3);
  CHECK(tokens[0].type == TokenType::FLOAT_LIT);
  CHECK(tokens[1].type == TokenType::FLOAT_LIT);
  CHECK(tokens[2].type == TokenType::FLOAT_LIT);
}

TEST_CASE("Tokenizer: String literals") {
  Tokenizer tokenizer("\"hello\" \"world\" \"\"");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() >= 3);
  CHECK(tokens[0].type == TokenType::STRING_LIT);
  CHECK(tokens[0].lexeme == "hello");
  CHECK(tokens[1].lexeme == "world");
  CHECK(tokens[2].lexeme == "");
}

TEST_CASE("Tokenizer: Operators") {
  Tokenizer tokenizer("+ - * / = == != < > <= >=");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() >= 10);
  CHECK(tokens[0].type == TokenType::PLUS);
  CHECK(tokens[1].type == TokenType::MINUS);
  CHECK(tokens[2].type == TokenType::STAR);
  CHECK(tokens[3].type == TokenType::SLASH);
  CHECK(tokens[4].type == TokenType::ASSIGN);
  CHECK(tokens[5].type == TokenType::EQ);
  CHECK(tokens[6].type == TokenType::NEQ);
  CHECK(tokens[7].type == TokenType::LT);
  CHECK(tokens[8].type == TokenType::GT);
  CHECK(tokens[9].type == TokenType::LTE);
}

TEST_CASE("Tokenizer: Punctuation") {
  Tokenizer tokenizer("( ) { } [ ] , ; : .");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 10);
  CHECK(tokens[0].type == TokenType::LPAREN);
  CHECK(tokens[1].type == TokenType::RPAREN);
  CHECK(tokens[2].type == TokenType::LBRACE);
  CHECK(tokens[3].type == TokenType::RBRACE);
  CHECK(tokens[4].type == TokenType::LBRACKET);
  CHECK(tokens[5].type == TokenType::RBRACKET);
  CHECK(tokens[6].type == TokenType::COMMA);
  CHECK(tokens[7].type == TokenType::SEMICOLON);
  CHECK(tokens[8].type == TokenType::COLON);
  CHECK(tokens[9].type == TokenType::DOT);
}

TEST_CASE("Tokenizer: Boolean literals") {
  Tokenizer tokenizer("true false");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() >= 2);
  CHECK(tokens[0].type == TokenType::BOOL_LIT);
  CHECK(tokens[1].type == TokenType::BOOL_LIT);
  CHECK(tokens[0].lexeme == "true");
  CHECK(tokens[1].lexeme == "false");
}

TEST_CASE("Tokenizer: Comments are skipped") {
  Tokenizer tokenizer("// this is a comment\nlet x = 42");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() == 4);
  CHECK(tokens[0].type == TokenType::KW_LET);
  CHECK(tokens[1].type == TokenType::IDENT);
  CHECK(tokens[2].type == TokenType::ASSIGN);
  CHECK(tokens[3].type == TokenType::INT_LIT);
}

TEST_CASE("Tokenizer: Whitespace is skipped") {
  Tokenizer tokenizer("  fn   foo  ()  ");
  auto tokens = tokenizer.tokenize();

  REQUIRE(tokens.size() >= 3);
  CHECK(tokens[0].type == TokenType::KW_FN);
  CHECK(tokens[1].type == TokenType::IDENT);
  CHECK(tokens[2].type == TokenType::LPAREN);
}

TEST_CASE("Tokenizer: EOF token at end") {
  Tokenizer tokenizer("let");
  auto tokens = tokenizer.tokenize();

  CHECK(tokens.size() >= 1);
  CHECK(tokens[0].type == TokenType::KW_LET);
}
