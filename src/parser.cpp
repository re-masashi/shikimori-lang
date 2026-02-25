#include "parser.h"

#include <format>

#include "tokenizer.hpp"

namespace shikimori {

Parser::Parser(std::string_view source, std::filesystem::path file)
    : source(source), file(std::move(file)), current_pos(0) {}

Parser::Parser(const std::vector<Token> &tokens, std::string_view source,
               std::filesystem::path file)
    : source(source), file(std::move(file)), tokens(tokens), current_pos(0) {}

void Parser::set_source(std::string_view source) { this->source = source; }

void Parser::set_tokens(std::vector<Token> tokens) {
  this->tokens = std::move(tokens);
  current_pos = 0;
}

void Parser::set_file(std::filesystem::path file) {
  this->file = std::move(file);
}

const Token &Parser::current() const { return tokens[current_pos]; }

const Token &Parser::peek(size_t offset) const {
  if (current_pos + offset >= tokens.size()) {
    return tokens.back();
  }
  return tokens[current_pos + offset];
}

const Token &Parser::previous() const { return tokens[current_pos - 1]; }

bool Parser::is_at_end() const {
  return current().type == TokenType::EOF_TOKEN;
}

bool Parser::check(TokenType type) const {
  if (is_at_end())
    return false;
  return current().type == type;
}

bool Parser::check_any(std::initializer_list<TokenType> types) const {
  if (is_at_end())
    return false;
  for (auto type : types) {
    if (current().type == type)
      return true;
  }
  return false;
}

bool Parser::match(TokenType type) {
  if (!check(type))
    return false;
  advance();
  return true;
}

bool Parser::match_any(std::initializer_list<TokenType> types) {
  for (auto type : types) {
    if (match(type))
      return true;
  }
  return false;
}

Token Parser::advance() {
  if (!is_at_end())
    current_pos++;
  return previous();
}

Token Parser::consume(TokenType type, const char *error_message) {
  if (check(type))
    return advance();
  report_expected_but_got(error_message, current());
  return Token(TokenType::ERROR, "", 0, 0);
}

template <typename... Ts> Token Parser::consume_any(const char *error_message) {
  if (check_any({Ts::value...}))
    return advance();
  report_expected_but_got(error_message, current());
  return Token(TokenType::ERROR, "", 0, 0);
}

Span Parser::get_current_span() const {
  return Span(current().start, current().end, file);
}

Span Parser::get_span_for(size_t start_pos) const {
  if (start_pos >= tokens.size()) {
    return Span(0, 0, file);
  }
  return Span(tokens[start_pos].start, current().end, file);
}

Span Parser::get_span_from_previous() const {
  return Span(previous().start, previous().end, file);
}

Span Parser::get_span_from(size_t offset) const {
  if (current_pos >= tokens.size() || offset >= current_pos) {
    return Span(0, 0, file);
  }
  return Span(tokens[current_pos - offset].start, previous().end, file);
}

void Parser::report_error(const Span &span, const char *message) {
  errors.push_back(std::format("Error at {}:{}:{}: {}", span.file.string(),
                               span.start, span.end, message));
}

void Parser::report_error(const char *message) {
  auto span = get_current_span();
  report_error(span, message);
}

void Parser::report_error_at_current(const char *message) {
  auto span = get_current_span();
  errors.push_back(std::format("Error at {}:{}:{}: {} (got '{}')",
                               span.file.string(), span.start, span.end,
                               message, current().lexeme));
}

void Parser::report_expected_but_got(const char *expected, const Token &got) {
  errors.push_back(std::format("Error at {}:{}: Expected {} but got '{}'",
                               file.string(), got.start, expected, got.lexeme));
}

void Parser::report_unexpected_token(const Token &token) {
  errors.push_back(std::format("Error at {}:{}: Unexpected token '{}'",
                               file.string(), token.start, token.lexeme));
}

void Parser::report_unexpected_token() { report_unexpected_token(current()); }

bool Parser::has_errors() const { return !errors.empty(); }

const std::vector<std::string> &Parser::get_errors() const { return errors; }

void Parser::clear_errors() { errors.clear(); }

} // namespace shikimori
