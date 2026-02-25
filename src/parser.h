#pragma once

#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "ast.h"
#include "span.h"
#include "tokenizer.hpp"

namespace shikimori {

class Parser {
public:
  explicit Parser(std::string_view source);
  explicit Parser(const std::vector<Token> &tokens, std::string_view source);

  std::optional<ast::Program> parse();

  void set_source(std::string_view source);
  void set_tokens(std::vector<Token> tokens);

  const Token &current() const;
  const Token &peek(size_t offset = 1) const;
  const Token &previous() const;
  bool is_at_end() const;
  bool check(TokenType type) const;
  bool check_any(std::initializer_list<TokenType> types) const;
  bool match(TokenType type);
  bool match_any(std::initializer_list<TokenType> types);

  Token advance();
  Token consume(TokenType type, const char *error_message);
  template <typename... Ts> Token consume_any(const char *error_message);

  Span get_current_span() const;
  Span get_span_for(size_t start_pos) const;
  Span get_span_from_previous() const;

  void report_error(const Span &span, const char *message);
  void report_error(const char *message);
  void report_error_at_current(const char *message);
  void report_expected_but_got(const char *expected, const Token &got);
  void report_unexpected_token(const Token &token);
  void report_unexpected_token();

  bool has_errors() const;
  const std::vector<std::string> &get_errors() const;
  void clear_errors();

  std::optional<ast::Decl> parse_declaration();
  std::optional<ast::FnDecl> parse_fn_decl();
  std::optional<ast::StructDecl> parse_struct_decl();
  std::optional<ast::UnionDecl> parse_union_decl();
  std::optional<ast::InterfaceDecl> parse_interface_decl();
  std::optional<ast::ExternDecl> parse_extern_decl();
  std::optional<ast::UseDecl> parse_use_decl();
  std::optional<ast::MacroDecl> parse_macro_decl();

  std::optional<Spanned<ast::TypeAnnot>> parse_type();
  std::optional<Spanned<ast::TypeAnnot>> parse_type_with_prefix_operators();
  std::optional<Spanned<ast::TypeAnnot>> parse_named_type();
  std::optional<Spanned<ast::InterfaceTypeAnnot>> parse_interface_type();
  std::optional<Spanned<ast::PrimitiveType>> parse_primitive_type();

  std::optional<ast::Param> parse_param();
  std::optional<ast::GenericParam> parse_generic_param();
  std::optional<ast::WhereConstraint> parse_where_constraint();
  std::optional<ast::ExternParam> parse_extern_param();
  std::optional<ast::MacroParam> parse_macro_param();
  std::optional<ast::InterfaceMethod> parse_interface_method();
  std::optional<ast::FieldDecl> parse_field_decl();
  std::optional<ast::UnionVariant> parse_variant_decl();

  std::optional<Spanned<ast::Stmt>> parse_statement();
  std::optional<Spanned<ast::Stmt>> parse_let_stmt();
  std::optional<Spanned<ast::Stmt>> parse_return_stmt();
  std::optional<Spanned<ast::Stmt>> parse_defer_stmt();
  std::optional<Spanned<ast::Stmt>> parse_loop_stmt();
  std::optional<Spanned<ast::Stmt>> parse_while_stmt();
  std::optional<Spanned<ast::Stmt>> parse_for_stmt();
  std::optional<Spanned<ast::Stmt>> parse_comptime_stmt();
  std::optional<Spanned<ast::Stmt>> parse_expr_stmt();

  std::optional<ast::Block> parse_block();

  std::optional<Spanned<ast::Expr>> parse_expression();
  std::optional<Spanned<ast::Expr>> parse_assignment();
  std::optional<Spanned<ast::Expr>> parse_logical_or();
  std::optional<Spanned<ast::Expr>> parse_logical_and();
  std::optional<Spanned<ast::Expr>> parse_bitwise_or();
  std::optional<Spanned<ast::Expr>> parse_bitwise_xor();
  std::optional<Spanned<ast::Expr>> parse_bitwise_and();
  std::optional<Spanned<ast::Expr>> parse_equality();
  std::optional<Spanned<ast::Expr>> parse_comparison();
  std::optional<Spanned<ast::Expr>> parse_shift();
  std::optional<Spanned<ast::Expr>> parse_additive();
  std::optional<Spanned<ast::Expr>> parse_multiplicative();
  std::optional<Spanned<ast::Expr>> parse_unary();
  std::optional<Spanned<ast::Expr>> parse_postfix();
  std::optional<Spanned<ast::Expr>> parse_primary();

  std::optional<Spanned<ast::Expr>> parse_if_expr();
  std::optional<Spanned<ast::Expr>> parse_match_expr();
  std::optional<Spanned<ast::Expr>> parse_struct_init();
  std::optional<Spanned<ast::Expr>> parse_union_init();
  std::optional<Spanned<ast::Expr>> parse_comptime_expr();
  std::optional<Spanned<ast::Expr>> parse_builtin_call();
  std::optional<Spanned<ast::Expr>> parse_break_expr();
  std::optional<Spanned<ast::Expr>> parse_continue_expr();

  std::optional<Spanned<ast::Pattern>> parse_pattern();
  std::optional<ast::MatchArm> parse_match_arm();

  std::optional<std::vector<std::unique_ptr<ast::Expr>>> parse_arg_list();
  std::optional<
      std::vector<std::pair<ast::Identifier, std::unique_ptr<ast::Expr>>>>
  parse_field_init_list();
  std::optional<std::vector<std::unique_ptr<ast::TypeAnnot>>>
  parse_type_arg_list();

  std::optional<ast::Identifier> parse_label();

private:
  std::string_view source;
  std::vector<Token> tokens;
  size_t current_pos;
  std::vector<std::string> errors;

  // Helper to get span for previous N tokens
  Span get_span_from(size_t offset) const;
};

} // namespace shikimori
