#pragma once

#include <filesystem>
#include <initializer_list>
#include <memory>
#include <optional>
#include <stddef.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "../ast/ast_decl.hpp"
#include "../ast/ast_expr.hpp"
#include "../ast/ast_stmt.hpp"
#include "../span.h"
#include "tokenizer.hpp"

using namespace std;

namespace shikimori {

class Parser {
public:
  explicit Parser(string_view source,
                  filesystem::path file = filesystem::path());
  explicit Parser(const vector<Token> &tokens, string_view source,
                  filesystem::path file = filesystem::path());

  optional<ast::Program> parse();

  void set_source(string_view source);
  void set_tokens(vector<Token> tokens);
  void set_file(filesystem::path file);

  const Token &current() const;
  const Token &peek(size_t offset = 1) const;
  const Token &previous() const;
  bool is_at_end() const;
  bool check(TokenType type) const;
  bool check_any(initializer_list<TokenType> types) const;
  bool match(TokenType type);
  bool match_any(initializer_list<TokenType> types);

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
  const vector<string> &get_errors() const;
  void clear_errors();

  optional<ast::Decl> parse_declaration();
  optional<ast::FnDecl> parse_fn_decl();
  optional<ast::StructDecl> parse_struct_decl();
  optional<ast::UnionDecl> parse_union_decl();
  optional<ast::InterfaceDecl> parse_interface_decl();
  optional<ast::ExternDecl> parse_extern_decl();
  optional<ast::UseDecl> parse_use_decl();
  optional<ast::MacroDecl> parse_macro_decl();

  optional<Spanned<ast::TypeAnnot>> parse_type();
  optional<Spanned<ast::TypeAnnot>> parse_type_with_prefix_operators();
  optional<Spanned<ast::TypeAnnot>> parse_named_type();
  optional<Spanned<ast::InterfaceTypeAnnot>> parse_interface_type();
  optional<Spanned<ast::PrimitiveType>> parse_primitive_type();

  optional<ast::Param> parse_param();
  optional<ast::GenericParam> parse_generic_param();
  optional<ast::WhereConstraint> parse_where_constraint();
  optional<ast::ExternParam> parse_extern_param();
  optional<ast::MacroParam> parse_macro_param();
  optional<ast::InterfaceMethod> parse_interface_method();
  optional<ast::FieldDecl> parse_field_decl();
  optional<ast::UnionVariant> parse_variant_decl();

  optional<Spanned<ast::Stmt>> parse_statement();
  optional<Spanned<ast::Stmt>> parse_let_stmt();
  optional<Spanned<ast::Stmt>> parse_return_stmt();
  optional<Spanned<ast::Stmt>> parse_defer_stmt();
  optional<Spanned<ast::Stmt>> parse_loop_stmt();
  optional<Spanned<ast::Stmt>> parse_while_stmt();
  optional<Spanned<ast::Stmt>> parse_for_stmt();
  optional<Spanned<ast::Stmt>> parse_comptime_stmt();
  optional<Spanned<ast::Stmt>> parse_expr_stmt();

  optional<ast::Block> parse_block();

  optional<Spanned<ast::Expr>> parse_expression();
  optional<Spanned<ast::Expr>> parse_assignment();
  optional<Spanned<ast::Expr>> parse_logical_or();
  optional<Spanned<ast::Expr>> parse_logical_and();
  optional<Spanned<ast::Expr>> parse_bitwise_or();
  optional<Spanned<ast::Expr>> parse_bitwise_xor();
  optional<Spanned<ast::Expr>> parse_bitwise_and();
  optional<Spanned<ast::Expr>> parse_equality();
  optional<Spanned<ast::Expr>> parse_comparison();
  optional<Spanned<ast::Expr>> parse_shift();
  optional<Spanned<ast::Expr>> parse_additive();
  optional<Spanned<ast::Expr>> parse_multiplicative();
  optional<Spanned<ast::Expr>> parse_unary();
  optional<Spanned<ast::Expr>> parse_postfix();
  optional<Spanned<ast::Expr>> parse_primary();

  optional<Spanned<ast::Expr>> parse_if_expr();
  optional<Spanned<ast::Expr>> parse_match_expr();
  optional<Spanned<ast::Expr>> parse_struct_init();
  optional<Spanned<ast::Expr>> parse_scope_access();
  optional<Spanned<ast::Expr>> parse_comptime_expr();
  optional<Spanned<ast::Expr>> parse_builtin_call();
  optional<Spanned<ast::Expr>> parse_break_expr();
  optional<Spanned<ast::Expr>> parse_continue_expr();

  optional<Spanned<ast::Pattern>> parse_pattern();
  optional<ast::MatchArm> parse_match_arm();

  optional<vector<unique_ptr<ast::Expr>>> parse_arg_list();
  optional<vector<pair<ast::Identifier, unique_ptr<ast::Expr>>>>
  parse_field_init_list();
  optional<vector<unique_ptr<ast::TypeAnnot>>> parse_type_arg_list();

  optional<ast::Identifier> parse_label();

private:
  string_view source;
  filesystem::path file;
  vector<Token> tokens;
  size_t current_pos;
  vector<string> errors;

  // Helper to get span for previous N tokens
  Span get_span_from(size_t offset) const;
};

inline bool is_primitive_keyword(TokenType t) {
  switch (t) {
  case TokenType::KW_I8:
  case TokenType::KW_I16:
  case TokenType::KW_I32:
  case TokenType::KW_I64:
  case TokenType::KW_U8:
  case TokenType::KW_U16:
  case TokenType::KW_U32:
  case TokenType::KW_U64:
  case TokenType::KW_F32:
  case TokenType::KW_F64:
  case TokenType::KW_BOOL:
  case TokenType::KW_USIZE:
  case TokenType::KW_STRING:
    return true;
  default:
    return false;
  }
}

} // namespace shikimori
