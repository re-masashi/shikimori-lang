#pragma once
#include <cstdint>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include "ast/ast.h"
#include "ast/typedast.h"

using namespace std;
using namespace shikimori;

struct StructDef {
  vector<pair<string, TypeRef>> fields;
  optional<ForAll> scheme;
};

struct UnionDef {
  vector<pair<string, vector<TypeRef>>> variants; // name -> payload
  optional<ForAll> scheme;
};

namespace shikimori {

struct TypeError : runtime_error {
  Span span;
  TypeError(const string &msg, Span span) : runtime_error(msg), span(span) {}
};

struct Typechecker {
  map<string, TypeRef> functions; // name -> ForAll or FnTy
  map<string, StructDef> structs;
  map<string, UnionDef> unions;
  map<string, TyInterface> interfaces;

  map<uint32_t, TypeRef> ty_solutions; // solved ETVars
  uint32_t next_id = 0;

  vector<map<string, TypeRef>> scopes;

  TypeRef current_return_type = nullptr;

  void collect(const ast::Program &program);
  void collect_fn(const ast::FnDecl &decl);
  void collect_struct(const ast::StructDecl &decl);
  void collect_union(const ast::UnionDecl &decl);
  void collect_interface(const ast::InterfaceDecl &decl);
  void collect_extern(const ast::ExternDecl &decl);

  TypeRef resolve_type(const ast::TypeAnnot &annot);

  typed::TypedProgram check(const ast::Program &program);
  typed::TypedFnDecl check_fn(const ast::FnDecl &decl);
  typed::TypedBlock check_block(const ast::Block &block);
  typed::TypedStmt check_stmt(const ast::Stmt &stmt);
  typed::TypedExpr check_expr(const ast::Expr &expr);

  typed::TypedExpr check_call(const ast::Call &call, Span span);
  typed::TypedExpr check_method_call(const ast::MethodCall &call, Span span);
  typed::TypedExpr check_field_access(const ast::FieldAccess &fa, Span span);
  typed::TypedExpr check_binary(const ast::BinaryExpr &bin, Span span);
  typed::TypedExpr check_unary(const ast::UnaryExpr &un, Span span);
  typed::TypedExpr check_if(const ast::IfExpr &if_expr, Span span);
  typed::TypedExpr check_match(const ast::MatchExpr &match, Span span);
  typed::TypedExpr check_struct_init(const ast::StructInit &init, Span span);
  typed::TypedExpr check_scope_access(const ast::ScopeAccess &sa, Span span);
  typed::TypedExpr check_builtin(const ast::BuiltinCall &call, Span span);

  void unify(TypeRef a, TypeRef b, Span span);

  TypeRef instantiate(const ForAll &scheme);

  TypeRef apply_solutions(TypeRef ty);

  void check_solved(TypeRef ty, Span span);

  void check_satisfies(const string &type_name, const string &iface_name,
                       Span span);

  uint32_t fresh_id() { return next_id++; }
  TypeRef fresh_etvar(const string &name);
  void push_scope();
  void pop_scope();
  void define_local(const string &name, TypeRef ty);
  optional<TypeRef> lookup_local(const string &name); // none -> not found
  optional<TypeRef> lookup_any(const string &name);
};

} // namespace shikimori
