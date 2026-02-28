#pragma once
#include <cstdint>
#include <format>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "ast/typedast.h"
#include "span.h"
#include "typechecker/import_resolver.h"
#include "types.h"

namespace shikimori {
namespace ast {
struct BinaryExpr;
struct Block;
struct BuiltinCall;
struct Call;
struct Expr;
struct ExternDecl;
struct FieldAccess;
struct FnDecl;
struct IfExpr;
struct ImportItem;
struct InterfaceDecl;
struct MatchExpr;
struct MethodCall;
struct Program;
struct ScopeAccess;
struct Stmt;
struct StructDecl;
struct StructInit;
struct TypeAnnot;
struct UnaryExpr;
struct UnionDecl;
struct UseDecl;
} // namespace ast
} // namespace shikimori

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

struct FnConstraint {
  string type_param;         // "T" in where T: interface X
  vector<string> interfaces; // where T: interface A + B
};

struct FnDef {
  TypeRef ty;                        // ForAll or FnTy
  vector<FnConstraint> where_clause; // constraints on type params
  bool is_static;                    // true if no self parameter
};

namespace shikimori {

struct TypeError : runtime_error {
  Span span;
  TypeError(const string &msg, Span span) : runtime_error(msg), span(span) {}
};

struct Typechecker {
public:
  ImportResolver import_resolver;
  map<string, FnDef> functions; // name -> FnDef with type and constraints
  map<string, StructDef> structs;
  map<string, UnionDef> unions;
  map<string, TyInterface> interfaces;

  map<uint32_t, TypeRef> ty_solutions; // solved ETVars
  uint32_t next_id = 0;

  map<string, uint32_t>
      current_type_vars; // active type vars in generic context

  vector<map<string, TypeRef>> scopes;

  TypeRef current_return_type = nullptr;

  void collect(const ast::Program &program);
  void collect_from_program(const ast::Program &program);
  void collect_from_program_filtered(const ast::Program &program,
                                     const vector<ast::ImportItem> &items);

  void collect_fn(const ast::FnDecl &decl, const string &name_prefix = "");
  void collect_struct(const ast::StructDecl &decl);
  void collect_union(const ast::UnionDecl &decl);
  void collect_interface(const ast::InterfaceDecl &decl);
  void collect_extern(const ast::ExternDecl &decl);

  void resolve_use(const ast::UseDecl &use, string file_path);

  TypeRef resolve_type(const ast::TypeAnnot &annot);

  typed::TypedProgram run(const ast::Program &program);
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
