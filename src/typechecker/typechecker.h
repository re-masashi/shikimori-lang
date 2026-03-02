#pragma once
#include <cstdint>
#include <map>
#include <optional>
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
  vector<pair<string, TypeRef>>
      fields; // resolved during collect for non generic, deferred for generic
  optional<ForAll> scheme;

  // For deferred type resolution in check phase. Store raw pointers to avoid
  // copying unique_ptr
  struct FieldInfo {
    string name;
    const ast::TypeAnnot *type; // non-owning pointer
  };
  vector<FieldInfo> field_decls;
  vector<string> generic_params; // to restore context when resolving
};

struct UnionDef {
  vector<pair<string, vector<TypeRef>>> variants; // name -> payload (resolved)
  optional<ForAll> scheme;

  // For deferred type resolution in check phase. Store raw pointers to avoid
  // copying unique_ptr
  struct VariantInfo {
    string name;
    const ast::TypeAnnot *type; // non owning pointer, null if no payload
  };
  vector<VariantInfo> variant_decls;
  vector<string> generic_params; // to restore context when resolving
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
  string current_fn_name; // current function being checked (for where clauses)

  bool resolved = false; // track if resolve() pass has been run

  void collect(const ast::Program &program);
  void collect_from_program(const ast::Program &program);
  void collect_from_program_filtered(const ast::Program &program,
                                     const vector<ast::ImportItem> &items);

  void collect_fn(const ast::FnDecl &decl, const string &name_prefix = "");
  void collect_struct(const ast::StructDecl &decl);
  void collect_struct_decl_only(const ast::StructDecl &decl);
  void collect_struct_methods(const ast::StructDecl &decl);
  void collect_union(const ast::UnionDecl &decl);
  void collect_union_decl_only(const ast::UnionDecl &decl);
  void collect_union_methods(const ast::UnionDecl &decl);
  void collect_interface(const ast::InterfaceDecl &decl);
  void collect_extern(const ast::ExternDecl &decl);

  void resolve_use(const ast::UseDecl &use, string file_path);

  void resolve();
  void resolve_struct_fields(const string &struct_name);
  void resolve_union_variants(const string &union_name);

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
  TypeRef apply_substitution(TypeRef ty, const map<uint32_t, TypeRef> &subst);

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
