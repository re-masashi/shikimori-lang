#pragma once

#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "ast/typedast.h"
#include "typechecker/typechecker.h"
#include "types.h"

namespace shikimori {

// Mangling scheme:
// - Primitives: i32, f64, bool, etc.
// - Pointers: ptr_T
// - Slices: Slice_T
// - Arrays: Arr_N_T
// - Struct/Union with type args: Name_T1_T2
// - Functions: name_T1_T2
// - Methods: TypeName_method_T1_T2

// Key for identifying a generic instantiation
struct InstantiationKey {
  std::string base_name;
  std::vector<TypeRef> type_args;

  bool operator==(const InstantiationKey &other) const;
  bool operator<(const InstantiationKey &other) const;

  static bool types_equal(const TypeRef &a, const TypeRef &b);
  static int compare_types(const TypeRef &a, const TypeRef &b);
};

// Hash for InstantiationKey (for unordered containers)
struct InstantiationKeyHash {
  size_t operator()(const InstantiationKey &key) const;
};

// Exception for monomorphization errors
class MonoError : public std::runtime_error {
public:
  Span span;
  MonoError(const std::string &msg, Span sp = Span{})
      : std::runtime_error(msg), span(sp) {}
};

class Monomorphizer {
public:
  // Maps from instantiation key to mangled name
  std::map<InstantiationKey, std::string> function_instances;
  std::map<InstantiationKey, std::string> struct_instances;
  std::map<InstantiationKey, std::string> union_instances;

  // Track what we've already processed
  std::set<std::string> processed_functions;
  std::set<InstantiationKey> pending_functions;
  std::set<InstantiationKey> pending_structs;
  std::set<InstantiationKey> pending_unions;

  // Reference to typechecker for type information
  const Typechecker &typechecker;

  // Store pointers to original declarations (cannot copy unique_ptrs)
  std::map<std::string, const typed::TypedFnDecl *> original_fns;
  std::map<std::string, const typed::TypedStructDecl *> original_structs;
  std::map<std::string, const typed::TypedUnionDecl *> original_unions;

  // Store methods for each struct/union (keyed by "TypeName.methodName")
  std::map<std::string, std::vector<typed::TypedFnDecl>> struct_methods;
  std::map<std::string, std::vector<typed::TypedFnDecl>> union_methods;

  // Method tracking (methods stored in typechecker.functions as "Type.method")
  std::map<std::string, std::vector<std::string>>
      method_names; // type_name -> [method_names]
  std::map<std::string, const FnDef *>
      original_methods; // "Type.method" -> FnDef

  // The final monomorphized program
  typed::TypedProgram result_program;

  // ETVar resolution map (built during resolution pass)
  std::map<uint32_t, TypeRef> etvar_resolutions;

  // Track variables with ETVar types (var_name -> ETVar ids)
  std::map<std::string, std::vector<uint32_t>> var_etvar_map;

  // Track resolved variable types (var_name -> concrete type)
  std::map<std::string, TypeRef> var_resolved_types;

  // Track ETVars currently being resolved (for cycle detection)
  std::set<uint32_t> resolving_etvars;

  explicit Monomorphizer(const Typechecker &tc) : typechecker(tc) {}

  // Main entry point
  typed::TypedProgram run(const typed::TypedProgram &program);

private:
  // Resolve ETVars from context (e.g., Option[T]::some(99) => T = i32)
  void resolve_etvars(const typed::TypedProgram &program);
  void resolve_etvars_in_fn(const typed::TypedFnDecl &fn);
  void resolve_etvars_in_block(const typed::TypedBlock &block);
  void resolve_etvars_in_stmt(const typed::TypedStmt &stmt);
  void resolve_etvars_in_expr(const typed::TypedExpr &expr);

  // Apply ETVar resolutions to a type (built during resolution pass)
  TypeRef apply_etvar_resolutions(const TypeRef &ty);

  // Apply ETVar resolutions to a function body
  void apply_resolutions_to_fn(typed::TypedFnDecl &fn);
  void apply_resolutions_to_block(typed::TypedBlock &block);
  void apply_resolutions_to_stmt(typed::TypedStmt &stmt);
  void apply_resolutions_to_expr(typed::TypedExpr &expr);

  // Validate that no type variables remain in the program
  void validate_no_typevars(const typed::TypedProgram &program);
  void validate_fn(const typed::TypedFnDecl &fn);
  void validate_block(const typed::TypedBlock &block);
  void validate_stmt(const typed::TypedStmt &stmt);
  void validate_expr(const typed::TypedExpr &expr);
  void validate_type(const TypeRef &ty, Span span);
  void validate_pattern(const typed::TypedPattern &pat);

  std::string mangle_type(const TypeRef &ty);
  std::string mangle_name(const std::string &base,
                          const std::vector<TypeRef> &type_args);

  void collect_from_program(const typed::TypedProgram &program);
  void collect_from_fn(const typed::TypedFnDecl &fn);
  void collect_from_block(const typed::TypedBlock &block);
  void collect_from_stmt(const typed::TypedStmt &stmt);
  void collect_from_expr(const typed::TypedExpr &expr);
  void collect_from_type(const TypeRef &ty);

  // Extract type arguments from a type based on a forall scheme
  std::vector<TypeRef> extract_type_args(const ForAll &forall,
                                         const TypeRef &instantiated);

  void register_fn_instantiation(const std::string &name,
                                 const std::vector<TypeRef> &type_args);
  void register_struct_instantiation(const std::string &name,
                                     const std::vector<TypeRef> &type_args);
  void register_union_instantiation(const std::string &name,
                                    const std::vector<TypeRef> &type_args);

  void generate_pending_instantiations();

  typed::TypedFnDecl monomorphize_fn(const InstantiationKey &key);
  typed::TypedStructDecl monomorphize_struct(const InstantiationKey &key);
  typed::TypedUnionDecl monomorphize_union(const InstantiationKey &key);

  // Monomorphize methods for a struct/union
  std::vector<typed::TypedFnDecl>
  monomorphize_struct_methods(const InstantiationKey &struct_key,
                              const std::map<uint32_t, TypeRef> &subst);
  std::vector<typed::TypedFnDecl>
  monomorphize_union_methods(const InstantiationKey &union_key,
                             const std::map<uint32_t, TypeRef> &subst);

  // Core substitution - replaces TyVar with concrete types
  TypeRef substitute_type(const TypeRef &ty,
                          const std::map<uint32_t, TypeRef> &subst);

  // Substitute and also replace generic type names with mangled versions
  TypeRef substitute_and_mangle_type(const TypeRef &ty,
                                     const std::map<uint32_t, TypeRef> &subst);

  typed::TypedExpr substitute_expr(const typed::TypedExpr &expr,
                                   const std::map<uint32_t, TypeRef> &subst);
  typed::TypedStmt substitute_stmt(const typed::TypedStmt &stmt,
                                   const std::map<uint32_t, TypeRef> &subst);
  typed::TypedBlock substitute_block(const typed::TypedBlock &block,
                                     const std::map<uint32_t, TypeRef> &subst);
  typed::TypedPattern
  substitute_pattern(const typed::TypedPattern &pat,
                     const std::map<uint32_t, TypeRef> &subst);

  TypeRef clone_type(const TypeRef &ty);
  typed::TypedExpr clone_expr(const typed::TypedExpr &expr);
  typed::TypedStmt clone_stmt(const typed::TypedStmt &stmt);
  typed::TypedBlock clone_block(const typed::TypedBlock &block);
  typed::TypedPattern clone_pattern(const typed::TypedPattern &pat);
  typed::TypedFnDecl clone_fn(const typed::TypedFnDecl &fn);
  typed::TypedStructDecl clone_struct(const typed::TypedStructDecl &s);
  typed::TypedUnionDecl clone_union(const typed::TypedUnionDecl &u);

  // Replace all references to generic types/functions with mangled names
  void replace_references(typed::TypedProgram &program);
  void replace_fn_refs(typed::TypedFnDecl &fn);
  void replace_block_refs(typed::TypedBlock &block);
  void replace_stmt_refs(typed::TypedStmt &stmt);
  void replace_expr_refs(typed::TypedExpr &expr);
  void replace_type_refs(TypeRef &ty);

  std::optional<std::string> get_mangled_type_name(const TypeRef &ty);
  bool is_concrete_type(const TypeRef &ty);
  bool has_typevars(const TypeRef &ty);
  bool is_generic_fn(const std::string &name);
  bool is_generic_struct(const std::string &name);
  bool is_generic_union(const std::string &name);

  // Build substitution map from forall vars to concrete types
  std::map<uint32_t, TypeRef>
  build_substitution(const ForAll &forall,
                     const std::vector<TypeRef> &type_args);

  // Get the monomorphized method name
  std::string mangle_method_name(const std::string &type_name,
                                 const std::string &method_name,
                                 const std::vector<TypeRef> &type_args,
                                 const std::vector<TypeRef> &method_type_args);
};

} // namespace shikimori
