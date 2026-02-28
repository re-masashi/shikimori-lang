#include <algorithm>
#include <functional>
#include <utility>
#include <variant>

#include "ast/ast.h"
#include "ast/typedast.h"
#include "typechecker.h"

#include "utils.h"

using namespace typed;

namespace shikimori {

void Typechecker::collect(const ast::Program &program) {
  import_resolver.collect(program);

  for (auto &[path, prog] : import_resolver.programs) {
    auto it = import_resolver.import_map.find(path);
    if (it != import_resolver.import_map.end()) {
      collect_from_program_filtered(*prog, it->second.items);
    }
  }

  collect_from_program(program);
}

void Typechecker::collect_from_program(const ast::Program &program) {
  collect_from_program_filtered(program, {});
}

void Typechecker::collect_from_program_filtered(
    const ast::Program &program, const vector<ast::ImportItem> &items) {
  bool import_all = items.empty();

  auto should_import = [&](const string &name) {
    if (import_all)
      return true;
    for (auto &item : items) {
      if (item.name == name)
        return true;
    }
    return false;
  };

  for (auto &declaration : program.declarations) {
    if (auto *fn = std::get_if<ast::FnDecl>(&declaration.value)) {
      if (should_import(fn->name))
        collect_fn(*fn);
    } else if (auto *strct = std::get_if<ast::StructDecl>(&declaration.value)) {
      if (should_import(strct->name))
        collect_struct(*strct);
    } else if (auto *union_decl =
                   std::get_if<ast::UnionDecl>(&declaration.value)) {
      if (should_import(union_decl->name))
        collect_union(*union_decl);
    } else if (auto *iface =
                   std::get_if<ast::InterfaceDecl>(&declaration.value)) {
      if (should_import(iface->name))
        collect_interface(*iface);
    } else if (auto *ext = std::get_if<ast::ExternDecl>(&declaration.value)) {
      if (should_import(ext->name))
        collect_extern(*ext);
    }
  }
}

void Typechecker::collect_fn(const ast::FnDecl &decl,
                             const string &name_prefix) {
  // Track which type vars we add at method level (vs inherited from struct)
  vector<pair<string, uint32_t>> method_type_vars;
  map<string, uint32_t> saved_vars;

  // Capture the type vars that were active before we added method-level ones
  // These represent the struct's type parameters that the method inherits
  vector<pair<string, uint32_t>> inherited_type_vars;
  for (auto const &[name, id] : current_type_vars) {
    inherited_type_vars.push_back({name, id});
  }
  // Sort by id to ensure consistent ordering
  sort(inherited_type_vars.begin(), inherited_type_vars.end(),
       [](const auto &a, const auto &b) { return a.second < b.second; });

  if (!decl.generic_params.empty()) {
    // Method has its own generic params. These come after any inherited ones
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      // Find the next available ID
      uint32_t next_id = static_cast<uint32_t>(current_type_vars.size());
      current_type_vars[decl.generic_params[i].name] = next_id;
      saved_vars[decl.generic_params[i].name] = next_id;
      method_type_vars.push_back({decl.generic_params[i].name, next_id});
    }
  }

  vector<TypeRef> arg_types;
  bool has_self = false;
  for (auto &param : decl.params) {
    if (!param.type) {
      // Self parameter. Use existential type variable wrapped in pointer (self
      // is always ref) ETVar will be solved during type inference
      has_self = true;
      auto self_ty_var = make_shared<Type>();
      self_ty_var->ty = ETVar{next_id++, "Self"};
      self_ty_var->span = param.span;

      // Wrap in pointer: self is *Self
      auto self_ty = make_shared<Type>();
      self_ty->ty = TyNamed{"ptr", Pointer, {self_ty_var}};
      self_ty->span = param.span;
      arg_types.push_back(self_ty);
    } else {
      arg_types.push_back(resolve_type(*param.type));
    }
  }

  TypeRef ret_type = make_shared<Type>();
  if (decl.return_type && *decl.return_type) {
    ret_type = resolve_type(**decl.return_type);
  } else {
    ret_type->ty = TyNamed{"()", Primitive, {}};
    ret_type->span = decl.span;
  }

  vector<FnConstraint> constraints;
  if (decl.where_clause) {
    for (auto &constraint : *decl.where_clause) {
      // constraint.name should be a type parameter
      // constraint.iface.names contains interface(s)
      for (auto &iface_name : constraint.iface.names) {
        if (!interfaces.contains(iface_name)) {
          throw TypeError("unknown interface in where clause: " + iface_name,
                          constraint.span);
        }
      }
      // Store the constraint
      FnConstraint c;
      c.type_param = constraint.name;
      c.interfaces = constraint.iface.names;
      constraints.push_back(c);
    }
  }

  // Clean up method-level type vars
  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
  }

  auto fn_ty = make_shared<Type>();
  fn_ty->ty = FnTy{arg_types, ret_type};
  fn_ty->span = decl.span;

  string full_fn_name =
      name_prefix.empty() ? decl.name : name_prefix + "." + decl.name;

  FnDef fn_def;
  fn_def.ty = fn_ty;
  fn_def.where_clause = std::move(constraints);
  fn_def.is_static = !has_self;

  // Wrap in ForAll if method has generic params (own or inherited)
  vector<pair<string, uint32_t>> all_type_vars = inherited_type_vars;
  all_type_vars.insert(all_type_vars.end(), method_type_vars.begin(),
                       method_type_vars.end());

  if (!all_type_vars.empty()) {
    auto forall = make_shared<Type>();
    forall->ty = ForAll{all_type_vars, fn_ty};
    forall->span = decl.span;
    fn_def.ty = forall;
  }

  functions[full_fn_name] = std::move(fn_def);
}

void Typechecker::collect_struct(const ast::StructDecl &decl) {
  StructDef def;

  map<string, uint32_t> saved_vars;
  if (!decl.generic_params.empty()) {
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      current_type_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
      saved_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
    }
  }

  for (auto &field : decl.fields) {
    def.fields.push_back({field.name, resolve_type(*field.type)});
  }

  if (!decl.generic_params.empty()) {
    vector<pair<string, uint32_t>> vars;
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      vars.push_back({decl.generic_params[i].name, static_cast<uint32_t>(i)});
    }
    auto body = make_shared<Type>();
    vector<TypeRef> tyvars;
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      auto tyvar = make_shared<Type>();
      tyvar->ty = TyVar{static_cast<uint32_t>(i), decl.generic_params[i].name};
      tyvars.push_back(tyvar);
    }
    body->ty = TyNamed{decl.name, Struct, tyvars};
    def.scheme = ForAll{vars, body};
  }

  structs[decl.name] = def;

  // Collect methods with type prefix (type vars still active)
  for (auto &method : decl.methods) {
    collect_fn(method, decl.name);
  }

  // Clean up type vars after all methods are collected
  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
  }
}

void Typechecker::collect_union(const ast::UnionDecl &decl) {
  UnionDef def;

  map<string, uint32_t> saved_vars;
  if (!decl.generic_params.empty()) {
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      current_type_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
      saved_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
    }
  }

  for (auto &variant : decl.variants) {
    vector<TypeRef> payload;
    if (variant.type && *variant.type) {
      payload.push_back(resolve_type(**variant.type));
    }
    def.variants.push_back({variant.name, payload});
  }

  if (!decl.generic_params.empty()) {
    vector<pair<string, uint32_t>> vars;
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      vars.push_back({decl.generic_params[i].name, static_cast<uint32_t>(i)});
    }
    auto body = make_shared<Type>();
    body->ty = TyNamed{decl.name, Union, {}};
    def.scheme = ForAll{vars, body};
  }

  unions[decl.name] = def;

  // Collect methods with type prefix (type vars still active)
  for (auto &method : decl.methods) {
    collect_fn(method, decl.name);
  }

  // Clean up type vars after all methods are collected
  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
  }
}

void Typechecker::collect_interface(const ast::InterfaceDecl &decl) {
  TyInterface iface;
  iface.name = decl.name;

  for (const auto &method : decl.methods) {
    vector<TypeRef> arg_types;
    for (const auto &param : method.params) {
      // Handle self parameter (no type annotation)
      if (!param.type) {
        // (self is always ref) ETVar will be solved during type inference
        auto self_ty_var = make_shared<Type>();
        self_ty_var->ty = ETVar{next_id++, "Self"};
        self_ty_var->span = param.span;

        // Wrap in pointer: self is *Self
        auto self_ty = make_shared<Type>();
        self_ty->ty = TyNamed{"ptr", Pointer, {self_ty_var}};
        self_ty->span = param.span;
        arg_types.push_back(self_ty);
      } else {
        arg_types.push_back(resolve_type(*param.type));
      }
    }
    TypeRef ret_type = make_shared<Type>();
    if (method.return_type && *method.return_type) {
      ret_type = resolve_type(**method.return_type);
    } else {
      ret_type->ty = TyNamed{"()", Primitive, {}};
      ret_type->span = method.span;
    }
    auto fn_ty = make_shared<Type>();
    fn_ty->ty = FnTy{arg_types, ret_type};
    fn_ty->span = method.span;
    iface.methods[method.name] = fn_ty;
  }

  interfaces[decl.name] = iface;
}

void Typechecker::collect_extern(const ast::ExternDecl &decl) {
  vector<TypeRef> arg_types;
  for (auto &param : decl.params) {
    arg_types.push_back(resolve_type(*param.type));
  }

  TypeRef ret_type = make_shared<Type>();
  if (decl.return_type && *decl.return_type) {
    ret_type = resolve_type(**decl.return_type);
  } else {
    ret_type->ty = TyNamed{"()", Primitive, {}};
    ret_type->span = decl.span;
  }

  auto fn_ty = make_shared<Type>();
  fn_ty->ty = FnTy{arg_types, ret_type};
  fn_ty->span = decl.span;

  FnDef fn_def;
  fn_def.ty = fn_ty;
  functions[decl.name] = std::move(fn_def);
}

TypeRef Typechecker::resolve_type(const ast::TypeAnnot &annot) {
  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, ast::PrimitiveType>) {
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          string name;
          switch (t) {
          case ast::PrimitiveType::I8:
            name = "i8";
            break;
          case ast::PrimitiveType::I16:
            name = "i16";
            break;
          case ast::PrimitiveType::I32:
            name = "i32";
            break;
          case ast::PrimitiveType::I64:
            name = "i64";
            break;
          case ast::PrimitiveType::U8:
            name = "u8";
            break;
          case ast::PrimitiveType::U16:
            name = "u16";
            break;
          case ast::PrimitiveType::U32:
            name = "u32";
            break;
          case ast::PrimitiveType::U64:
            name = "u64";
            break;
          case ast::PrimitiveType::F32:
            name = "f32";
            break;
          case ast::PrimitiveType::F64:
            name = "f64";
            break;
          case ast::PrimitiveType::Bool:
            name = "bool";
            break;
          case ast::PrimitiveType::Usize:
            name = "usize";
            break;
          case ast::PrimitiveType::String:
            name = "string";
            break;
          }
          ty->ty = TyNamed{name, Primitive, {}};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::PointerTypeAnnot>) {
          auto inner = resolve_type(*t.inner);
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = TyNamed{"ptr", Pointer, {inner}};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::SliceTypeAnnot>) {
          auto inner = resolve_type(*t.inner);
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = TyNamed{"Slice", Slice, {inner}};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::ArrayTypeAnnot>) {
          auto inner = resolve_type(*t.inner);
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = TyArray{inner, 0};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::OptionalTypeAnnot>) {
          auto inner = resolve_type(*t.inner);
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = TyNamed{"Option", Union, {inner}};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::InterfaceTypeAnnot>) {
          // interface Drawable or interface A + B
          // Create interface object type (fat pointer)
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = TyInterfaceObj{t.names, nullptr};
          return ty;

        } else if constexpr (std::is_same_v<T, ast::NamedTypeAnnot>) {
          if (current_type_vars.contains(t.name)) {
            auto ty = make_shared<Type>();
            ty->span = annot.span;
            ty->ty = TyVar{current_type_vars[t.name], t.name};
            return ty;
          }

          vector<TypeRef> args;
          for (auto &arg : t.generic_args)
            args.push_back(resolve_type(*arg));

          auto ty = make_shared<Type>();
          ty->span = annot.span;

          if (structs.contains(t.name))
            ty->ty = TyNamed{t.name, Struct, args};
          else if (unions.contains(t.name))
            ty->ty = TyNamed{t.name, Union, args};
          else
            throw TypeError("unknown type: " + t.name, annot.span);

          return ty;
        }

        throw TypeError("unhandled type annotation", annot.span);
      },
      annot.value);
}

void Typechecker::resolve_use(const ast::UseDecl &use, string file_path) {
  import_resolver.resolve_use(use, file_path);
}

typed::TypedProgram Typechecker::run(const ast::Program &program) {
  collect(program);
  return check(program);
}

// Scope management
void Typechecker::push_scope() { scopes.emplace_back(); }

void Typechecker::pop_scope() {
  if (scopes.empty())
    throw TypeError("scope underflow", Span{});
  scopes.pop_back();
}

void Typechecker::define_local(const string &name, TypeRef ty) {
  if (scopes.empty())
    throw TypeError("no active scope to define local", ty->span);
  scopes.back()[name] = ty;
}

optional<TypeRef> Typechecker::lookup_local(const string &name) {
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    if (it->contains(name)) {
      return it->at(name);
    }
  }
  return nullopt;
}

optional<TypeRef> Typechecker::lookup_any(const string &name) {
  // Check locals first
  if (auto local = lookup_local(name)) {
    return local;
  }
  // Check functions
  if (functions.contains(name)) {
    return functions.at(name).ty;
  }
  // Check structs (for static methods / constructors)
  if (structs.contains(name)) {
    if (functions.contains(name))
      return functions.at(name).ty;
    return nullopt;
  }
  // Check unions
  if (unions.contains(name)) {
    if (functions.contains(name))
      return functions.at(name).ty;
    return nullopt;
  }
  return nullopt;
}

// Existential type variables
TypeRef Typechecker::fresh_etvar(const string &name) {
  auto ty = make_shared<Type>();
  ty->ty = ETVar{fresh_id(), name};
  ty->span = Span{};
  return ty;
}

TypeRef Typechecker::apply_solutions(TypeRef ty) {
  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, ETVar>) {
          if (ty_solutions.contains(t.id)) {
            return apply_solutions(ty_solutions.at(t.id));
          }
          return ty;

        } else if constexpr (std::is_same_v<T, TyVar>) {
          return ty;

        } else if constexpr (std::is_same_v<T, TyNamed>) {
          vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(apply_solutions(arg));
          }
          auto result = make_shared<Type>();
          result->ty = TyNamed{t.name, t.kind, new_args};
          result->span = ty->span;
          return result;

        } else if constexpr (std::is_same_v<T, FnTy>) {
          vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(apply_solutions(arg));
          }
          auto result = make_shared<Type>();
          result->ty = FnTy{new_args, apply_solutions(t.return_type)};
          result->span = ty->span;
          return result;

        } else if constexpr (std::is_same_v<T, ForAll>) {
          // Don't substitute under forall
          return ty;

        } else if constexpr (std::is_same_v<T, TyArray>) {
          auto result = make_shared<Type>();
          result->ty = TyArray{apply_solutions(t.inner), t.size};
          result->span = ty->span;
          return result;

        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          auto result = make_shared<Type>();
          result->ty = TyInterfaceObj{
              t.interfaces, t.data_ty ? apply_solutions(t.data_ty) : nullptr};
          result->span = ty->span;
          return result;
        }

        throw TypeError("unhandled type in apply_solutions", ty->span);
      },
      ty->ty);
}

void Typechecker::check_solved(TypeRef ty, Span span) {
  auto solved = apply_solutions(ty);
  if (std::holds_alternative<ETVar>(solved->ty)) {
    auto &etvar = std::get<ETVar>(solved->ty);
    throw TypeError("could not infer type variable: " + etvar.name, span);
  }
}

// Instantiate polymorphic types
TypeRef Typechecker::instantiate(const ForAll &scheme) {
  map<uint32_t, TypeRef> subst;
  for (auto &[name, id] : scheme.vars) {
    auto etvar = fresh_etvar(name);
    subst[id] = etvar;
  }

  function<TypeRef(TypeRef)> do_subst = [&](TypeRef ty) -> TypeRef {
    return std::visit(
        [&](auto &&t) -> TypeRef {
          using T = std::decay_t<decltype(t)>;

          if constexpr (std::is_same_v<T, TyVar>) {
            if (subst.contains(t.id)) {
              return subst.at(t.id);
            }
            return ty;

          } else if constexpr (std::is_same_v<T, TyNamed>) {
            vector<TypeRef> new_args;
            for (auto &arg : t.args) {
              new_args.push_back(do_subst(arg));
            }
            auto result = make_shared<Type>();
            result->ty = TyNamed{t.name, t.kind, new_args};
            result->span = ty->span;
            return result;

          } else if constexpr (std::is_same_v<T, FnTy>) {
            vector<TypeRef> new_args;
            for (auto &arg : t.args) {
              new_args.push_back(do_subst(arg));
            }
            auto result = make_shared<Type>();
            result->ty = FnTy{new_args, do_subst(t.return_type)};
            result->span = ty->span;
            return result;

          } else if constexpr (std::is_same_v<T, TyArray>) {
            auto result = make_shared<Type>();
            result->ty = TyArray{do_subst(t.inner), t.size};
            result->span = ty->span;
            return result;

          } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
            auto result = make_shared<Type>();
            result->ty = TyInterfaceObj{
                t.interfaces, t.data_ty ? do_subst(t.data_ty) : nullptr};
            result->span = ty->span;
            return result;

          } else if constexpr (std::is_same_v<T, ForAll>) {
            // Don't substitute under forall
            return ty;

          } else {
            return ty;
          }
        },
        ty->ty);
  };

  return do_subst(scheme.body);
}

// Unification
void Typechecker::unify(TypeRef a, TypeRef b, Span span) {
  a = apply_solutions(a);
  b = apply_solutions(b);

  // Same type
  if (a == b)
    return;

  // Solve existential
  if (auto etvar = std::get_if<ETVar>(&a->ty)) {
    ty_solutions[etvar->id] = b;
    return;
  }

  if (auto etvar = std::get_if<ETVar>(&b->ty)) {
    ty_solutions[etvar->id] = a;
    return;
  }

  // Named types
  if (auto named_a = std::get_if<TyNamed>(&a->ty)) {
    if (auto named_b = std::get_if<TyNamed>(&b->ty)) {
      if (named_a->name != named_b->name || named_a->kind != named_b->kind) {
        throw TypeError(
            "type mismatch: " + named_a->name + " vs " + named_b->name, span);
      }
      if (named_a->args.size() != named_b->args.size()) {
        throw TypeError("arity mismatch for " + named_a->name, span);
      }
      for (size_t i = 0; i < named_a->args.size(); i++) {
        unify(named_a->args[i], named_b->args[i], span);
      }
      return;
    }
  }

  // Function types
  if (auto fn_a = std::get_if<FnTy>(&a->ty)) {
    if (auto fn_b = std::get_if<FnTy>(&b->ty)) {
      if (fn_a->args.size() != fn_b->args.size()) {
        throw TypeError("function arity mismatch", span);
      }
      for (size_t i = 0; i < fn_a->args.size(); i++) {
        unify(fn_a->args[i], fn_b->args[i], span);
      }
      unify(fn_a->return_type, fn_b->return_type, span);
      return;
    }
  }

  // Arrays
  if (auto arr_a = std::get_if<TyArray>(&a->ty)) {
    if (auto arr_b = std::get_if<TyArray>(&b->ty)) {
      if (arr_a->size != arr_b->size) {
        throw TypeError("array size mismatch", span);
      }
      unify(arr_a->inner, arr_b->inner, span);
      return;
    }
  }

  // Interface objects
  if (auto iface_a = std::get_if<TyInterfaceObj>(&a->ty)) {
    if (auto iface_b = std::get_if<TyInterfaceObj>(&b->ty)) {
      // Check if interfaces match (order independent)
      if (iface_a->interfaces.size() != iface_b->interfaces.size()) {
        throw TypeError("interface composition size mismatch", span);
      }
      // Sort and compare
      auto sorted_a = iface_a->interfaces;
      auto sorted_b = iface_b->interfaces;
      sort(sorted_a.begin(), sorted_a.end());
      sort(sorted_b.begin(), sorted_b.end());
      if (sorted_a != sorted_b) {
        throw TypeError("interface mismatch", span);
      }
      // If both have concrete data types, unify them
      if (iface_a->data_ty && iface_b->data_ty) {
        unify(iface_a->data_ty, iface_b->data_ty, span);
      }
      return;
    }
    // Interface object can unify with concrete type if it satisfies all
    // interfaces
    string concrete_name;
    if (auto named_b = std::get_if<TyNamed>(&b->ty)) {
      concrete_name = named_b->name;
    } else {
      throw TypeError("cannot unify interface with non-named type", span);
    }
    for (auto &iface_name : iface_a->interfaces) {
      check_satisfies(concrete_name, iface_name, span);
    }
    // Set the data type
    a->ty = TyInterfaceObj{iface_a->interfaces, b};
    return;
  }

  // Concrete type unifying with interface object
  if (auto iface_b = std::get_if<TyInterfaceObj>(&b->ty)) {
    string concrete_name;
    if (auto named_a = std::get_if<TyNamed>(&a->ty)) {
      concrete_name = named_a->name;
    } else {
      throw TypeError("cannot unify interface with non-named type", span);
    }
    for (auto &iface_name : iface_b->interfaces) {
      check_satisfies(concrete_name, iface_name, span);
    }
    b->ty = TyInterfaceObj{iface_b->interfaces, a};
    return;
  }

  throw TypeError("cannot unify types", span);
}

// Interface satisfaction check
void Typechecker::check_satisfies(const string &type_name,
                                  const string &iface_name, Span span) {
  if (!interfaces.contains(iface_name)) {
    throw TypeError("unknown interface: " + iface_name, span);
  }

  auto &iface = interfaces.at(iface_name);

  // Helper to check if a method signature matches
  auto check_method_signature = [&](const string &method_name,
                                    TypeRef iface_method_ty,
                                    TypeRef impl_method_ty, Span span) {
    // Unwrap forall if present
    TypeRef impl_fn_ty = impl_method_ty;
    if (auto forall = std::get_if<ForAll>(&impl_method_ty->ty)) {
      impl_fn_ty = instantiate(*forall);
    }

    auto *impl_fn = std::get_if<FnTy>(&impl_fn_ty->ty);
    auto *iface_fn = std::get_if<FnTy>(&iface_method_ty->ty);

    if (!impl_fn || !iface_fn) {
      throw TypeError("method '" + method_name + "' is not a function", span);
    }

    // Check parameter count (impl may have extra self param)
    size_t impl_param_count = impl_fn->args.size();
    size_t iface_param_count = iface_fn->args.size();

    // Interface methods should have self as first param
    // Implementation should match
    if (impl_param_count != iface_param_count) {
      throw TypeError("method '" + method_name +
                          "' has wrong parameter count: " + "expected " +
                          to_string(iface_param_count) + ", got " +
                          to_string(impl_param_count),
                      span);
    }

    // Check each parameter type
    for (size_t i = 0; i < iface_param_count; i++) {
      // Create fresh copies for unification
      auto impl_param = apply_solutions(impl_fn->args[i]);
      auto iface_param = apply_solutions(iface_fn->args[i]);

      // For self parameter, check that both are pointers to compatible types
      if (i == 0) {
        // Self parameter should be *ConcreteType (pointer to concrete type)
        // Check that both are pointer types
        auto *impl_ptr = std::get_if<TyNamed>(&impl_param->ty);
        auto *iface_ptr = std::get_if<TyNamed>(&iface_param->ty);

        if (!impl_ptr || !iface_ptr || impl_ptr->kind != Pointer ||
            iface_ptr->kind != Pointer) {
          throw TypeError("method '" + method_name +
                              "' self parameter must be a reference (pointer)",
                          span);
        }

        // The inner types should both be able to unify with the concrete type
        // For interface satisfaction, we just need to verify they're both
        // pointers The actual type will be checked when the method is called
      } else {
        // Other parameters must match exactly
        try {
          unify(impl_param, iface_param, span);
        } catch (const TypeError &) {
          throw TypeError("method '" + method_name + "' parameter " +
                              to_string(i) + " type mismatch",
                          span);
        }
      }
    }

    // Check return type
    auto impl_ret = apply_solutions(impl_fn->return_type);
    auto iface_ret = apply_solutions(iface_fn->return_type);
    try {
      unify(impl_ret, iface_ret, span);
    } catch (const TypeError &) {
      throw TypeError("method '" + method_name + "' return type mismatch",
                      span);
    }
  };

  // Check if the type has all required methods with matching signatures
  if (structs.contains(type_name)) {
    for (auto &[method_name, iface_method_ty] : iface.methods) {
      // Look for method in struct's methods
      string full_method_name = type_name + "." + method_name;
      if (!functions.contains(full_method_name)) {
        throw TypeError("type '" + type_name +
                            "' does not satisfy interface '" + iface_name +
                            "': missing method '" + method_name + "'",
                        span);
      }
      auto impl_method_ty = functions.at(full_method_name).ty;
      check_method_signature(method_name, iface_method_ty, impl_method_ty,
                             span);
    }
  } else if (unions.contains(type_name)) {
    // Similar check for unions
    for (auto &[method_name, iface_method_ty] : iface.methods) {
      string full_method_name = type_name + "." + method_name;
      if (!functions.contains(full_method_name)) {
        throw TypeError("type '" + type_name +
                            "' does not satisfy interface '" + iface_name +
                            "': missing method '" + method_name + "'",
                        span);
      }
      auto impl_method_ty = functions.at(full_method_name).ty;
      check_method_signature(method_name, iface_method_ty, impl_method_ty,
                             span);
    }
  } else {
    throw TypeError("type '" + type_name +
                        "' cannot satisfy interface: not a struct or union",
                    span);
  }
}

// Helper to make unit type
static TypeRef make_unit(Span span = Span{}) {
  auto ty = make_shared<Type>();
  ty->ty = TyNamed{"()", Primitive, {}};
  ty->span = span;
  return ty;
}

// Helper to make primitive type by name
static TypeRef make_primitive(const string &name, Span span = Span{}) {
  auto ty = make_shared<Type>();
  ty->ty = TyNamed{name, Primitive, {}};
  ty->span = span;
  return ty;
}

typed::TypedExpr Typechecker::check_call(const ast::Call &call, Span span) {
  auto callee = check_expr(*call.callee);
  auto callee_ty = apply_solutions(callee.ty);

  // Get the function name for where clause lookup
  string fn_name;
  if (auto *ident = std::get_if<typed::IdentifierExpr>(&callee.value)) {
    fn_name = ident->name;
  }

  // Look up function definition for where clause constraints
  const FnDef *fn_def = nullptr;
  if (!fn_name.empty() && functions.contains(fn_name)) {
    fn_def = &functions.at(fn_name);
  }

  // Unwrap ForAll if present
  TypeRef fn_ty_ref = callee_ty;
  if (auto forall = std::get_if<ForAll>(&callee_ty->ty)) {
    fn_ty_ref = instantiate(*forall);
  }

  auto fn_ty = std::get_if<FnTy>(&fn_ty_ref->ty);
  if (!fn_ty) {
    throw TypeError("callee is not a function", span);
  }

  if (call.args.size() != fn_ty->args.size()) {
    throw TypeError("argument count mismatch: expected " +
                        to_string(fn_ty->args.size()) + ", got " +
                        to_string(call.args.size()),
                    span);
  }

  // Check arguments and collect type variable bindings
  vector<unique_ptr<typed::TypedExpr>> typed_args;
  map<uint32_t, TypeRef> type_bindings; // tyvar id -> inferred type

  for (size_t i = 0; i < call.args.size(); i++) {
    auto arg = check_expr(*call.args[i]);
    unify(arg.ty, fn_ty->args[i], span);
    typed_args.push_back(make_unique<typed::TypedExpr>(std::move(arg)));
  }

  // Apply solutions to get inferred types
  for (size_t i = 0; i < fn_ty->args.size(); i++) {
    auto param_ty = apply_solutions(fn_ty->args[i]);
    auto arg_ty = apply_solutions(typed_args[i]->ty);

    // If param was a type variable, record the binding
    if (auto tvar = std::get_if<TyVar>(&fn_ty->args[i]->ty)) {
      type_bindings[tvar->id] = arg_ty;
    }
  }

  // Check where clause constraints
  if (fn_def && !fn_def->where_clause.empty()) {
    for (auto &constraint : fn_def->where_clause) {
      // Find the type variable id for this constraint
      uint32_t tvar_id = 0;
      bool found = false;
      if (auto forall = std::get_if<ForAll>(&callee_ty->ty)) {
        for (auto &[name, id] : forall->vars) {
          if (name == constraint.type_param) {
            tvar_id = id;
            found = true;
            break;
          }
        }
      }

      if (!found) {
        continue;
      }

      // Get the inferred type for this type variable
      TypeRef inferred_ty = nullptr;
      if (type_bindings.contains(tvar_id)) {
        inferred_ty = type_bindings.at(tvar_id);
      }

      if (!inferred_ty) {
        throw TypeError(
            "could not infer type for '" + constraint.type_param + "'", span);
      }

      // Get the concrete type name
      string type_name;
      if (auto named = std::get_if<TyNamed>(&inferred_ty->ty)) {
        type_name = named->name;
      } else {
        throw TypeError("type parameter '" + constraint.type_param +
                            "' must be a concrete type",
                        span);
      }

      // Check that the type satisfies all required interfaces
      for (auto &iface_name : constraint.interfaces) {
        check_satisfies(type_name, iface_name, span);
      }
    }
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(fn_ty->return_type);
  result->value =
      typed::Call{span, make_unique<typed::TypedExpr>(std::move(callee)),
                  std::move(typed_args)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_method_call(const ast::MethodCall &call,
                                                Span span) {
  auto obj = check_expr(*call.object);
  auto obj_ty = apply_solutions(obj.ty);

  // Look up method on the object's type
  string type_name;
  if (auto named = std::get_if<TyNamed>(&obj_ty->ty)) {
    type_name = named->name;
  } else {
    throw TypeError("method call on non-named type", span);
  }

  // Find the method
  string method_name = type_name + "." + call.method;
  if (!functions.contains(method_name)) {
    if (!functions.contains(call.method)) {
      throw TypeError("method not found: " + call.method + " on " + type_name,
                      span);
    }
    method_name = call.method;
  }

  auto fn_def = functions.at(method_name);
  TypeRef fn_ty_ref;

  // If the method is generic, we need to instantiate it
  if (auto forall = std::get_if<ForAll>(&fn_def.ty->ty)) {
    // Build substitution from object type
    map<uint32_t, TypeRef> subst;

    // Extract type args from object type for struct type params
    if (auto obj_named = std::get_if<TyNamed>(&obj_ty->ty)) {
      // Assume first N type params are from struct, rest are method-specific
      size_t struct_type_params = obj_named->args.size();
      for (size_t i = 0; i < struct_type_params && i < forall->vars.size();
           i++) {
        subst[forall->vars[i].second] = obj_named->args[i];
      }
      // For method-specific type params, use etvars (will be inferred from
      // args)
      for (size_t i = struct_type_params; i < forall->vars.size(); i++) {
        subst[forall->vars[i].second] = fresh_etvar(forall->vars[i].first);
      }
    } else {
      // No object type args, use etvars for all
      for (auto &[name, id] : forall->vars) {
        subst[id] = fresh_etvar(name);
      }
    }

    // Apply substitution to instantiate the method type
    function<TypeRef(TypeRef)> substitute;
    substitute = [&](TypeRef ty) -> TypeRef {
      return std::visit(
          [&](auto &&inner) -> TypeRef {
            using U = std::decay_t<decltype(inner)>;
            if constexpr (std::is_same_v<U, TyVar>) {
              if (subst.contains(inner.id)) {
                return subst.at(inner.id);
              }
              return ty;
            } else if constexpr (std::is_same_v<U, TyNamed>) {
              vector<TypeRef> new_args;
              for (auto &arg : inner.args) {
                new_args.push_back(substitute(arg));
              }
              auto result = make_shared<Type>();
              result->ty = TyNamed{inner.name, inner.kind, new_args};
              result->span = ty->span;
              return result;
            }
            return ty;
          },
          ty->ty);
    };

    fn_ty_ref = std::visit(
        [&](auto &&t) -> TypeRef {
          using T = std::decay_t<decltype(t)>;
          if constexpr (std::is_same_v<T, FnTy>) {
            vector<TypeRef> new_args;
            for (auto &arg : t.args) {
              new_args.push_back(substitute(arg));
            }
            auto result = make_shared<Type>();
            result->ty = FnTy{new_args, substitute(t.return_type)};
            result->span = forall->body->span;
            return result;
          }
          return forall->body;
        },
        forall->body->ty);
  } else {
    fn_ty_ref = fn_def.ty;
  }

  auto fn_ty = std::get_if<FnTy>(&fn_ty_ref->ty);
  if (!fn_ty) {
    throw TypeError("method is not a function", span);
  }

  // Check self parameter
  if (fn_ty->args.empty()) {
    throw TypeError("method has no self parameter", span);
  }

  auto self_param = fn_ty->args[0];
  auto *self_ptr = std::get_if<TyNamed>(&self_param->ty);
  if (!self_ptr || self_ptr->kind != Pointer) {
    throw TypeError("method self parameter must be a reference (pointer)",
                    span);
  }

  if (self_ptr->args.empty()) {
    throw TypeError("method self pointer has no inner type", span);
  }
  unify(obj.ty, self_ptr->args[0], span);

  // Check remaining args
  if (call.args.size() != fn_ty->args.size() - 1) {
    throw TypeError("argument count mismatch for method", span);
  }

  vector<unique_ptr<typed::TypedExpr>> typed_args;
  for (size_t i = 0; i < call.args.size(); i++) {
    auto arg = check_expr(*call.args[i]);
    unify(arg.ty, fn_ty->args[i + 1], span);
    typed_args.push_back(make_unique<typed::TypedExpr>(std::move(arg)));
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(fn_ty->return_type);
  result->value =
      typed::MethodCall{span, make_unique<typed::TypedExpr>(std::move(obj)),
                        call.method, std::move(typed_args)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_field_access(const ast::FieldAccess &fa,
                                                 Span span) {
  auto obj = check_expr(*fa.object);
  auto obj_ty = apply_solutions(obj.ty);

  string type_name;
  if (auto named = std::get_if<TyNamed>(&obj_ty->ty)) {
    type_name = named->name;
  } else {
    throw TypeError("field access on non-named type", span);
  }

  if (!structs.contains(type_name)) {
    throw TypeError("type has no fields: " + type_name, span);
  }

  auto &s = structs.at(type_name);

  // Build substitution map from struct type args to field type vars
  map<uint32_t, TypeRef> subst;
  if (auto named = std::get_if<TyNamed>(&obj_ty->ty)) {
    // obj_ty is Pair[string, i32], we need to map TyVar IDs to these args
    if (s.scheme) {
      for (size_t i = 0; i < s.scheme->vars.size() && i < named->args.size();
           i++) {
        subst[s.scheme->vars[i].second] = named->args[i];
      }
    }
  }

  TypeRef field_ty = nullptr;
  size_t field_idx = 0;
  for (auto &[fname, fty] : s.fields) {
    if (fname == fa.field) {
      // Apply substitution to get the actual field type
      if (auto tvar = std::get_if<TyVar>(&fty->ty)) {
        if (subst.contains(tvar->id)) {
          field_ty = subst.at(tvar->id);
        } else {
          field_ty = fty;
        }
      } else {
        field_ty = fty;
      }
      break;
    }
    field_idx++;
  }

  if (!field_ty) {
    throw TypeError("field not found: " + fa.field + " in " + type_name, span);
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(field_ty);
  result->value = typed::FieldAccess{
      span, make_unique<typed::TypedExpr>(std::move(obj)), fa.field};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_scope_access(const ast::ScopeAccess &sa,
                                                 Span span) {
  // Scope access like Foo::new() or Foo::bar or Union::variant(value)

  // First check: is this a union variant initialization?
  if (unions.contains(sa.scope)) {
    auto &u = unions.at(sa.scope);
    bool found_variant = false;
    size_t variant_idx = 0;

    for (size_t i = 0; i < u.variants.size(); i++) {
      if (u.variants[i].first == sa.member) {
        found_variant = true;
        variant_idx = i;
        break;
      }
    }

    if (found_variant) {
      auto &[var_name, var_payload] = u.variants[variant_idx];

      if (var_payload.empty()) {
        if (!sa.payload.empty()) {
          throw TypeError("variant '" + sa.member + "' has no payload", span);
        }
      } else {
        if (sa.payload.size() != var_payload.size()) {
          throw TypeError("variant '" + sa.member + "' expects " +
                              to_string(var_payload.size()) + " argument(s)",
                          span);
        }
        for (size_t i = 0; i < sa.payload.size(); i++) {
          auto arg = check_expr(*sa.payload[i]);
          unify(arg.ty, var_payload[i], sa.payload[i]->span);
        }
      }

      TypeRef union_ty;
      if (u.scheme) {
        union_ty = instantiate(*u.scheme);
      } else {
        union_ty = make_shared<Type>();
        union_ty->ty = TyNamed{sa.scope, Union, {}};
        union_ty->span = span;
      }

      vector<unique_ptr<typed::TypedExpr>> typed_payload;
      for (auto &arg : sa.payload) {
        typed_payload.push_back(
            make_unique<typed::TypedExpr>(check_expr(*arg)));
      }

      auto result = make_shared<typed::TypedExpr>();
      result->span = span;
      result->ty = apply_solutions(union_ty);
      result->value = typed::UnionVariantInit{span, sa.scope, sa.member,
                                              std::move(typed_payload)};
      return std::move(*result);
    }
    // Variant not found. Fall through to check for static methods
  }

  // Second check: is this a static method call?
  // Look for the function
  string method_name = sa.scope + "." + sa.member;
  if (!functions.contains(method_name) && !functions.contains(sa.member)) {
    throw TypeError("not found: " + sa.member +
                        (unions.contains(sa.scope)
                             ? " (not a variant of union '" + sa.scope + "')"
                             : " in scope '" + sa.scope + "'"),
                    span);
  }

  auto fn_def = functions.contains(method_name) ? functions.at(method_name)
                                                : functions.at(sa.member);
  TypeRef fn_ty_ref;

  if (auto forall = std::get_if<ForAll>(&fn_def.ty->ty)) {
    fn_ty_ref = instantiate(*forall);
  } else {
    fn_ty_ref = fn_def.ty;
  }

  auto fn_ty = std::get_if<FnTy>(&fn_ty_ref->ty);
  if (!fn_ty) {
    throw TypeError("scope member is not a function", span);
  }

  // Check args
  if (sa.payload.size() != fn_ty->args.size()) {
    throw TypeError("argument count mismatch", span);
  }

  vector<unique_ptr<typed::TypedExpr>> typed_args;
  for (size_t i = 0; i < sa.payload.size(); i++) {
    auto arg = check_expr(*sa.payload[i]);
    auto expected_ty = fn_ty->args[i];

    // If this is the self parameter (pointer) and arg is not a pointer,
    // automatically take address
    auto expected_ptr = std::get_if<TyNamed>(&expected_ty->ty);
    if (i == 0 && expected_ptr && expected_ptr->kind == Pointer) {
      auto arg_ty = apply_solutions(arg.ty);
      auto arg_named = std::get_if<TyNamed>(&arg_ty->ty);
      bool arg_is_pointer = arg_named && arg_named->kind == Pointer;

      if (!arg_is_pointer) {
        // arg is not already a pointer, wrap in &
        auto inner_ty = expected_ptr->args[0];
        unify(arg.ty, inner_ty, sa.payload[i]->span);

        auto addr_of_expr = make_unique<typed::TypedExpr>();
        addr_of_expr->span = span;
        addr_of_expr->ty = expected_ty;
        addr_of_expr->value =
            typed::UnaryExpr{span, ast::UnaryOp::AddrOf,
                             make_unique<typed::TypedExpr>(std::move(arg))};
        typed_args.push_back(std::move(addr_of_expr));
        continue;
      }
    }

    unify(arg.ty, expected_ty, span);
    typed_args.push_back(make_unique<typed::TypedExpr>(std::move(arg)));
  }

  // Create a synthetic callee expression
  auto callee_expr = make_unique<typed::TypedExpr>();
  callee_expr->span = span;
  callee_expr->ty = fn_ty_ref;
  callee_expr->value = typed::IdentifierExpr{span, sa.member, fn_ty_ref};

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(fn_ty->return_type);
  result->value =
      typed::Call{span, std::move(callee_expr), std::move(typed_args)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_binary(const ast::BinaryExpr &bin,
                                           Span span) {
  auto left = check_expr(*bin.left);
  auto right = check_expr(*bin.right);

  using ast::BinaryOp;

  // Arithmetic operators
  if (bin.op == BinaryOp::Add || bin.op == BinaryOp::Sub ||
      bin.op == BinaryOp::Mul || bin.op == BinaryOp::Div ||
      bin.op == BinaryOp::Mod) {
    unify(left.ty, right.ty, span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(left.ty);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  // Comparison operators
  if (bin.op == BinaryOp::Lt || bin.op == BinaryOp::Gt ||
      bin.op == BinaryOp::Lte || bin.op == BinaryOp::Gte) {
    unify(left.ty, right.ty, span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = make_primitive("bool", span);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  // Equality operators
  if (bin.op == BinaryOp::Eq || bin.op == BinaryOp::Neq) {
    unify(left.ty, right.ty, span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = make_primitive("bool", span);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  // Bitwise operators
  if (bin.op == BinaryOp::BitAnd || bin.op == BinaryOp::BitXor ||
      bin.op == BinaryOp::BitOr) {
    unify(left.ty, right.ty, span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(left.ty);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  // Shift operators
  if (bin.op == BinaryOp::LShift || bin.op == BinaryOp::RShift) {
    unify(right.ty, make_primitive("usize", span), span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(left.ty);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  // Logical operators
  if (bin.op == BinaryOp::And || bin.op == BinaryOp::Or) {
    unify(left.ty, make_primitive("bool", span), span);
    unify(right.ty, make_primitive("bool", span), span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = make_primitive("bool", span);
    result->value = typed::BinaryExpr{
        span, bin.op, make_unique<typed::TypedExpr>(std::move(left)),
        make_unique<typed::TypedExpr>(std::move(right))};
    return std::move(*result);
  }

  throw TypeError("unknown binary operator", span);
}

typed::TypedExpr Typechecker::check_unary(const ast::UnaryExpr &un, Span span) {
  using ast::UnaryOp;

  auto operand = check_expr(*un.operand);

  if (un.op == UnaryOp::Not) {
    unify(operand.ty, make_primitive("bool", span), span);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = make_primitive("bool", span);
    result->value = typed::UnaryExpr{
        span, un.op, make_unique<typed::TypedExpr>(std::move(operand))};
    return std::move(*result);
  }

  if (un.op == UnaryOp::Neg) {
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(operand.ty);
    result->value = typed::UnaryExpr{
        span, un.op, make_unique<typed::TypedExpr>(std::move(operand))};
    return std::move(*result);
  }

  if (un.op == UnaryOp::BitNot) {
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(operand.ty);
    result->value = typed::UnaryExpr{
        span, un.op, make_unique<typed::TypedExpr>(std::move(operand))};
    return std::move(*result);
  }

  if (un.op == UnaryOp::Deref) {
    // Expect pointer type
    auto inner_ty = fresh_etvar("deref");
    auto ptr_ty = make_shared<Type>();
    ptr_ty->ty = TyNamed{"ptr", Pointer, {inner_ty}};
    ptr_ty->span = span;
    unify(operand.ty, ptr_ty, span);

    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = apply_solutions(inner_ty);
    result->value = typed::UnaryExpr{
        span, un.op, make_unique<typed::TypedExpr>(std::move(operand))};
    return std::move(*result);
  }

  if (un.op == UnaryOp::AddrOf) {
    auto inner_ty = apply_solutions(operand.ty);
    auto result = make_shared<typed::TypedExpr>();
    result->span = span;
    result->ty = make_shared<Type>();
    result->ty->ty = TyNamed{"ptr", Pointer, {inner_ty}};
    result->ty->span = span;
    result->value = typed::UnaryExpr{
        span, un.op, make_unique<typed::TypedExpr>(std::move(operand))};
    return std::move(*result);
  }

  throw TypeError("unknown unary operator", span);
}

typed::TypedExpr Typechecker::check_if(const ast::IfExpr &if_expr, Span span) {
  // Check all branches
  vector<typed::IfBranch> typed_branches;
  TypeRef branch_ty = nullptr;

  for (auto &branch : if_expr.branches) {
    auto cond = check_expr(*branch.condition);
    unify(cond.ty, make_primitive("bool", branch.span), branch.span);

    push_scope();
    auto body = check_block(branch.body);
    pop_scope();

    if (!branch_ty) {
      branch_ty = body.ty;
    } else {
      unify(branch_ty, body.ty, branch.span);
    }

    typed_branches.push_back(typed::IfBranch{
        branch.span, make_unique<typed::TypedExpr>(std::move(cond)),
        std::move(body)});
  }

  optional<typed::TypedBlock> typed_else;
  if (if_expr.else_branch) {
    push_scope();
    auto else_body = check_block(*if_expr.else_branch);
    pop_scope();

    if (branch_ty) {
      unify(branch_ty, else_body.ty, if_expr.else_branch->span);
    } else {
      branch_ty = else_body.ty;
    }
    typed_else = std::move(else_body);
  }

  if (!branch_ty) {
    branch_ty = make_unit(span);
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(branch_ty);
  result->value =
      typed::IfExpr{span, std::move(typed_branches), std::move(typed_else)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_match(const ast::MatchExpr &match,
                                          Span span) {
  auto subject = check_expr(*match.subject);
  auto subject_ty = apply_solutions(subject.ty);

  // Get union type for variant lookup
  string subject_union_name;
  const UnionDef *union_def = nullptr;
  if (auto named = std::get_if<TyNamed>(&subject_ty->ty)) {
    if (named->kind == Union) {
      subject_union_name = named->name;
      if (unions.contains(named->name)) {
        union_def = &unions.at(named->name);
      }
    }
  }

  vector<typed::MatchArm> typed_arms;
  TypeRef arm_ty = nullptr;

  for (auto &arm : match.arms) {
    push_scope();

    // Check pattern and bind variables
    typed::TypedPattern typed_pattern;
    typed_pattern.span = arm.pattern.span;
    typed_pattern.ty = subject_ty;
    typed_pattern.value = arm.pattern.value;

    // Bind pattern variables
    if (auto *var_pat = std::get_if<ast::VariantPattern>(&arm.pattern.value)) {
      if (union_def) {
        TypeRef payload_ty = nullptr;
        for (auto &[var_name, var_payload] : union_def->variants) {
          if (var_name == var_pat->variant) {
            if (!var_payload.empty()) {
              payload_ty = var_payload[0];
            } else {
              payload_ty = make_unit(arm.pattern.span);
            }
            break;
          }
        }
        if (payload_ty && var_pat->binding) {
          define_local(*var_pat->binding, payload_ty);
        }
      }
    } else if (auto *ident_pat =
                   std::get_if<ast::IdentPattern>(&arm.pattern.value)) {
      define_local(ident_pat->name, subject_ty);
    } else if (auto *lit_pat =
                   std::get_if<ast::LiteralPattern>(&arm.pattern.value)) {
      // Check literal pattern type matches subject type
      TypeRef lit_type = std::visit(
          [&](auto &&lit) -> TypeRef {
            using L = std::decay_t<decltype(lit)>;
            if constexpr (std::is_same_v<L, ast::IntLiteral>) {
              return make_primitive("i32", lit.span);
            } else if constexpr (std::is_same_v<L, ast::FloatLiteral>) {
              return make_primitive("f64", lit.span);
            } else if constexpr (std::is_same_v<L, ast::BoolLiteral>) {
              return make_primitive("bool", lit.span);
            } else if constexpr (std::is_same_v<L, ast::StringLiteral>) {
              return make_primitive("string", lit.span);
            } else if constexpr (std::is_same_v<L, ast::NullLiteral>) {
              return fresh_etvar("null");
            }
            return subject_ty; // Fallback
          },
          lit_pat->value);

      try {
        unify(subject_ty, lit_type, arm.pattern.span);
      } catch (const TypeError &) {
        throw TypeError("literal pattern type does not match subject type",
                        arm.pattern.span);
      }
    }
    // Wildcard patterns don't need type checking

    auto body = check_block(arm.body);
    pop_scope();

    if (!arm_ty) {
      arm_ty = body.ty;
    } else {
      unify(arm_ty, body.ty, arm.span);
    }

    typed_arms.push_back(
        typed::MatchArm{arm.span, std::move(typed_pattern), std::move(body)});
  }

  if (!arm_ty) {
    arm_ty = make_unit(span);
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(arm_ty);
  result->value =
      typed::MatchExpr{span, make_unique<typed::TypedExpr>(std::move(subject)),
                       std::move(typed_arms)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_struct_init(const ast::StructInit &init,
                                                Span span) {
  if (!structs.contains(init.name)) {
    throw TypeError("unknown struct: " + init.name, span);
  }

  auto &s = structs.at(init.name);
  TypeRef struct_ty;

  // Build substitution map for type variables
  map<uint32_t, TypeRef> subst;
  if (s.scheme) {
    for (auto &[name, id] : s.scheme->vars) {
      subst[id] = fresh_etvar(name);
    }

    // Apply substitution to get instantiated struct type
    auto body = s.scheme->body;
    struct_ty = std::visit(
        [&](auto &&t) -> TypeRef {
          using T = std::decay_t<decltype(t)>;
          if constexpr (std::is_same_v<T, TyNamed>) {
            vector<TypeRef> new_args;
            for (auto &arg : t.args) {
              if (auto tvar = std::get_if<TyVar>(&arg->ty)) {
                if (subst.contains(tvar->id)) {
                  new_args.push_back(subst.at(tvar->id));
                } else {
                  new_args.push_back(arg);
                }
              } else {
                new_args.push_back(arg);
              }
            }
            auto result = make_shared<Type>();
            result->ty = TyNamed{t.name, t.kind, new_args};
            result->span = span;
            return result;
          }
          return body;
        },
        body->ty);
  } else {
    struct_ty = make_shared<Type>();
    struct_ty->ty = TyNamed{init.name, Struct, {}};
    struct_ty->span = span;
  }

  // Check fields
  vector<pair<Identifier, unique_ptr<typed::TypedExpr>>> typed_fields;

  for (size_t i = 0; i < init.fields.size(); i++) {
    auto &[fname, fexpr] = init.fields[i];
    TypeRef expected_ty = nullptr;

    // Find the field in the definition and apply substitution
    for (size_t j = 0; j < s.fields.size(); j++) {
      if (s.fields[j].first == fname) {
        auto field_ty = s.fields[j].second;
        // Apply substitution to field type
        if (auto tvar = std::get_if<TyVar>(&field_ty->ty)) {
          if (subst.contains(tvar->id)) {
            expected_ty = subst.at(tvar->id);
          } else {
            expected_ty = field_ty;
          }
        } else {
          expected_ty = field_ty;
        }
        break;
      }
    }

    if (!expected_ty) {
      throw TypeError("unknown field: " + fname + " in " + init.name, span);
    }

    auto checked = check_expr(*fexpr);
    unify(checked.ty, expected_ty, fexpr->span);
    typed_fields.push_back(
        {fname, make_unique<typed::TypedExpr>(std::move(checked))});
  }

  auto result = make_shared<typed::TypedExpr>();
  result->span = span;
  result->ty = apply_solutions(struct_ty);
  result->value = typed::StructInit{span, init.name, std::move(typed_fields)};
  return std::move(*result);
}

typed::TypedExpr Typechecker::check_builtin(const ast::BuiltinCall &call,
                                            Span span) {
  // Builtins: @typename, @typeid, @sizeof, @alignof, @fields, @variants,
  // @has_field, @field
  auto result = make_shared<typed::TypedExpr>();
  result->span = span;

  if (call.name == "typename" || call.name == "typeid" ||
      call.name == "sizeof" || call.name == "alignof") {
    if (call.type_args.empty()) {
      throw TypeError("@" + call.name + " requires a type argument", span);
    }

    auto type_arg = resolve_type(*call.type_args[0]);

    if (call.name == "typename") {
      result->ty = make_primitive("string", span);
    } else {
      result->ty = make_primitive("usize", span);
    }

    vector<unique_ptr<typed::TypedExpr>> typed_args;
    result->value =
        typed::BuiltinCall{span, call.name, {type_arg}, std::move(typed_args)};
    return std::move(*result);
  }

  if (call.name == "has_field") {
    if (call.type_args.empty()) {
      throw TypeError("@has_field requires a type argument", span);
    }
    if (call.args.size() != 1) {
      throw TypeError("@has_field expects 1 argument (field name)", span);
    }

    auto type_arg = resolve_type(*call.type_args[0]);

    result->ty = make_primitive("bool", span);
    vector<unique_ptr<typed::TypedExpr>> typed_args;
    typed_args.push_back(
        make_unique<typed::TypedExpr>(check_expr(*call.args[0])));
    result->value =
        typed::BuiltinCall{span, call.name, {type_arg}, std::move(typed_args)};
    return std::move(*result);
  }

  if (call.name == "fields" || call.name == "variants") {
    if (call.type_args.empty()) {
      throw TypeError("@" + call.name + " requires a type argument", span);
    }

    auto type_arg = resolve_type(*call.type_args[0]);

    result->ty =
        make_primitive("usize", span); // Placeholder for comptime array
    vector<unique_ptr<typed::TypedExpr>> typed_args;
    result->value =
        typed::BuiltinCall{span, call.name, {type_arg}, std::move(typed_args)};
    return std::move(*result);
  }

  if (call.name == "field") {
    if (call.args.size() != 2) {
      throw TypeError("@field expects 2 arguments", span);
    }
    auto val = check_expr(*call.args[0]);
    // Second arg should be a string literal
    result->ty = fresh_etvar("field");
    vector<unique_ptr<typed::TypedExpr>> typed_args;
    typed_args.push_back(make_unique<typed::TypedExpr>(std::move(val)));
    typed_args.push_back(
        make_unique<typed::TypedExpr>(check_expr(*call.args[1])));
    result->value =
        typed::BuiltinCall{span, call.name, {}, std::move(typed_args)};
    return std::move(*result);
  }

  throw TypeError("unknown builtin: " + call.name, span);
}

typed::TypedBlock Typechecker::check_block(const ast::Block &block) {
  push_scope();

  typed::TypedBlock result;
  result.span = block.span;
  result.ty = make_unit(block.span);

  for (auto &stmt : block.statements) {
    auto checked = check_stmt(*stmt);
    result.stmts.push_back(make_unique<typed::TypedStmt>(std::move(checked)));

    if (std::holds_alternative<unique_ptr<typed::TypedExpr>>(checked.value)) {
      result.ty = make_unit(block.span);
    }
  }

  pop_scope();
  return result;
}

typed::TypedStmt Typechecker::check_stmt(const ast::Stmt &stmt) {
  return std::visit(
      [&](auto &&s) -> typed::TypedStmt {
        using T = std::decay_t<decltype(s)>;
        auto result = make_shared<typed::TypedStmt>();
        result->span = stmt.span;

        if constexpr (std::is_same_v<T, ast::LetStmt>) {
          auto init = check_expr(*s.init);
          TypeRef let_ty;

          if (s.type && *s.type) {
            let_ty = resolve_type(**s.type);
            unify(init.ty, let_ty, s.span);
          } else {
            let_ty = apply_solutions(init.ty);
          }

          define_local(s.name, let_ty);

          result->value =
              typed::LetStmt{s.span, s.name, let_ty,
                             make_unique<typed::TypedExpr>(std::move(init))};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::ReturnStmt>) {
          if (s.value) {
            auto val = check_expr(**s.value);
            if (current_return_type) {
              unify(val.ty, current_return_type, s.span);
            }
            result->value = typed::ReturnStmt{
                s.span, make_unique<typed::TypedExpr>(std::move(val))};
          } else {
            if (current_return_type) {
              auto ret_ty = apply_solutions(current_return_type);
              bool is_unit = false;
              if (auto named = std::get_if<TyNamed>(&ret_ty->ty)) {
                is_unit = (named->name == "()");
              }
              if (!is_unit) {
                throw TypeError("return type is not unit: expected '" +
                                    std::get<TyNamed>(ret_ty->ty).name +
                                    "', got '()'",
                                s.span);
              }
            }
            result->value = typed::ReturnStmt{s.span, nullopt};
          }
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::DeferStmt>) {
          auto checked = check_stmt(*s.stmt);
          result->value = typed::DeferStmt{
              s.span, make_unique<typed::TypedStmt>(std::move(checked))};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::LoopStmt>) {
          push_scope();
          auto body = check_block(s.body);
          pop_scope();

          result->value = typed::LoopStmt{s.span, s.label, std::move(body)};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::WhileStmt>) {
          auto cond = check_expr(*s.condition);
          unify(cond.ty, make_primitive("bool", s.span), s.span);

          push_scope();
          auto body = check_block(s.body);
          pop_scope();

          result->value = typed::WhileStmt{
              s.span, s.label, make_unique<typed::TypedExpr>(std::move(cond)),
              std::move(body)};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::ForStmt>) {
          auto iter = check_expr(*s.iterable);
          auto iter_ty = apply_solutions(iter.ty);
          TypeRef elem_ty;

          // Infer element type from iterable
          if (auto arr = std::get_if<TyArray>(&iter_ty->ty)) {
            // Array [N]T -> element type is T
            elem_ty = apply_solutions(arr->inner);
          } else if (auto named = std::get_if<TyNamed>(&iter_ty->ty)) {
            if (named->name == "Slice") {
              // Slice []T -> element type is T (always has args)
              elem_ty = apply_solutions(named->args[0]);
            } else if (named->name == "Range") {
              // Range[T] -> element type is T
              if (named->args.empty()) {
                throw TypeError("Range type requires element type: Range[T]",
                                s.span);
              }
              elem_ty = apply_solutions(named->args[0]);
            } else {
              elem_ty = fresh_etvar("for_elem");
            }
          } else {
            elem_ty = fresh_etvar("for_elem");
          }

          define_local(s.var, elem_ty);

          push_scope();
          auto body = check_block(s.body);
          pop_scope();

          result->value =
              typed::ForStmt{s.span,
                             s.label,
                             s.var,
                             apply_solutions(elem_ty),
                             make_unique<typed::TypedExpr>(std::move(iter)),
                             std::move(body)};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::ComptimeStmt>) {
          throw TypeError("comptime statements not implemented", stmt.span);

        } else if constexpr (std::is_same_v<T, ast::ExprStmt>) {
          auto expr = check_expr(*s.expr);
          result->value = make_unique<typed::TypedExpr>(std::move(expr));
          return std::move(*result);
        }

        throw TypeError("unhandled statement form", stmt.span);
      },
      stmt.value);
}

typed::TypedFnDecl Typechecker::check_fn(const ast::FnDecl &decl) {
  push_scope();

  map<string, uint32_t> saved_vars;
  if (!decl.generic_params.empty()) {
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      current_type_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
      saved_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
    }
  }

  vector<typed::TypedParam> typed_params;
  vector<TypeRef> param_types;

  for (auto &param : decl.params) {
    auto param_ty = resolve_type(*param.type);
    param_types.push_back(param_ty);
    define_local(param.name, param_ty);
    typed_params.push_back(typed::TypedParam{param.span, param.name, param_ty});
  }

  TypeRef ret_type;
  if (decl.return_type && *decl.return_type) {
    ret_type = resolve_type(**decl.return_type);
  } else {
    ret_type = make_unit(decl.span);
  }

  auto saved_ret = current_return_type;
  current_return_type = ret_type;

  auto body = check_block(decl.body);

  current_return_type = saved_ret;
  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
  }
  pop_scope();

  auto fn_ty = make_shared<Type>();
  fn_ty->ty = FnTy{param_types, ret_type};
  fn_ty->span = decl.span;

  TypeRef final_ty = fn_ty;

  if (!decl.generic_params.empty()) {
    vector<pair<string, uint32_t>> vars;
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      vars.push_back({decl.generic_params[i].name, static_cast<uint32_t>(i)});
    }
    auto forall = make_shared<Type>();
    forall->ty = ForAll{vars, fn_ty};
    forall->span = decl.span;
    final_ty = forall;
  }

  return typed::TypedFnDecl{decl.span, decl.name,
                            final_ty,  std::move(typed_params),
                            ret_type,  std::move(body)};
}

typed::TypedExpr Typechecker::check_expr(const ast::Expr &expr) {
  return std::visit(
      [&](auto &&e) -> typed::TypedExpr {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, ast::IntLiteral>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_primitive("i32", e.span);
          result->value = typed::IntLiteral{e.span, e.value};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::FloatLiteral>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_primitive("f64", e.span);
          result->value = typed::FloatLiteral{e.span, e.value};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::BoolLiteral>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_primitive("bool", e.span);
          result->value = typed::BoolLiteral{e.span, e.value};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::StringLiteral>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_primitive("string", e.span);
          result->value = typed::StringLiteral{e.span, e.value};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::NullLiteral>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = fresh_etvar("null");
          result->value = typed::NullLiteral{e.span};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::IdentifierExpr>) {
          auto found = lookup_any(e.name);
          if (!found) {
            throw TypeError("undefined variable: " + e.name, e.span);
          }
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = apply_solutions(*found);
          result->value = typed::IdentifierExpr{e.span, e.name, result->ty};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::Call>) {
          return check_call(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::MethodCall>) {
          return check_method_call(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::FieldAccess>) {
          return check_field_access(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
          return check_binary(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::UnaryExpr>) {
          return check_unary(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::IfExpr>) {
          return check_if(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::MatchExpr>) {
          return check_match(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::StructInit>) {
          return check_struct_init(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::ScopeAccess>) {
          return check_scope_access(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::BuiltinCall>) {
          return check_builtin(e, e.span);

        } else if constexpr (std::is_same_v<T, ast::Assignment>) {
          auto target = check_expr(*e.target);
          auto value = check_expr(*e.value);
          unify(target.ty, value.ty, e.span);

          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_unit(e.span);
          result->value = typed::Assignment{
              e.span, make_unique<typed::TypedExpr>(std::move(target)),
              make_unique<typed::TypedExpr>(std::move(value))};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::Break>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = fresh_etvar("break");
          if (e.value) {
            auto val = check_expr(**e.value);
            result->value = typed::Break{
                e.span, e.label, make_unique<typed::TypedExpr>(std::move(val))};
          } else {
            result->value = typed::Break{e.span, e.label, nullopt};
          }
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::Continue>) {
          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = make_unit(e.span);
          result->value = typed::Continue{e.span, e.label};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::RangeExpr>) {
          auto start = check_expr(*e.start);
          auto end = check_expr(*e.end);
          unify(start.ty, end.ty, e.span);

          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          // Range type: Range[T] where T is the element type
          auto elem_ty = apply_solutions(start.ty);
          auto range_ty = make_shared<Type>();
          range_ty->ty = TyNamed{"Range", Struct, {elem_ty}};
          range_ty->span = e.span;
          result->ty = range_ty;
          result->value = typed::RangeExpr{
              e.span, make_unique<typed::TypedExpr>(std::move(start)),
              make_unique<typed::TypedExpr>(std::move(end))};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::IndexAccess>) {
          auto obj = check_expr(*e.object);
          auto idx = check_expr(*e.index);
          unify(idx.ty, make_primitive("usize", e.span), e.span);

          auto elem_ty = fresh_etvar("elem");
          auto arr_ty = make_shared<Type>();
          arr_ty->ty = TyArray{elem_ty, 0};
          arr_ty->span = e.span;
          unify(obj.ty, arr_ty, e.span);

          auto result = make_shared<typed::TypedExpr>();
          result->span = e.span;
          result->ty = apply_solutions(elem_ty);
          result->value = typed::IndexAccess{
              e.span, make_unique<typed::TypedExpr>(std::move(obj)),
              make_unique<typed::TypedExpr>(std::move(idx))};
          return std::move(*result);

        } else if constexpr (std::is_same_v<T, ast::GenericIdent>) {
          throw TypeError("generic instantiation not implemented", e.span);

        } else if constexpr (std::is_same_v<T, ast::TypeInit>) {
          throw TypeError("type init not implemented", e.span);

        } else if constexpr (std::is_same_v<T, ast::ComptimeExpr>) {
          throw TypeError("comptime expressions not implemented", e.span);
        }

        throw TypeError("unhandled expression form", expr.span);
      },
      expr.value);
}

typed::TypedProgram Typechecker::check(const ast::Program &program) {
  typed::TypedProgram result;

  // Check all declarations (collect already ran in run())
  for (auto &declaration : program.declarations) {
    std::visit(overload{
                   [&](const ast::FnDecl &fn) {
                     result.declarations.push_back(
                         typed::TypedDecl{fn.span, check_fn(fn)});
                   },
                   [&](const ast::StructDecl &) {
                     // Structs processed in collect, methods checked separately
                   },
                   [&](const ast::UnionDecl &) {
                     // Unions processed in collect, methods checked separately
                   },
                   [&](const ast::InterfaceDecl &) {},
                   [&](const ast::ExternDecl &) {},
                   [&](const ast::UseDecl &) {},
                   [&](const ast::MacroDecl &) {
                     // Macros not typechecked yet
                   },
                   [&](const ast::ComptimeStmt &) {
                     // Comptime not implemented yet
                   },
               },
               declaration.value);
  }

  return result;
}

} // namespace shikimori
