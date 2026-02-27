#include "typechecker.h"
#include "ast/ast.h"
#include "ast/typedast.h"
#include <variant>

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

void Typechecker::collect_fn(const ast::FnDecl &decl) {
  map<string, uint32_t> saved_vars;
  if (!decl.generic_params.empty()) {
    for (size_t i = 0; i < decl.generic_params.size(); i++) {
      current_type_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
      saved_vars[decl.generic_params[i].name] = static_cast<uint32_t>(i);
    }
  }

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

  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
  }

  auto fn_ty = make_shared<Type>();
  fn_ty->ty = FnTy{arg_types, ret_type};
  fn_ty->span = decl.span;

  if (decl.generic_params.empty()) {
    functions[decl.name] = fn_ty;
    return;
  }

  vector<pair<string, uint32_t>> vars;
  for (size_t i = 0; i < decl.generic_params.size(); i++) {
    vars.push_back({decl.generic_params[i].name, static_cast<uint32_t>(i)});
  }

  auto forall = make_shared<Type>();
  forall->ty = ForAll{vars, fn_ty};
  forall->span = decl.span;
  functions[decl.name] = forall;
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

  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
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

  for (auto &kv : saved_vars) {
    current_type_vars.erase(kv.first);
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
}

void Typechecker::collect_interface(const ast::InterfaceDecl &decl) {
  TyInterface iface;
  iface.name = decl.name;

  for (auto &method : decl.methods) {
    vector<TypeRef> arg_types;
    for (auto &param : method.params) {
      arg_types.push_back(resolve_type(*param.type));
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
  functions[decl.name] = fn_ty;
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
          ty->ty = TyNamed{"slice", Slice, {inner}};
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
          auto ty = make_shared<Type>();
          ty->span = annot.span;
          ty->ty = InterfaceConstraint{t.names};
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

} // namespace shikimori
