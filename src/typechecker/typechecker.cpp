#include "typechecker.h"
#include "ast/ast.h"
#include "ast/typedast.h"

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
}

void Typechecker::collect_union(const ast::UnionDecl &decl) {
  UnionDef def;
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

TypeRef Typechecker::resolve_type(const ast::TypeAnnot &) { return nullptr; }

void Typechecker::resolve_use(const ast::UseDecl &use, string file_path) {
  import_resolver.resolve_use(use, file_path);
}

} // namespace shikimori
