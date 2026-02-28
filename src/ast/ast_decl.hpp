#pragma once

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ast/ast_stmt.hpp"
#include "ast/ast_types.hpp"
#include "span.h"

using namespace std;

namespace shikimori::ast {

struct Param {
  Span span;
  Identifier name;
  unique_ptr<TypeAnnot> type;
};

struct GenericParam {
  Span span;
  Identifier name;
};

struct WhereConstraint {
  Span span;
  Identifier name;
  InterfaceTypeAnnot iface;
};

struct FnDecl {
  Span span;
  Identifier name;
  vector<GenericParam> generic_params;
  vector<Param> params;
  optional<unique_ptr<TypeAnnot>> return_type;
  optional<vector<WhereConstraint>> where_clause;
  Block body;
};

struct FieldDecl {
  Span span;
  Identifier name;
  unique_ptr<TypeAnnot> type;
};

struct StructDecl {
  Span span;
  Identifier name;
  vector<GenericParam> generic_params;
  vector<FieldDecl> fields;
  vector<FnDecl> methods;
};

struct UnionVariant {
  Span span;
  Identifier name;
  optional<unique_ptr<TypeAnnot>> type;
};

struct UnionDecl {
  Span span;
  Identifier name;
  vector<GenericParam> generic_params;
  vector<UnionVariant> variants;
  vector<FnDecl> methods;
};

struct InterfaceMethod {
  Span span;
  Identifier name;
  vector<Param> params;
  optional<unique_ptr<TypeAnnot>> return_type;
};

struct InterfaceDecl {
  Span span;
  Identifier name;
  vector<InterfaceMethod> methods;
};

struct ExternParam {
  Span span;
  unique_ptr<TypeAnnot> type;
};

struct ExternDecl {
  Span span;
  Identifier name;
  vector<ExternParam> params;
  optional<unique_ptr<TypeAnnot>> return_type;
};

struct ImportItem {
  Span span;
  Identifier name;
  optional<Identifier> alias;
};

struct UseDecl {
  Span span;
  string path;
  vector<ImportItem> items;
};

enum class MacroKind { Expr, Stmt, Type, Ident };

struct MacroParam {
  Span span;
  Identifier name;
  MacroKind kind;
};

struct MacroDecl {
  Span span;
  Identifier name;
  vector<MacroParam> params;
  Block body;
};

struct Decl {
  Span span;
  variant<FnDecl, StructDecl, UnionDecl, InterfaceDecl, ExternDecl, UseDecl,
          MacroDecl, ComptimeStmt>
      value;
};

struct Program {
  Span span;
  vector<Decl> declarations;
};

} // namespace shikimori::ast
