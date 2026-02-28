#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "../span.h"
#include "../types.h"
#include "ast.h"

using namespace std;

namespace shikimori::typed {

using namespace shikimori;
using namespace std;

using Identifier = string;

struct TypedExpr;
struct TypedStmt;
struct TypedBlock;

struct IntLiteral {
  Span span;
  int64_t value;
};
struct FloatLiteral {
  Span span;
  double value;
};
struct BoolLiteral {
  Span span;
  bool value;
};
struct StringLiteral {
  Span span;
  string value;
};
struct NullLiteral {
  Span span;
};

struct IdentifierExpr {
  Span span;
  Identifier name;
  TypeRef ty;
};

struct StructInit {
  Span span;
  Identifier name;
  vector<pair<Identifier, unique_ptr<TypedExpr>>> fields;
};

struct ScopeAccess {
  Span span;
  Identifier scope;
  Identifier member;
  vector<unique_ptr<TypedExpr>> payload;
};

struct UnionVariantInit {
  Span span;
  Identifier union_name;
  Identifier variant;
  vector<unique_ptr<TypedExpr>> payload;
};

struct FieldAccess {
  Span span;
  unique_ptr<TypedExpr> object;
  Identifier field;
};

struct MethodCall {
  Span span;
  unique_ptr<TypedExpr> object;
  Identifier method;
  vector<unique_ptr<TypedExpr>> args;
};

struct IndexAccess {
  Span span;
  unique_ptr<TypedExpr> object;
  unique_ptr<TypedExpr> index;
};

struct Call {
  Span span;
  unique_ptr<TypedExpr> callee;
  vector<unique_ptr<TypedExpr>> args;
};

struct UnaryExpr {
  Span span;
  ast::UnaryOp op;
  unique_ptr<TypedExpr> operand;
};

struct BinaryExpr {
  Span span;
  ast::BinaryOp op;
  unique_ptr<TypedExpr> left;
  unique_ptr<TypedExpr> right;
};

struct Assignment {
  Span span;
  unique_ptr<TypedExpr> target;
  unique_ptr<TypedExpr> value;
};

struct Break {
  Span span;
  optional<Identifier> label;
  optional<unique_ptr<TypedExpr>> value;
};

struct Continue {
  Span span;
  optional<Identifier> label;
};

struct BuiltinCall {
  Span span;
  Identifier name;
  vector<TypeRef> type_args;
  vector<unique_ptr<TypedExpr>> args;
};

struct RangeExpr {
  Span span;
  unique_ptr<TypedExpr> start;
  unique_ptr<TypedExpr> end;
};

struct TypedBlock {
  Span span;
  vector<unique_ptr<TypedStmt>> stmts;
  TypeRef ty; // type of last expr, or unit
};

struct IfBranch {
  Span span;
  unique_ptr<TypedExpr> condition;
  TypedBlock body;
};

struct IfExpr {
  Span span;
  vector<IfBranch> branches;
  optional<TypedBlock> else_branch;
};

struct TypedPattern {
  Span span;
  TypeRef ty; // type of what's being matched
  variant<ast::VariantPattern, ast::LiteralPattern, ast::WildcardPattern,
          ast::IdentPattern>
      value;
};

struct MatchArm {
  Span span;
  TypedPattern pattern;
  TypedBlock body;
};

struct MatchExpr {
  Span span;
  unique_ptr<TypedExpr> subject;
  vector<MatchArm> arms;
};

struct TypeInit {
  Span span;
  TypeRef ty; // resolved [8]i32 etc
  vector<unique_ptr<TypedExpr>> fields;
};

struct ComptimeExpr {
  Span span;
  unique_ptr<TypedExpr> expr;
};

struct TypedExpr {
  Span span;
  TypeRef ty;
  variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, NullLiteral,
          IdentifierExpr, StructInit, UnionVariantInit, ScopeAccess,
          FieldAccess, MethodCall, IndexAccess, Call, UnaryExpr, BinaryExpr,
          Assignment, IfExpr, MatchExpr, Break, Continue, BuiltinCall,
          RangeExpr, TypeInit>
      value;
};

struct LetStmt {
  Span span;
  Identifier name;
  TypeRef ty; // resolved, no more optional TypeAnnot
  unique_ptr<TypedExpr> init;
};

struct ReturnStmt {
  Span span;
  optional<unique_ptr<TypedExpr>> value;
};

struct DeferStmt {
  Span span;
  unique_ptr<TypedStmt> stmt;
};

struct LoopStmt {
  Span span;
  optional<Identifier> label;
  TypedBlock body;
};

struct WhileStmt {
  Span span;
  optional<Identifier> label;
  unique_ptr<TypedExpr> condition;
  TypedBlock body;
};

struct ForStmt {
  Span span;
  optional<Identifier> label;
  Identifier var;
  TypeRef var_ty; // type of the loop variable
  unique_ptr<TypedExpr> iterable;
  TypedBlock body;
};

struct TypedStmt {
  Span span;
  variant<LetStmt, ReturnStmt, DeferStmt, LoopStmt, WhileStmt, ForStmt,
          unique_ptr<TypedExpr> // ExprStmt
          >
      value;
};

struct TypedParam {
  Span span;
  Identifier name;
  TypeRef ty; // resolved
};

struct TypedFnDecl {
  Span span;
  Identifier name;
  TypeRef ty; // the full FnTy or ForAll. resolved scheme
  vector<TypedParam> params;
  TypeRef return_type;
  TypedBlock body;
};

struct TypedFieldDecl {
  Span span;
  Identifier name;
  TypeRef ty;
};

struct TypedStructDecl {
  Span span;
  Identifier name;
  TypeRef ty; // the ForAll scheme or TyNamed if not generic
  vector<TypedFieldDecl> fields;
  vector<TypedFnDecl> methods;
};

struct TypedUnionVariant {
  Span span;
  Identifier name;
  optional<TypeRef> ty;
};

struct TypedUnionDecl {
  Span span;
  Identifier name;
  TypeRef ty;
  vector<TypedUnionVariant> variants;
  vector<TypedFnDecl> methods;
};

// InterfaceDecl doesn't change much, methods just get resolved types
struct TypedInterfaceMethod {
  Span span;
  Identifier name;
  vector<TypedParam> params;
  TypeRef return_type;
};

struct TypedInterfaceDecl {
  Span span;
  Identifier name;
  vector<TypedInterfaceMethod> methods;
};

struct TypedExternDecl {
  Span span;
  Identifier name;
  vector<TypeRef> params;
  TypeRef return_type;
};

using ComptimeInner = variant<LetStmt,
                              unique_ptr<TypedExpr>, // IfExpr as stmt
                              ForStmt>;

struct ComptimeStmt {
  Span span;
  ComptimeInner stmt;
};

struct TypedDecl {
  Span span;
  variant<TypedFnDecl, TypedStructDecl, TypedUnionDecl, TypedInterfaceDecl,
          TypedExternDecl>
      value;
};

struct TypedProgram {
  vector<TypedDecl> declarations;
};

} // namespace shikimori::typed
