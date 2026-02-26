#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "span.h"

using namespace std;

namespace shikimori::ast {

using Identifier = string;

struct TypeAnnot;
struct Expr;
struct Stmt;
struct Decl;

enum class UnaryOp {
  Not,
  Neg,
  Deref,
  AddrOf,
  BitNot,
};

enum class BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  LShift,
  RShift,
  Lt,
  Gt,
  Lte,
  Gte,
  Eq,
  Neq,
  BitAnd,
  BitXor,
  BitOr,
  And,
  Or,
};

enum class PrimitiveType {
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  Bool,
  Usize,
  String,
};

struct PointerTypeAnnot {
  Span span;
  unique_ptr<TypeAnnot> inner;
};

struct SliceTypeAnnot {
  Span span;
  unique_ptr<TypeAnnot> inner;
};

struct ArrayTypeAnnot {
  Span span;
  unique_ptr<Expr> size;
  unique_ptr<TypeAnnot> inner;
};

struct OptionalTypeAnnot {
  Span span;
  unique_ptr<TypeAnnot> inner;
};

struct InterfaceTypeAnnot {
  Span span;
  vector<Identifier> names;
};

struct NamedTypeAnnot {
  Span span;
  Identifier name;
  vector<unique_ptr<TypeAnnot>> generic_args;
};

struct TypeAnnot {
  Span span;
  variant<PrimitiveType, PointerTypeAnnot, SliceTypeAnnot, ArrayTypeAnnot,
          OptionalTypeAnnot, InterfaceTypeAnnot, NamedTypeAnnot>
      value;
};

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

struct VariantPattern {
  Span span;
  Identifier variant;
  optional<Identifier> binding;
};

struct LiteralPattern {
  Span span;
  variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, NullLiteral>
      value;
};

struct WildcardPattern {
  Span span;
};

struct IdentPattern {
  Span span;
  Identifier name;
};

struct Pattern {
  Span span;
  variant<VariantPattern, LiteralPattern, WildcardPattern, IdentPattern> value;
};

struct Block {
  Span span;
  vector<unique_ptr<Stmt>> statements;
};

struct IdentifierExpr {
  Span span;
  Identifier name;
};

struct GenericIdent {
  Span span;
  Identifier name;
  vector<unique_ptr<TypeAnnot>> type_args;
};

struct StructInit {
  Span span;
  Identifier name;
  optional<vector<std::unique_ptr<TypeAnnot>>> generic_args;
  vector<pair<Identifier, std::unique_ptr<Expr>>> fields;
};

struct ScopeAccess {
  Span span;
  Identifier scope;
  Identifier member;
  vector<unique_ptr<TypeAnnot>> generic_args;
  vector<unique_ptr<Expr>> payload;
};

struct FieldAccess {
  Span span;
  unique_ptr<Expr> object;
  Identifier field;
};

struct MethodCall {
  Span span;
  unique_ptr<Expr> object;
  Identifier method;
  vector<unique_ptr<Expr>> args;
};

struct IndexAccess {
  Span span;
  unique_ptr<Expr> object;
  unique_ptr<Expr> index;
};

struct Call {
  Span span;
  unique_ptr<Expr> callee;
  vector<unique_ptr<Expr>> args;
};

struct UnaryExpr {
  Span span;
  UnaryOp op;
  unique_ptr<Expr> operand;
};

struct BinaryExpr {
  Span span;
  BinaryOp op;
  unique_ptr<Expr> left;
  unique_ptr<Expr> right;
};

struct Assignment {
  Span span;
  unique_ptr<Expr> target;
  unique_ptr<Expr> value;
};

struct Break {
  Span span;
  optional<Identifier> label;
  optional<unique_ptr<Expr>> value;
};

struct Continue {
  Span span;
  optional<Identifier> label;
};

struct ComptimeExpr {
  Span span;
  unique_ptr<Expr> expr;
};

struct BuiltinCall {
  Span span;
  Identifier name;
  vector<unique_ptr<Expr>> args;
};

struct RangeExpr {
  Span span;
  unique_ptr<Expr> start;
  unique_ptr<Expr> end;
  bool inclusive; // true for ..=, false for ..
};

struct IfBranch {
  Span span;
  unique_ptr<Expr> condition;
  Block body;
};

struct IfExpr {
  Span span;
  vector<IfBranch> branches;
  optional<Block> else_branch;
};

// match as expression
// typechecker enforces: if used as value -> all arms same type
struct MatchArm {
  Span span;
  Pattern pattern;
  Block body; // block's last expr is the arm's value (if used as expression)
};

struct MatchExpr {
  Span span;
  unique_ptr<Expr> subject;
  vector<MatchArm> arms;
};

struct TypeInit {
  Span span;
  unique_ptr<TypeAnnot> type;
  vector<pair<Identifier, std::unique_ptr<Expr>>> fields;
};

struct Expr {
  Span span;
  variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, NullLiteral,
          IdentifierExpr, GenericIdent, StructInit, ScopeAccess, FieldAccess,
          MethodCall, IndexAccess, Call, UnaryExpr, BinaryExpr, Assignment,
          IfExpr, MatchExpr, Break, Continue, ComptimeExpr, BuiltinCall,
          RangeExpr, TypeInit>
      value;
};

struct LetStmt {
  Span span;
  Identifier name;
  optional<unique_ptr<TypeAnnot>> type;
  unique_ptr<Expr> init;
};

struct ReturnStmt {
  Span span;
  optional<unique_ptr<Expr>> value;
};

struct DeferStmt {
  Span span;
  unique_ptr<Stmt> stmt;
};

struct ExprStmt {
  Span span;
  unique_ptr<Expr> expr;
};

struct LoopStmt {
  Span span;
  optional<Identifier> label;
  Block body;
};

struct WhileStmt {
  Span span;
  optional<Identifier> label;
  unique_ptr<Expr> condition;
  Block body;
};

struct ForStmt {
  Span span;
  optional<Identifier> label;
  Identifier var;
  unique_ptr<Expr> iterable;
  Block body;
};

struct ComptimeStmt {
  Span span;
  unique_ptr<Stmt> stmt; // only LetStmt, IfExpr as stmt, ForStmt valid here
};

struct Stmt {
  Span span;
  variant<LetStmt, ReturnStmt, DeferStmt, LoopStmt, WhileStmt, ForStmt,
          ComptimeStmt,
          ExprStmt // if, match, assignments, calls. all just ExprStmt
          >
      value;
};

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
