#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "span.h"

namespace shikimori::ast {

using Identifier = std::string;

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
  std::unique_ptr<TypeAnnot> inner;
};

struct SliceTypeAnnot {
  Span span;
  std::unique_ptr<TypeAnnot> inner;
};

struct ArrayTypeAnnot {
  Span span;
  std::unique_ptr<Expr> size;
  std::unique_ptr<TypeAnnot> inner;
};

struct OptionalTypeAnnot {
  Span span;
  std::unique_ptr<TypeAnnot> inner;
};

struct InterfaceTypeAnnot {
  Span span;
  std::vector<Identifier> names;
};

struct NamedTypeAnnot {
  Span span;
  Identifier name;
  std::vector<std::unique_ptr<TypeAnnot>> generic_args;
};

struct TypeAnnot {
  Span span;
  std::variant<PrimitiveType, PointerTypeAnnot, SliceTypeAnnot, ArrayTypeAnnot,
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
  std::string value;
};
struct NullLiteral {
  Span span;
};

struct VariantPattern {
  Span span;
  Identifier variant;
  std::optional<Identifier> binding;
};

struct LiteralPattern {
  Span span;
  std::variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral,
               NullLiteral>
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
  std::variant<VariantPattern, LiteralPattern, WildcardPattern, IdentPattern>
      value;
};

struct Block {
  Span span;
  std::vector<std::unique_ptr<Stmt>> statements;
};

struct IdentifierExpr {
  Span span;
  Identifier name;
};

struct GenericIdent {
  Span span;
  Identifier name;
  std::vector<std::unique_ptr<TypeAnnot>> type_args;
};

struct StructInit {
  Span span;
  Identifier name;
  std::optional<std::vector<std::unique_ptr<TypeAnnot>>> generic_args;
  std::vector<std::pair<Identifier, std::unique_ptr<Expr>>> fields;
};

struct ScopeAccess {
  Span span;
  Identifier scope;
  Identifier member;
  std::vector<std::unique_ptr<TypeAnnot>> generic_args;
  std::vector<std::unique_ptr<Expr>> payload;
};

struct FieldAccess {
  Span span;
  std::unique_ptr<Expr> object;
  Identifier field;
};

struct MethodCall {
  Span span;
  std::unique_ptr<Expr> object;
  Identifier method;
  std::vector<std::unique_ptr<Expr>> args;
};

struct IndexAccess {
  Span span;
  std::unique_ptr<Expr> object;
  std::unique_ptr<Expr> index;
};

struct Call {
  Span span;
  std::unique_ptr<Expr> callee;
  std::vector<std::unique_ptr<Expr>> args;
};

struct UnaryExpr {
  Span span;
  UnaryOp op;
  std::unique_ptr<Expr> operand;
};

struct BinaryExpr {
  Span span;
  BinaryOp op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
};

struct Assignment {
  Span span;
  std::unique_ptr<Expr> target;
  std::unique_ptr<Expr> value;
};

struct Break {
  Span span;
  std::optional<Identifier> label;
  std::optional<std::unique_ptr<Expr>> value;
};

struct Continue {
  Span span;
  std::optional<Identifier> label;
};

struct ComptimeExpr {
  Span span;
  std::unique_ptr<Expr> expr;
};

struct BuiltinCall {
  Span span;
  Identifier name;
  std::vector<std::unique_ptr<Expr>> args;
};

struct RangeExpr {
  Span span;
  std::unique_ptr<Expr> start;
  std::unique_ptr<Expr> end;
  bool inclusive; // true for ..=, false for ..
};

struct IfBranch {
  Span span;
  std::unique_ptr<Expr> condition;
  Block body;
};

struct IfExpr {
  Span span;
  std::vector<IfBranch> branches;
  std::optional<Block> else_branch;
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
  std::unique_ptr<Expr> subject;
  std::vector<MatchArm> arms;
};

struct TypeInit {
  Span span;
  std::unique_ptr<TypeAnnot> type;
  std::vector<std::pair<Identifier, std::unique_ptr<Expr>>> fields;
};

struct Expr {
  Span span;
  std::variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral,
               NullLiteral, IdentifierExpr, GenericIdent, StructInit,
               ScopeAccess, FieldAccess, MethodCall, IndexAccess, Call,
               UnaryExpr, BinaryExpr, Assignment, IfExpr, MatchExpr, Break,
               Continue, ComptimeExpr, BuiltinCall, RangeExpr, TypeInit>
      value;
};

struct LetStmt {
  Span span;
  Identifier name;
  std::optional<std::unique_ptr<TypeAnnot>> type;
  std::unique_ptr<Expr> init;
};

struct ReturnStmt {
  Span span;
  std::optional<std::unique_ptr<Expr>> value;
};

struct DeferStmt {
  Span span;
  std::unique_ptr<Stmt> stmt;
};

struct ExprStmt {
  Span span;
  std::unique_ptr<Expr> expr;
};

struct LoopStmt {
  Span span;
  std::optional<Identifier> label;
  Block body;
};

struct WhileStmt {
  Span span;
  std::optional<Identifier> label;
  std::unique_ptr<Expr> condition;
  Block body;
};

struct ForStmt {
  Span span;
  std::optional<Identifier> label;
  Identifier var;
  std::unique_ptr<Expr> iterable;
  Block body;
};

struct ComptimeStmt {
  Span span;
  std::unique_ptr<Stmt>
      stmt; // only LetStmt, IfExpr as stmt, ForStmt valid here
};

struct Stmt {
  Span span;
  std::variant<LetStmt, ReturnStmt, DeferStmt, LoopStmt, WhileStmt, ForStmt,
               ComptimeStmt,
               ExprStmt // if, match, assignments, calls. all just ExprStmt
               >
      value;
};

struct Param {
  Span span;
  Identifier name;
  std::unique_ptr<TypeAnnot> type;
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
  std::vector<GenericParam> generic_params;
  std::vector<Param> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
  std::optional<std::vector<WhereConstraint>> where_clause;
  Block body;
};

struct FieldDecl {
  Span span;
  Identifier name;
  std::unique_ptr<TypeAnnot> type;
};

struct StructDecl {
  Span span;
  Identifier name;
  std::vector<GenericParam> generic_params;
  std::vector<FieldDecl> fields;
  std::vector<FnDecl> methods;
};

struct UnionVariant {
  Span span;
  Identifier name;
  std::optional<std::unique_ptr<TypeAnnot>> type;
};

struct UnionDecl {
  Span span;
  Identifier name;
  std::vector<GenericParam> generic_params;
  std::vector<UnionVariant> variants;
  std::vector<FnDecl> methods;
};

struct InterfaceMethod {
  Span span;
  Identifier name;
  std::vector<Param> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
};

struct InterfaceDecl {
  Span span;
  Identifier name;
  std::vector<InterfaceMethod> methods;
};

struct ExternParam {
  Span span;
  std::unique_ptr<TypeAnnot> type;
};

struct ExternDecl {
  Span span;
  Identifier name;
  std::vector<ExternParam> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
};

struct ImportItem {
  Span span;
  Identifier name;
  std::optional<Identifier> alias;
};

struct UseDecl {
  Span span;
  std::string path;
  std::vector<ImportItem> items;
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
  std::vector<MacroParam> params;
  Block body;
};

struct Decl {
  Span span;
  std::variant<FnDecl, StructDecl, UnionDecl, InterfaceDecl, ExternDecl,
               UseDecl, MacroDecl, ComptimeStmt>
      value;
};

struct Program {
  Span span;
  std::vector<Decl> declarations;
};

} // namespace shikimori::ast
