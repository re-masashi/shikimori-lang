#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

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
  std::unique_ptr<TypeAnnot> inner;
};

struct SliceTypeAnnot {
  std::unique_ptr<TypeAnnot> inner;
};

struct ArrayTypeAnnot {
  std::unique_ptr<Expr> size;
  std::unique_ptr<TypeAnnot> inner;
};

struct OptionalTypeAnnot {
  std::unique_ptr<TypeAnnot> inner;
};

struct InterfaceTypeAnnot {
  std::vector<Identifier> names;
};

struct NamedTypeAnnot {
  Identifier name;
  std::vector<std::unique_ptr<TypeAnnot>> generic_args;
};

struct TypeAnnot {
  std::variant<PrimitiveType, PointerTypeAnnot, SliceTypeAnnot, ArrayTypeAnnot,
               OptionalTypeAnnot, InterfaceTypeAnnot, NamedTypeAnnot>
      value;
};

struct IntLiteral {
  int64_t value;
};
struct FloatLiteral {
  double value;
};
struct BoolLiteral {
  bool value;
};
struct StringLiteral {
  std::string value;
};
struct NullLiteral {};

struct VariantPattern {
  Identifier variant;
  std::optional<Identifier> binding;
};

struct LiteralPattern {
  std::variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral,
               NullLiteral>
      value;
};

struct WildcardPattern {};

struct IdentPattern {
  Identifier name;
};

struct Pattern {
  std::variant<VariantPattern, LiteralPattern, WildcardPattern, IdentPattern>
      value;
};

struct Block {
  std::vector<std::unique_ptr<Stmt>> statements;
};

struct IdentifierExpr {
  Identifier name;
};

struct GenericIdent {
  Identifier name;
  std::vector<std::unique_ptr<TypeAnnot>> type_args;
};

struct StructInit {
  Identifier name;
  std::optional<std::vector<std::unique_ptr<TypeAnnot>>> generic_args;
  std::vector<std::pair<Identifier, std::unique_ptr<Expr>>> fields;
};

struct UnionInit {
  Identifier union_name;
  Identifier variant_name;
  std::optional<std::unique_ptr<Expr>> payload;
};

struct FieldAccess {
  std::unique_ptr<Expr> object;
  Identifier field;
};

struct MethodCall {
  std::unique_ptr<Expr> object;
  Identifier method;
  std::vector<std::unique_ptr<Expr>> args;
};

struct IndexAccess {
  std::unique_ptr<Expr> object;
  std::unique_ptr<Expr> index;
};

struct Call {
  std::unique_ptr<Expr> callee;
  std::vector<std::unique_ptr<Expr>> args;
};

struct UnaryExpr {
  UnaryOp op;
  std::unique_ptr<Expr> operand;
};

struct BinaryExpr {
  BinaryOp op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
};

struct Assignment {
  std::unique_ptr<Expr> target;
  std::unique_ptr<Expr> value;
};

struct Break {
  std::optional<Identifier> label;
  std::optional<std::unique_ptr<Expr>> value;
};

struct Continue {
  std::optional<Identifier> label;
};

struct ComptimeExpr {
  std::unique_ptr<Expr> expr;
};

struct BuiltinCall {
  Identifier name;
  std::vector<std::unique_ptr<Expr>> args;
};

// if as expression
// typechecker enforces: if used as value → all branches same type + else
// required if used as stmt (value discarded) → else optional, branches can be
// void
struct IfBranch {
  std::unique_ptr<Expr> condition;
  Block body;
};

struct IfExpr {
  std::vector<IfBranch> branches;
  std::optional<Block> else_branch;
};

// match as expression
// typechecker enforces: if used as value → all arms same type
struct MatchArm {
  Pattern pattern;
  Block body; // block's last expr is the arm's value (if used as expression)
};

struct MatchExpr {
  std::unique_ptr<Expr> subject;
  std::vector<MatchArm> arms;
};

struct Expr {
  std::variant<IntLiteral, FloatLiteral, BoolLiteral, StringLiteral,
               NullLiteral, IdentifierExpr, GenericIdent, StructInit, UnionInit,
               FieldAccess, MethodCall, IndexAccess, Call, UnaryExpr,
               BinaryExpr, Assignment, IfExpr, MatchExpr, Break, Continue,
               ComptimeExpr, BuiltinCall>
      value;
};

struct LetStmt {
  Identifier name;
  std::optional<std::unique_ptr<TypeAnnot>> type;
  std::unique_ptr<Expr> init;
};

struct ReturnStmt {
  std::optional<std::unique_ptr<Expr>> value;
};

struct DeferStmt {
  std::unique_ptr<Stmt> stmt;
};

struct ExprStmt {
  std::unique_ptr<Expr> expr;
};

struct LoopStmt {
  std::optional<Identifier> label;
  Block body;
};

struct WhileStmt {
  std::optional<Identifier> label;
  std::unique_ptr<Expr> condition;
  Block body;
};

struct ForStmt {
  std::optional<Identifier> label;
  Identifier var;
  std::unique_ptr<Expr> iterable;
  Block body;
};

struct ComptimeStmt {
  std::unique_ptr<Stmt>
      stmt; // only LetStmt, IfExpr as stmt, ForStmt valid here
};

struct Stmt {
  std::variant<LetStmt, ReturnStmt, DeferStmt, LoopStmt, WhileStmt, ForStmt,
               ComptimeStmt,
               ExprStmt // if, match, assignments, calls. all just ExprStmt
               >
      value;
};

struct Param {
  Identifier name;
  std::unique_ptr<TypeAnnot> type;
};

struct GenericParam {
  Identifier name;
};

struct WhereConstraint {
  Identifier name;
  InterfaceTypeAnnot iface;
};

struct FnDecl {
  Identifier name;
  std::vector<GenericParam> generic_params;
  std::vector<Param> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
  std::optional<std::vector<WhereConstraint>> where_clause;
  Block body;
};

struct FieldDecl {
  Identifier name;
  std::unique_ptr<TypeAnnot> type;
};

struct StructDecl {
  Identifier name;
  std::vector<GenericParam> generic_params;
  std::vector<FieldDecl> fields;
  std::vector<FnDecl> methods;
};

struct UnionVariant {
  Identifier name;
  std::optional<std::unique_ptr<TypeAnnot>> type;
};

struct UnionDecl {
  Identifier name;
  std::vector<GenericParam> generic_params;
  std::vector<UnionVariant> variants;
  std::vector<FnDecl> methods;
};

struct InterfaceMethod {
  Identifier name;
  std::vector<Param> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
};

struct InterfaceDecl {
  Identifier name;
  std::vector<InterfaceMethod> methods;
};

struct ExternParam {
  std::unique_ptr<TypeAnnot> type;
};

struct ExternDecl {
  Identifier name;
  std::vector<ExternParam> params;
  std::optional<std::unique_ptr<TypeAnnot>> return_type;
};

struct ImportItem {
  Identifier name;
  std::optional<Identifier> alias;
};

struct UseDecl {
  std::string path;
  std::vector<ImportItem> items;
};

enum class MacroKind { Expr, Stmt, Type, Ident };

struct MacroParam {
  Identifier name;
  MacroKind kind;
};

struct MacroDecl {
  Identifier name;
  std::vector<MacroParam> params;
  Block body;
};

struct Decl {
  std::variant<FnDecl, StructDecl, UnionDecl, InterfaceDecl, ExternDecl,
               UseDecl, MacroDecl>
      value;
};

struct Program {
  std::vector<Decl> declarations;
};

} // namespace shikimori::ast
