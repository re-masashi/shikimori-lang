#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "ast/ast_types.hpp"
#include "span.h"

using namespace std;

namespace shikimori::ast {

// Forward declarations
struct Stmt;
struct Block;

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

// Literals
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

// Patterns
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

// Block - defined here because it's used by expressions (IfExpr, MatchExpr)
struct Block {
  Span span;
  vector<unique_ptr<Stmt>> statements;
};

// Expressions
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
  optional<vector<unique_ptr<TypeAnnot>>> generic_args;
  vector<pair<Identifier, unique_ptr<Expr>>> fields;
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
  vector<unique_ptr<TypeAnnot>> type_args;
  vector<unique_ptr<Expr>> args;
};

struct RangeExpr {
  Span span;
  unique_ptr<Expr> start;
  unique_ptr<Expr> end;
  bool inclusive;
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

struct MatchArm {
  Span span;
  Pattern pattern;
  Block body;
};

struct MatchExpr {
  Span span;
  unique_ptr<Expr> subject;
  vector<MatchArm> arms;
};

struct TypeInit {
  Span span;
  unique_ptr<TypeAnnot> type;
  vector<pair<Identifier, unique_ptr<Expr>>> fields;
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

} // namespace shikimori::ast
