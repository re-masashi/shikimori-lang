#pragma once

#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "ast/ast_expr.hpp"
#include "span.h"

using namespace std;

namespace shikimori::ast {

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
  unique_ptr<Stmt> stmt;
};

struct Stmt {
  Span span;
  variant<LetStmt, ReturnStmt, DeferStmt, LoopStmt, WhileStmt, ForStmt,
          ComptimeStmt, ExprStmt>
      value;
};

} // namespace shikimori::ast
