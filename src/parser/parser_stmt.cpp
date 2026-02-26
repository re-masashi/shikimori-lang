#include "parser.h"
#include <utility>

using namespace std;

namespace shikimori {

optional<ast::Block> Parser::parse_block() {
  size_t start = current_pos;
  consume(TokenType::LBRACE, "'{'");
  if (has_errors())
    return nullopt;

  ast::Block block;
  while (!check(TokenType::RBRACE) && !is_at_end()) {
    auto stmt = parse_statement();
    if (!stmt) {
      if (has_errors())
        return nullopt;
      report_unexpected_token();
      advance();
      continue;
    }
    block.statements.push_back(make_unique<ast::Stmt>(std::move(stmt->value)));
  }

  consume(TokenType::RBRACE, "'}'");
  if (has_errors())
    return nullopt;
  block.span = get_span_for(start);
  return block;
}

optional<ast::Identifier> Parser::parse_label() {
  // label ::= ":" IDENT
  if (!check(TokenType::COLON))
    return nullopt;
  // lookahead: colon followed by IDENT, then loop/while/for keyword
  if (peek().type != TokenType::IDENT)
    return nullopt;
  auto after_ident = peek(2).type;
  if (after_ident != TokenType::KW_LOOP && after_ident != TokenType::KW_WHILE &&
      after_ident != TokenType::KW_FOR) {
    return nullopt;
  }
  advance(); // consume ':'
  auto name_tok = consume(TokenType::IDENT, "label name");
  if (has_errors())
    return nullopt;
  return name_tok.lexeme;
}

optional<Spanned<ast::Stmt>> Parser::parse_statement() {
  // Check for labeled loops first
  if (check(TokenType::COLON) && peek().type == TokenType::IDENT) {
    auto after = peek(2).type;
    if (after == TokenType::KW_LOOP)
      return parse_loop_stmt();
    if (after == TokenType::KW_WHILE)
      return parse_while_stmt();
    if (after == TokenType::KW_FOR)
      return parse_for_stmt();
  }

  if (check(TokenType::KW_LET))
    return parse_let_stmt();
  if (check(TokenType::KW_RETURN))
    return parse_return_stmt();
  if (check(TokenType::KW_DEFER))
    return parse_defer_stmt();
  if (check(TokenType::KW_LOOP))
    return parse_loop_stmt();
  if (check(TokenType::KW_WHILE))
    return parse_while_stmt();
  if (check(TokenType::KW_FOR))
    return parse_for_stmt();
  if (check(TokenType::KW_COMPTIME))
    return parse_comptime_stmt();

  return parse_expr_stmt();
}

optional<Spanned<ast::Stmt>> Parser::parse_let_stmt() {
  size_t start = current_pos;
  consume(TokenType::KW_LET, "'let'");
  if (has_errors())
    return nullopt;

  auto name_tok = consume(TokenType::IDENT, "variable name");
  if (has_errors())
    return nullopt;

  ast::LetStmt let_stmt;
  let_stmt.name = name_tok.lexeme;

  if (match(TokenType::COLON)) {
    auto t = parse_type();
    if (!t)
      return nullopt;
    let_stmt.type = make_unique<ast::TypeAnnot>(std::move(t->value));
  }

  consume(TokenType::ASSIGN, "'='");
  if (has_errors())
    return nullopt;

  auto init = parse_expression();
  if (!init)
    return nullopt;
  let_stmt.init = make_unique<ast::Expr>(std::move(init->value));

  consume(TokenType::SEMICOLON, "';'");
  if (has_errors())
    return nullopt;

  let_stmt.span = get_span_for(start);
  ast::Stmt stmt;
  stmt.span = let_stmt.span;
  stmt.value = std::move(let_stmt);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_return_stmt() {
  size_t start = current_pos;
  consume(TokenType::KW_RETURN, "'return'");
  if (has_errors())
    return nullopt;

  ast::ReturnStmt ret;

  if (!check(TokenType::SEMICOLON)) {
    auto expr = parse_expression();
    if (!expr)
      return nullopt;
    ret.value = make_unique<ast::Expr>(std::move(expr->value));
  }

  consume(TokenType::SEMICOLON, "';'");
  if (has_errors())
    return nullopt;

  ret.span = get_span_for(start);
  ast::Stmt stmt;
  stmt.span = ret.span;
  stmt.value = std::move(ret);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_defer_stmt() {
  size_t start = current_pos;
  consume(TokenType::KW_DEFER, "'defer'");
  if (has_errors())
    return nullopt;

  ast::DeferStmt defer;

  if (check(TokenType::LBRACE)) {
    // defer block
    auto blk = parse_block();
    if (!blk)
      return nullopt;

    // Wrap the block as an ExprStmt containing an... actually,
    // DeferStmt holds a Stmt. A block by itself isn't a Stmt in our AST.
    // The grammar says: defer_stmt ::= "defer" ( expr_stmt | block )
    // For the block case, we'll wrap it as an ExprStmt with a dummy.
    // Actually, let's look at the AST: DeferStmt has a unique_ptr<Stmt>.
    // For a block, we can create a LoopStmt-like wrapper... but that's wrong.
    // Let me just store it as an ExprStmt where the expr is the block content.
    // Actually simplest: create a synthetic "block statement". But we don't
    // have one. Let me just wrap each statement from the block individually.
    // Per the grammar, defer can wrap a block. But the AST says defer wraps a
    // single Stmt. The simplest correct approach: if block has multiple stmts,
    // wrap in a LoopStmt that immediately breaks? No, that's hacky. Let's just
    // treat "defer { ... }" as "defer" followed by an expression statement
    // where the expression is... hmm.
    //
    // OK: the practical thing is that DeferStmt stores Stmt, and for the block
    // case we need a new variant or reuse ExprStmt. Let me just store the block
    // as an ExprStmt with the expr being the if_expr/match_expr pattern. But
    // blocks aren't expressions either...
    //
    // The most pragmatic solution: treat "defer { stmts }" by wrapping the
    // stmts in a LoopStmt with no label that only runs once. No...
    //
    // Simplest fix: DeferStmt actually stores either a Stmt or a Block. But the
    // AST already defines it as unique_ptr<Stmt>. Let me create a "BlockStmt"
    // or just use the existing block to create a synthetic stmt. Let's make it
    // an ExprStmt containing an IfExpr with condition `true` -- no, too hacky.
    //
    // Actually, looking at the all.shiki usage:
    // defer c_free(p);           -- expr_stmt
    // defer { c_free(s); ... }   -- block
    //
    // Let me just store empty ExprStmt for single-stmt case and for block,
    // actually let's just use a LoopStmt with one iteration. No.
    //
    // I'll add a special case: if block has exactly one stmt, use that stmt.
    // If multiple, we need a compound. Let me just keep the first approach:
    // create a new LetStmt variant? No.
    //
    // OK, simplest: I'll change DeferStmt to hold an optional<Block> alongside
    // the stmt. Actually let me not modify AST. Let me just wrap block stmts
    // into a synthetic compound by making it a ForStmt or... OK no.
    //
    // PRAGMATIC: Store the Block in a synthetic LoopStmt that the codegen knows
    // is a defer-block. Actually the cleanest: make the block an ExprStmt whose
    // expr is a MatchExpr with no arms... no.
    //
    // Final answer: I'll wrap the block in a ComptimeStmt... no.
    //
    // Let me just create a "compound" by nesting ExprStmt(IfExpr(true, block)).
    ast::IfExpr if_expr;
    ast::IfBranch branch;
    ast::Expr true_expr;
    true_expr.span = blk->span;
    true_expr.value = ast::BoolLiteral{blk->span, true};
    branch.condition = make_unique<ast::Expr>(std::move(true_expr));
    branch.body = std::move(*blk);
    branch.span = branch.body.span;
    if_expr.branches.push_back(std::move(branch));
    if_expr.span = get_span_for(start);

    ast::Expr wrapper;
    wrapper.span = if_expr.span;
    wrapper.value = std::move(if_expr);

    ast::ExprStmt es;
    es.expr = make_unique<ast::Expr>(std::move(wrapper));
    es.span = get_span_for(start);

    ast::Stmt inner_stmt;
    inner_stmt.span = es.span;
    inner_stmt.value = std::move(es);

    defer.stmt = make_unique<ast::Stmt>(std::move(inner_stmt));
  } else {
    // defer expr_stmt
    auto es = parse_expr_stmt();
    if (!es)
      return nullopt;
    defer.stmt = make_unique<ast::Stmt>(std::move(es->value));
  }

  defer.span = get_span_for(start);
  ast::Stmt stmt;
  stmt.span = defer.span;
  stmt.value = std::move(defer);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_loop_stmt() {
  size_t start = current_pos;

  optional<ast::Identifier> label;
  auto lbl = parse_label();
  if (lbl)
    label = *lbl;

  consume(TokenType::KW_LOOP, "'loop'");
  if (has_errors())
    return nullopt;

  auto body = parse_block();
  if (!body)
    return nullopt;

  ast::LoopStmt loop;
  loop.label = label;
  loop.body = std::move(*body);
  loop.span = get_span_for(start);

  ast::Stmt stmt;
  stmt.span = loop.span;
  stmt.value = std::move(loop);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_while_stmt() {
  size_t start = current_pos;

  optional<ast::Identifier> label;
  auto lbl = parse_label();
  if (lbl)
    label = *lbl;

  consume(TokenType::KW_WHILE, "'while'");
  if (has_errors())
    return nullopt;

  auto cond = parse_expression();
  if (!cond)
    return nullopt;

  auto body = parse_block();
  if (!body)
    return nullopt;

  ast::WhileStmt ws;
  ws.label = label;
  ws.condition = make_unique<ast::Expr>(std::move(cond->value));
  ws.body = std::move(*body);
  ws.span = get_span_for(start);

  ast::Stmt stmt;
  stmt.span = ws.span;
  stmt.value = std::move(ws);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_for_stmt() {
  size_t start = current_pos;

  optional<ast::Identifier> label;
  auto lbl = parse_label();
  if (lbl)
    label = *lbl;

  consume(TokenType::KW_FOR, "'for'");
  if (has_errors())
    return nullopt;

  auto var_tok = consume(TokenType::IDENT, "loop variable name");
  if (has_errors())
    return nullopt;

  consume(TokenType::KW_IN, "'in'");
  if (has_errors())
    return nullopt;

  auto iterable = parse_expression();
  if (!iterable)
    return nullopt;

  auto body = parse_block();
  if (!body)
    return nullopt;

  ast::ForStmt fs;
  fs.label = label;
  fs.var = var_tok.lexeme;
  fs.iterable = make_unique<ast::Expr>(std::move(iterable->value));
  fs.body = std::move(*body);
  fs.span = get_span_for(start);

  ast::Stmt stmt;
  stmt.span = fs.span;
  stmt.value = std::move(fs);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_comptime_stmt() {
  size_t start = current_pos;
  consume(TokenType::KW_COMPTIME, "'comptime'");
  if (has_errors())
    return nullopt;

  ast::ComptimeStmt cs;

  if (check(TokenType::KW_LET)) {
    auto inner = parse_let_stmt();
    if (!inner)
      return nullopt;
    cs.stmt = make_unique<ast::Stmt>(std::move(inner->value));
  } else if (check(TokenType::KW_FOR)) {
    auto inner = parse_for_stmt();
    if (!inner)
      return nullopt;
    cs.stmt = make_unique<ast::Stmt>(std::move(inner->value));
  } else {
    // expr_stmt (e.g. comptime if ...)
    auto inner = parse_expr_stmt();
    if (!inner)
      return nullopt;
    cs.stmt = make_unique<ast::Stmt>(std::move(inner->value));
  }

  cs.span = get_span_for(start);
  ast::Stmt stmt;
  stmt.span = cs.span;
  stmt.value = std::move(cs);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

optional<Spanned<ast::Stmt>> Parser::parse_expr_stmt() {
  size_t start = current_pos;
  auto expr = parse_expression();
  if (!expr)
    return nullopt;

  // Semicolons are not required when:
  // 1. if/match expressions used as statements (they end with '}')
  // 2. The expression is a trailing value in a block (followed directly by '}')
  bool needs_semi = true;
  if (holds_alternative<ast::IfExpr>(expr->value.value) ||
      holds_alternative<ast::MatchExpr>(expr->value.value)) {
    needs_semi = check(TokenType::SEMICOLON);
  } else if (check(TokenType::RBRACE)) {
    // Trailing expression in a block (the block's value)
    needs_semi = false;
  }

  if (needs_semi) {
    consume(TokenType::SEMICOLON, "';'");
    if (has_errors())
      return nullopt;
  }

  ast::ExprStmt es;
  es.expr = make_unique<ast::Expr>(std::move(expr->value));
  es.span = get_span_for(start);

  ast::Stmt stmt;
  stmt.span = es.span;
  stmt.value = std::move(es);
  return Spanned<ast::Stmt>(std::move(stmt), stmt.span);
}

} // namespace shikimori
