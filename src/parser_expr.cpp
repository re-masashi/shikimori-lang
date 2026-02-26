#include "parser.h"

namespace shikimori {

std::optional<Spanned<ast::Expr>> Parser::parse_expression() {
  return parse_assignment();
}

std::optional<Spanned<ast::Expr>> Parser::parse_assignment() {
  size_t start = current_pos;
  auto left = parse_logical_or();
  if (!left)
    return std::nullopt;

  // Range operators: .. and ..=
  if (check(TokenType::DOT_DOT) || check(TokenType::DOT_DOT_EQ)) {
    bool inclusive = current().type == TokenType::DOT_DOT_EQ;
    advance();
    auto right = parse_logical_or();
    if (!right)
      return std::nullopt;

    ast::RangeExpr range;
    range.start = std::make_unique<ast::Expr>(std::move(left->value));
    range.end = std::make_unique<ast::Expr>(std::move(right->value));
    range.inclusive = inclusive;
    range.span = get_span_for(start);

    ast::Expr expr;
    expr.span = range.span;
    expr.value = std::move(range);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::ASSIGN)) {
    auto right = parse_expression();
    if (!right)
      return std::nullopt;

    ast::Assignment assign;
    assign.target = std::make_unique<ast::Expr>(std::move(left->value));
    assign.value = std::make_unique<ast::Expr>(std::move(right->value));
    assign.span = get_span_for(start);

    ast::Expr expr;
    expr.span = assign.span;
    expr.value = std::move(assign);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  return left;
}

// Helper macro for binary operator parsing
#define DEFINE_BINARY_PARSER(name, next_parser, ...)                           \
  std::optional<Spanned<ast::Expr>> Parser::name() {                           \
    size_t start = current_pos;                                                \
    auto left = next_parser();                                                 \
    if (!left)                                                                 \
      return std::nullopt;                                                     \
    while (check_any({__VA_ARGS__})) {                                         \
      auto op_tok = current();                                                 \
      advance();                                                               \
      auto right = next_parser();                                              \
      if (!right)                                                              \
        return std::nullopt;                                                   \
      ast::BinaryOp op;                                                        \
      switch (op_tok.type) {                                                   \
      case TokenType::OR:                                                      \
        op = ast::BinaryOp::Or;                                                \
        break;                                                                 \
      case TokenType::AND:                                                     \
        op = ast::BinaryOp::And;                                               \
        break;                                                                 \
      case TokenType::BIT_OR:                                                  \
        op = ast::BinaryOp::BitOr;                                             \
        break;                                                                 \
      case TokenType::BIT_XOR:                                                 \
        op = ast::BinaryOp::BitXor;                                            \
        break;                                                                 \
      case TokenType::BIT_AND:                                                 \
        op = ast::BinaryOp::BitAnd;                                            \
        break;                                                                 \
      case TokenType::EQ:                                                      \
        op = ast::BinaryOp::Eq;                                                \
        break;                                                                 \
      case TokenType::NEQ:                                                     \
        op = ast::BinaryOp::Neq;                                               \
        break;                                                                 \
      case TokenType::LT:                                                      \
        op = ast::BinaryOp::Lt;                                                \
        break;                                                                 \
      case TokenType::GT:                                                      \
        op = ast::BinaryOp::Gt;                                                \
        break;                                                                 \
      case TokenType::LTE:                                                     \
        op = ast::BinaryOp::Lte;                                               \
        break;                                                                 \
      case TokenType::GTE:                                                     \
        op = ast::BinaryOp::Gte;                                               \
        break;                                                                 \
      case TokenType::LSHIFT:                                                  \
        op = ast::BinaryOp::LShift;                                            \
        break;                                                                 \
      case TokenType::RSHIFT:                                                  \
        op = ast::BinaryOp::RShift;                                            \
        break;                                                                 \
      case TokenType::PLUS:                                                    \
        op = ast::BinaryOp::Add;                                               \
        break;                                                                 \
      case TokenType::MINUS:                                                   \
        op = ast::BinaryOp::Sub;                                               \
        break;                                                                 \
      case TokenType::STAR:                                                    \
        op = ast::BinaryOp::Mul;                                               \
        break;                                                                 \
      case TokenType::SLASH:                                                   \
        op = ast::BinaryOp::Div;                                               \
        break;                                                                 \
      case TokenType::PERCENT:                                                 \
        op = ast::BinaryOp::Mod;                                               \
        break;                                                                 \
      default:                                                                 \
        op = ast::BinaryOp::Add;                                               \
        break;                                                                 \
      }                                                                        \
      ast::BinaryExpr bin;                                                     \
      bin.op = op;                                                             \
      bin.left = std::make_unique<ast::Expr>(std::move(left->value));          \
      bin.right = std::make_unique<ast::Expr>(std::move(right->value));        \
      bin.span = get_span_for(start);                                          \
      ast::Expr expr;                                                          \
      expr.span = bin.span;                                                    \
      expr.value = std::move(bin);                                             \
      left = Spanned<ast::Expr>(std::move(expr), expr.span);                   \
    }                                                                          \
    return left;                                                               \
  }

DEFINE_BINARY_PARSER(parse_logical_or, parse_logical_and, TokenType::OR)
DEFINE_BINARY_PARSER(parse_logical_and, parse_bitwise_or, TokenType::AND)
DEFINE_BINARY_PARSER(parse_bitwise_or, parse_bitwise_xor, TokenType::BIT_OR)
DEFINE_BINARY_PARSER(parse_bitwise_xor, parse_bitwise_and, TokenType::BIT_XOR)
DEFINE_BINARY_PARSER(parse_bitwise_and, parse_equality, TokenType::BIT_AND)
DEFINE_BINARY_PARSER(parse_equality, parse_comparison, TokenType::EQ,
                     TokenType::NEQ)
DEFINE_BINARY_PARSER(parse_comparison, parse_shift, TokenType::LT,
                     TokenType::GT, TokenType::LTE, TokenType::GTE)
DEFINE_BINARY_PARSER(parse_shift, parse_additive, TokenType::LSHIFT,
                     TokenType::RSHIFT)
DEFINE_BINARY_PARSER(parse_additive, parse_multiplicative, TokenType::PLUS,
                     TokenType::MINUS)
DEFINE_BINARY_PARSER(parse_multiplicative, parse_unary, TokenType::STAR,
                     TokenType::SLASH, TokenType::PERCENT)

#undef DEFINE_BINARY_PARSER

std::optional<Spanned<ast::Expr>> Parser::parse_unary() {
  size_t start = current_pos;

  if (match(TokenType::NOT)) {
    auto operand = parse_unary();
    if (!operand)
      return std::nullopt;
    ast::UnaryExpr un;
    un.op = ast::UnaryOp::Not;
    un.operand = std::make_unique<ast::Expr>(std::move(operand->value));
    un.span = get_span_for(start);
    ast::Expr expr;
    expr.span = un.span;
    expr.value = std::move(un);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::MINUS)) {
    // Check if it's a negative literal
    if (check(TokenType::INT_LIT)) {
      auto tok = current();
      advance();
      ast::IntLiteral lit;
      lit.value = -(tok.int_value.value_or(0));
      lit.span = get_span_for(start);
      ast::Expr expr;
      expr.span = lit.span;
      expr.value = std::move(lit);
      return Spanned<ast::Expr>(std::move(expr), expr.span);
    }
    if (check(TokenType::FLOAT_LIT)) {
      auto tok = current();
      advance();
      ast::FloatLiteral lit;
      lit.value = -(tok.float_value.value_or(0.0));
      lit.span = get_span_for(start);
      ast::Expr expr;
      expr.span = lit.span;
      expr.value = std::move(lit);
      return Spanned<ast::Expr>(std::move(expr), expr.span);
    }
    auto operand = parse_unary();
    if (!operand)
      return std::nullopt;
    ast::UnaryExpr un;
    un.op = ast::UnaryOp::Neg;
    un.operand = std::make_unique<ast::Expr>(std::move(operand->value));
    un.span = get_span_for(start);
    ast::Expr expr;
    expr.span = un.span;
    expr.value = std::move(un);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::STAR)) {
    auto operand = parse_unary();
    if (!operand)
      return std::nullopt;
    ast::UnaryExpr un;
    un.op = ast::UnaryOp::Deref;
    un.operand = std::make_unique<ast::Expr>(std::move(operand->value));
    un.span = get_span_for(start);
    ast::Expr expr;
    expr.span = un.span;
    expr.value = std::move(un);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::BIT_AND)) {
    auto operand = parse_unary();
    if (!operand)
      return std::nullopt;
    ast::UnaryExpr un;
    un.op = ast::UnaryOp::AddrOf;
    un.operand = std::make_unique<ast::Expr>(std::move(operand->value));
    un.span = get_span_for(start);
    ast::Expr expr;
    expr.span = un.span;
    expr.value = std::move(un);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::TILDE)) {
    auto operand = parse_unary();
    if (!operand)
      return std::nullopt;
    ast::UnaryExpr un;
    un.op = ast::UnaryOp::BitNot;
    un.operand = std::make_unique<ast::Expr>(std::move(operand->value));
    un.span = get_span_for(start);
    ast::Expr expr;
    expr.span = un.span;
    expr.value = std::move(un);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  return parse_postfix();
}

std::optional<Spanned<ast::Expr>> Parser::parse_postfix() {
  size_t start = current_pos;
  auto left = parse_primary();
  if (!left)
    return std::nullopt;

  while (true) {
    if (check(TokenType::DOT)) {
      advance(); // consume '.'
      auto name_tok = consume(TokenType::IDENT, "field/method name");
      if (has_errors())
        return std::nullopt;

      if (match(TokenType::LPAREN)) {
        // method call
        ast::MethodCall mc;
        mc.object = std::make_unique<ast::Expr>(std::move(left->value));
        mc.method = name_tok.lexeme;
        if (!check(TokenType::RPAREN)) {
          auto args = parse_arg_list();
          if (!args)
            return std::nullopt;
          mc.args = std::move(*args);
        }
        consume(TokenType::RPAREN, "')'");
        if (has_errors())
          return std::nullopt;
        mc.span = get_span_for(start);
        ast::Expr expr;
        expr.span = mc.span;
        expr.value = std::move(mc);
        left = Spanned<ast::Expr>(std::move(expr), expr.span);
      } else {
        // field access
        ast::FieldAccess fa;
        fa.object = std::make_unique<ast::Expr>(std::move(left->value));
        fa.field = name_tok.lexeme;
        fa.span = get_span_for(start);
        ast::Expr expr;
        expr.span = fa.span;
        expr.value = std::move(fa);
        left = Spanned<ast::Expr>(std::move(expr), expr.span);
      }
    } else if (match(TokenType::HASH_BRACK)) {
      // index access: #[expr]
      auto idx = parse_expression();
      if (!idx)
        return std::nullopt;
      consume(TokenType::RBRACKET, "']'");
      if (has_errors())
        return std::nullopt;

      ast::IndexAccess ia;
      ia.object = std::make_unique<ast::Expr>(std::move(left->value));
      ia.index = std::make_unique<ast::Expr>(std::move(idx->value));
      ia.span = get_span_for(start);
      ast::Expr expr;
      expr.span = ia.span;
      expr.value = std::move(ia);
      left = Spanned<ast::Expr>(std::move(expr), expr.span);
    } else if (check(TokenType::LPAREN)) {
      // function call
      advance();
      ast::Call call;
      call.callee = std::make_unique<ast::Expr>(std::move(left->value));
      if (!check(TokenType::RPAREN)) {
        auto args = parse_arg_list();
        if (!args)
          return std::nullopt;
        call.args = std::move(*args);
      }
      consume(TokenType::RPAREN, "')'");
      if (has_errors())
        return std::nullopt;
      call.span = get_span_for(start);
      ast::Expr expr;
      expr.span = call.span;
      expr.value = std::move(call);
      left = Spanned<ast::Expr>(std::move(expr), expr.span);
    } else {
      break;
    }
  }

  return left;
}

// Returns true if current position (at '{') looks like a struct init body
// We're positioned AT the '{', so we need to peek inside
static bool looks_like_struct_init_body(const Parser &p) {
  // current = '{', peek(1) = first inside token, peek(2) = second inside
  if (p.peek(1).type == TokenType::RBRACE)
    return true; // empty struct init: Name {}
  if (p.peek(1).type == TokenType::IDENT && p.peek(2).type == TokenType::COLON)
    return true;
  return false;
}

std::optional<Spanned<ast::Expr>> Parser::parse_primary() {
  size_t start = current_pos;

  // Literals
  if (check(TokenType::INT_LIT)) {
    auto tok = current();
    advance();
    ast::IntLiteral lit;
    lit.value = tok.int_value.value_or(0);
    lit.span = get_span_for(start);
    ast::Expr expr;
    expr.span = lit.span;
    expr.value = std::move(lit);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (check(TokenType::FLOAT_LIT)) {
    auto tok = current();
    advance();
    ast::FloatLiteral lit;
    lit.value = tok.float_value.value_or(0.0);
    lit.span = get_span_for(start);
    ast::Expr expr;
    expr.span = lit.span;
    expr.value = std::move(lit);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (check(TokenType::BOOL_LIT)) {
    auto tok = current();
    advance();
    ast::BoolLiteral lit;
    lit.value = (tok.lexeme == "true");
    lit.span = get_span_for(start);
    ast::Expr expr;
    expr.span = lit.span;
    expr.value = std::move(lit);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (check(TokenType::STRING_LIT)) {
    auto tok = current();
    advance();
    ast::StringLiteral lit;
    lit.value = tok.lexeme;
    lit.span = get_span_for(start);
    ast::Expr expr;
    expr.span = lit.span;
    expr.value = std::move(lit);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  if (match(TokenType::KW_NULL)) {
    ast::NullLiteral lit;
    lit.span = get_span_for(start);
    ast::Expr expr;
    expr.span = lit.span;
    expr.value = std::move(lit);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  // if expression
  if (check(TokenType::KW_IF))
    return parse_if_expr();

  // match expression
  if (check(TokenType::KW_MATCH))
    return parse_match_expr();

  // loop as expression (break with value)
  if (check(TokenType::KW_LOOP)) {
    size_t loop_start = current_pos;
    advance(); // consume 'loop'
    auto body = parse_block();
    if (!body)
      return std::nullopt;
    // Wrap as a LoopStmt inside... actually this needs to be an expression.
    // Let's handle loop-as-expr: we create a MatchExpr-like wrapper?
    // Actually looking at the AST, there's no LoopExpr. Let's look at usage:
    // let result: i32 = loop { break 99; };
    // This should be a LoopStmt but used as an expression.
    // We don't have a LoopExpr in the variant. Let's just parse it as
    // an identifier with some block? No.
    // The pragmatic solution: treat "loop { ... }" in expression position
    // as an IfExpr(true, block) -- the codegen/typechecker knows.
    // Actually, let's just create an IfExpr with condition true and the body.
    ast::IfExpr if_expr;
    ast::IfBranch branch;
    ast::Expr true_expr;
    true_expr.span = body->span;
    true_expr.value = ast::BoolLiteral{body->span, true};
    branch.condition = std::make_unique<ast::Expr>(std::move(true_expr));
    branch.body = std::move(*body);
    branch.span = get_span_for(loop_start);
    if_expr.branches.push_back(std::move(branch));
    if_expr.span = get_span_for(loop_start);
    ast::Expr expr;
    expr.span = if_expr.span;
    expr.value = std::move(if_expr);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  // break
  if (check(TokenType::KW_BREAK))
    return parse_break_expr();

  // continue
  if (check(TokenType::KW_CONTINUE))
    return parse_continue_expr();

  // comptime expression
  if (check(TokenType::KW_COMPTIME))
    return parse_comptime_expr();

  // builtin call
  if (check(TokenType::AT))
    return parse_builtin_call();

  // parenthesized expression
  if (match(TokenType::LPAREN)) {
    auto inner = parse_expression();
    if (!inner)
      return std::nullopt;
    consume(TokenType::RPAREN, "')'");
    if (has_errors())
      return std::nullopt;
    return inner;
  }

  // Type prefixed initializers / Array literals: [8]i32 { ... }
  if (check(TokenType::LBRACKET) || check(TokenType::STAR) ||
      check(TokenType::QUESTION) || is_primitive_keyword(current().type)) {
    // Save position to backtrack if it's not an init
    size_t saved = current_pos;
    auto type = parse_type();
    if (type && check(TokenType::LBRACE) &&
        looks_like_struct_init_body(*this)) {
      advance(); // consume '{'
      ast::TypeInit ti;
      ti.type = std::make_unique<ast::TypeAnnot>(std::move(type->value));
      if (!check(TokenType::RBRACE)) {
        auto fields = parse_field_init_list();
        if (!fields)
          return std::nullopt;
        ti.fields = std::move(*fields);
      }
      consume(TokenType::RBRACE, "'}'");
      if (has_errors())
        return std::nullopt;
      ti.span = get_span_for(start);
      ast::Expr expr;
      expr.span = ti.span;
      expr.value = std::move(ti);
      return Spanned<ast::Expr>(std::move(expr), expr.span);
    } else {
      // Not a type-prefixed init, backtrack
      current_pos = saved;
      clear_errors();
    }
  }

  // Identifier-based: could be plain ident, generic ident, struct init, or
  // scope access
  if (check(TokenType::IDENT)) {
    auto name_tok = current();
    advance();

    // Check for scope access: IDENT :: IDENT [(expr)]
    if (check(TokenType::DOUBLE_COLON)) {
      advance(); // consume '::'
      auto member_tok = consume(TokenType::IDENT, "scope member name");
      if (has_errors())
        return std::nullopt;

      ast::ScopeAccess sa;
      sa.scope = name_tok.lexeme;
      sa.member = member_tok.lexeme;
      sa.span = get_span_for(start);

      // Check for payload: (expr, expr, ...)
      if (match(TokenType::LPAREN)) {
        std::vector<std::unique_ptr<ast::Expr>> args;
        if (!check(TokenType::RPAREN)) {
          do {
            auto arg = parse_expression();
            if (!arg)
              return std::nullopt;
            args.push_back(std::make_unique<ast::Expr>(std::move(arg->value)));
          } while (match(TokenType::COMMA));
        }
        consume(TokenType::RPAREN, "')'");
        if (has_errors())
          return std::nullopt;
        sa.payload = std::move(args);
      }

      ast::Expr expr;
      expr.span = sa.span;
      expr.value = std::move(sa);
      return Spanned<ast::Expr>(std::move(expr), expr.span);
    }

    // Check for generic instantiation: IDENT [ types ]
    if (check(TokenType::LBRACKET)) {
      // Could be generic ident or struct init with generics
      // Save position for backtracking
      size_t saved = current_pos;
      advance(); // consume '['

      // Try to parse as type arg list
      std::vector<std::unique_ptr<ast::TypeAnnot>> type_args;
      bool valid_types = true;
      while (!check(TokenType::RBRACKET) && !is_at_end()) {
        auto t = parse_type();
        if (!t) {
          valid_types = false;
          break;
        }
        type_args.push_back(
            std::make_unique<ast::TypeAnnot>(std::move(t->value)));
        if (!match(TokenType::COMMA))
          break;
      }

      if (!valid_types || !check(TokenType::RBRACKET)) {
        // Not valid type args, backtrack
        current_pos = saved;
        clear_errors();
        ast::IdentifierExpr ident;
        ident.name = name_tok.lexeme;
        ident.span = Span(name_tok.start, name_tok.end, file);
        ast::Expr expr;
        expr.span = ident.span;
        expr.value = std::move(ident);
        return Spanned<ast::Expr>(std::move(expr), expr.span);
      }

      advance(); // consume ']'

      // Check if followed by { for struct init with generics
      if (check(TokenType::LBRACE) && looks_like_struct_init_body(*this)) {
        // Don't advance past '{' yet - parse_field_init_list doesn't expect it
        advance(); // consume '{'
        ast::StructInit si;
        si.name = name_tok.lexeme;
        si.generic_args = std::move(type_args);
        if (!check(TokenType::RBRACE)) {
          auto fields = parse_field_init_list();
          if (!fields)
            return std::nullopt;
          si.fields = std::move(*fields);
        }
        consume(TokenType::RBRACE, "'}'");
        if (has_errors())
          return std::nullopt;
        si.span = get_span_for(start);
        ast::Expr expr;
        expr.span = si.span;
        expr.value = std::move(si);
        return Spanned<ast::Expr>(std::move(expr), expr.span);
      }

      // Check for scope access on generic: IDENT [types] :: IDENT [(expr)]
      if (check(TokenType::DOUBLE_COLON)) {
        advance(); // consume '::'
        auto member_tok = consume(TokenType::IDENT, "scope member name");
        if (has_errors())
          return std::nullopt;

        ast::ScopeAccess sa;
        sa.scope = name_tok.lexeme;
        sa.member = member_tok.lexeme;
        sa.generic_args = std::move(type_args);
        sa.span = get_span_for(start);

        // Check for payload: (expr, expr, ...)
        if (match(TokenType::LPAREN)) {
          std::vector<std::unique_ptr<ast::Expr>> args;
          if (!check(TokenType::RPAREN)) {
            do {
              auto arg = parse_expression();
              if (!arg)
                return std::nullopt;
              args.push_back(
                  std::make_unique<ast::Expr>(std::move(arg->value)));
            } while (match(TokenType::COMMA));
          }
          consume(TokenType::RPAREN, "')'");
          if (has_errors())
            return std::nullopt;
          sa.payload = std::move(args);
        }

        ast::Expr expr;
        expr.span = sa.span;
        expr.value = std::move(sa);
        return Spanned<ast::Expr>(std::move(expr), expr.span);
      }

      // Generic identifier (e.g. Pair[B, A])
      ast::GenericIdent gi;
      gi.name = name_tok.lexeme;
      gi.type_args = std::move(type_args);
      gi.span = get_span_for(start);
      ast::Expr expr;
      expr.span = gi.span;
      expr.value = std::move(gi);
      return Spanned<ast::Expr>(std::move(expr), expr.span);
    }

    // Check for struct init: IDENT { field: expr, ... }
    if (check(TokenType::LBRACE)) {
      // Lookahead to see if this looks like struct init
      if (looks_like_struct_init_body(*this)) {
        advance(); // consume '{'
        ast::StructInit si;
        si.name = name_tok.lexeme;
        if (!check(TokenType::RBRACE)) {
          auto fields = parse_field_init_list();
          if (!fields)
            return std::nullopt;
          si.fields = std::move(*fields);
        }
        consume(TokenType::RBRACE, "'}'");
        if (has_errors())
          return std::nullopt;
        si.span = get_span_for(start);
        ast::Expr expr;
        expr.span = si.span;
        expr.value = std::move(si);
        return Spanned<ast::Expr>(std::move(expr), expr.span);
      }
    }

    // Plain identifier
    ast::IdentifierExpr ident;
    ident.name = name_tok.lexeme;
    ident.span = Span(name_tok.start, name_tok.end, file);
    ast::Expr expr;
    expr.span = ident.span;
    expr.value = std::move(ident);
    return Spanned<ast::Expr>(std::move(expr), expr.span);
  }

  report_error_at_current("expected expression");
  return std::nullopt;
}

std::optional<Spanned<ast::Expr>> Parser::parse_if_expr() {
  size_t start = current_pos;
  consume(TokenType::KW_IF, "'if'");
  if (has_errors())
    return std::nullopt;

  ast::IfExpr if_expr;

  auto cond = parse_expression();
  if (!cond)
    return std::nullopt;
  auto body = parse_block();
  if (!body)
    return std::nullopt;

  ast::IfBranch first;
  first.condition = std::make_unique<ast::Expr>(std::move(cond->value));
  first.body = std::move(*body);
  first.span = get_span_for(start);
  if_expr.branches.push_back(std::move(first));

  while (check(TokenType::KW_ELSE)) {
    advance(); // consume 'else'
    if (match(TokenType::KW_IF)) {
      auto ei_cond = parse_expression();
      if (!ei_cond)
        return std::nullopt;
      auto ei_body = parse_block();
      if (!ei_body)
        return std::nullopt;
      ast::IfBranch branch;
      branch.condition = std::make_unique<ast::Expr>(std::move(ei_cond->value));
      branch.body = std::move(*ei_body);
      branch.span = get_span_for(start);
      if_expr.branches.push_back(std::move(branch));
    } else {
      auto else_body = parse_block();
      if (!else_body)
        return std::nullopt;
      if_expr.else_branch = std::move(*else_body);
      break;
    }
  }

  if_expr.span = get_span_for(start);
  ast::Expr expr;
  expr.span = if_expr.span;
  expr.value = std::move(if_expr);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

std::optional<Spanned<ast::Expr>> Parser::parse_match_expr() {
  size_t start = current_pos;
  consume(TokenType::KW_MATCH, "'match'");
  if (has_errors())
    return std::nullopt;

  auto subject = parse_expression();
  if (!subject)
    return std::nullopt;

  consume(TokenType::LBRACE, "'{'");
  if (has_errors())
    return std::nullopt;

  ast::MatchExpr match_expr;
  match_expr.subject = std::make_unique<ast::Expr>(std::move(subject->value));

  while (!check(TokenType::RBRACE) && !is_at_end()) {
    auto arm = parse_match_arm();
    if (!arm)
      return std::nullopt;
    match_expr.arms.push_back(std::move(*arm));
  }

  consume(TokenType::RBRACE, "'}'");
  if (has_errors())
    return std::nullopt;

  match_expr.span = get_span_for(start);
  ast::Expr expr;
  expr.span = match_expr.span;
  expr.value = std::move(match_expr);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

std::optional<ast::MatchArm> Parser::parse_match_arm() {
  size_t start = current_pos;
  auto pat = parse_pattern();
  if (!pat)
    return std::nullopt;

  consume(TokenType::FAT_ARROW, "'=>'");
  if (has_errors())
    return std::nullopt;

  auto body = parse_block();
  if (!body)
    return std::nullopt;

  // Consume trailing comma after arm block
  match(TokenType::COMMA);

  ast::MatchArm arm;
  arm.pattern = std::move(pat->value);
  arm.body = std::move(*body);
  arm.span = get_span_for(start);
  return arm;
}

std::optional<Spanned<ast::Pattern>> Parser::parse_pattern() {
  size_t start = current_pos;

  // Wildcard: _
  if (check(TokenType::IDENT) && current().lexeme == "_") {
    advance();
    ast::WildcardPattern wp;
    wp.span = get_span_for(start);
    ast::Pattern pat;
    pat.span = wp.span;
    pat.value = std::move(wp);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  // Literal patterns
  if (check(TokenType::INT_LIT)) {
    auto tok = current();
    advance();
    ast::IntLiteral lit;
    lit.value = tok.int_value.value_or(0);
    lit.span = get_span_for(start);
    ast::LiteralPattern lp;
    lp.span = lit.span;
    lp.value = std::move(lit);
    ast::Pattern pat;
    pat.span = lp.span;
    pat.value = std::move(lp);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  if (check(TokenType::FLOAT_LIT)) {
    auto tok = current();
    advance();
    ast::FloatLiteral lit;
    lit.value = tok.float_value.value_or(0.0);
    lit.span = get_span_for(start);
    ast::LiteralPattern lp;
    lp.span = lit.span;
    lp.value = std::move(lit);
    ast::Pattern pat;
    pat.span = lp.span;
    pat.value = std::move(lp);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  if (check(TokenType::BOOL_LIT)) {
    auto tok = current();
    advance();
    ast::BoolLiteral lit;
    lit.value = (tok.lexeme == "true");
    lit.span = get_span_for(start);
    ast::LiteralPattern lp;
    lp.span = lit.span;
    lp.value = std::move(lit);
    ast::Pattern pat;
    pat.span = lp.span;
    pat.value = std::move(lp);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  if (check(TokenType::STRING_LIT)) {
    auto tok = current();
    advance();
    ast::StringLiteral lit;
    lit.value = tok.lexeme;
    lit.span = get_span_for(start);
    ast::LiteralPattern lp;
    lp.span = lit.span;
    lp.value = std::move(lit);
    ast::Pattern pat;
    pat.span = lp.span;
    pat.value = std::move(lp);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  // Variant pattern: IDENT ( IDENT ) or plain IDENT
  if (check(TokenType::IDENT)) {
    auto name_tok = current();
    advance();

    if (match(TokenType::LPAREN)) {
      auto binding_tok = consume(TokenType::IDENT, "binding name");
      if (has_errors())
        return std::nullopt;
      consume(TokenType::RPAREN, "')'");
      if (has_errors())
        return std::nullopt;

      ast::VariantPattern vp;
      vp.variant = name_tok.lexeme;
      vp.binding = binding_tok.lexeme;
      vp.span = get_span_for(start);
      ast::Pattern pat;
      pat.span = vp.span;
      pat.value = std::move(vp);
      return Spanned<ast::Pattern>(std::move(pat), pat.span);
    }

    // Plain identifier pattern (unit variant name or wildcard)
    ast::IdentPattern ip;
    ip.name = name_tok.lexeme;
    ip.span = get_span_for(start);
    ast::Pattern pat;
    pat.span = ip.span;
    pat.value = std::move(ip);
    return Spanned<ast::Pattern>(std::move(pat), pat.span);
  }

  report_error_at_current("expected pattern");
  return std::nullopt;
}

std::optional<Spanned<ast::Expr>> Parser::parse_comptime_expr() {
  size_t start = current_pos;
  consume(TokenType::KW_COMPTIME, "'comptime'");
  if (has_errors())
    return std::nullopt;

  auto inner = parse_expression();
  if (!inner)
    return std::nullopt;

  ast::ComptimeExpr ce;
  ce.expr = std::make_unique<ast::Expr>(std::move(inner->value));
  ce.span = get_span_for(start);

  ast::Expr expr;
  expr.span = ce.span;
  expr.value = std::move(ce);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

std::optional<Spanned<ast::Expr>> Parser::parse_builtin_call() {
  size_t start = current_pos;
  consume(TokenType::AT, "'@'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "builtin name");
  if (has_errors())
    return std::nullopt;

  consume(TokenType::LPAREN, "'('");
  if (has_errors())
    return std::nullopt;

  ast::BuiltinCall bc;
  bc.name = name_tok.lexeme;

  if (!check(TokenType::RPAREN)) {
    while (!check(TokenType::RPAREN) && !is_at_end()) {
      if (is_primitive_keyword(current().type)) {
        auto tok = current();
        advance();
        ast::IdentifierExpr ident;
        ident.name = tok.lexeme;
        ident.span = Span(tok.start, tok.end, file);
        ast::Expr arg_expr;
        arg_expr.span = ident.span;
        arg_expr.value = std::move(ident);
        bc.args.push_back(std::make_unique<ast::Expr>(std::move(arg_expr)));
      } else {
        auto arg = parse_expression();
        if (!arg)
          return std::nullopt;
        bc.args.push_back(std::make_unique<ast::Expr>(std::move(arg->value)));
      }
      if (!match(TokenType::COMMA))
        break;
    }
  }

  consume(TokenType::RPAREN, "')'");
  if (has_errors())
    return std::nullopt;

  bc.span = get_span_for(start);
  ast::Expr expr;
  expr.span = bc.span;
  expr.value = std::move(bc);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

std::optional<Spanned<ast::Expr>> Parser::parse_break_expr() {
  size_t start = current_pos;
  consume(TokenType::KW_BREAK, "'break'");
  if (has_errors())
    return std::nullopt;

  ast::Break brk;

  // Optional label: :name
  if (match(TokenType::COLON)) {
    auto label_tok = consume(TokenType::IDENT, "label name");
    if (has_errors())
      return std::nullopt;
    brk.label = label_tok.lexeme;
  }

  // Optional value
  if (!check(TokenType::SEMICOLON) && !check(TokenType::RBRACE)) {
    auto val = parse_expression();
    if (!val)
      return std::nullopt;
    brk.value = std::make_unique<ast::Expr>(std::move(val->value));
  }

  brk.span = get_span_for(start);
  ast::Expr expr;
  expr.span = brk.span;
  expr.value = std::move(brk);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

std::optional<Spanned<ast::Expr>> Parser::parse_continue_expr() {
  size_t start = current_pos;
  consume(TokenType::KW_CONTINUE, "'continue'");
  if (has_errors())
    return std::nullopt;

  ast::Continue cont;

  if (match(TokenType::COLON)) {
    auto label_tok = consume(TokenType::IDENT, "label name");
    if (has_errors())
      return std::nullopt;
    cont.label = label_tok.lexeme;
  }

  cont.span = get_span_for(start);
  ast::Expr expr;
  expr.span = cont.span;
  expr.value = std::move(cont);
  return Spanned<ast::Expr>(std::move(expr), expr.span);
}

// Argument list helpers

std::optional<std::vector<std::unique_ptr<ast::Expr>>>
Parser::parse_arg_list() {
  std::vector<std::unique_ptr<ast::Expr>> args;
  while (!is_at_end()) {
    auto arg = parse_expression();
    if (!arg)
      return std::nullopt;
    args.push_back(std::make_unique<ast::Expr>(std::move(arg->value)));
    if (!match(TokenType::COMMA))
      break;
  }
  return args;
}

std::optional<
    std::vector<std::pair<ast::Identifier, std::unique_ptr<ast::Expr>>>>
Parser::parse_field_init_list() {
  std::vector<std::pair<ast::Identifier, std::unique_ptr<ast::Expr>>> fields;
  while (!check(TokenType::RBRACE) && !is_at_end()) {
    auto name_tok = consume(TokenType::IDENT, "field name");
    if (has_errors())
      return std::nullopt;
    consume(TokenType::COLON, "':'");
    if (has_errors())
      return std::nullopt;
    auto val = parse_expression();
    if (!val)
      return std::nullopt;
    consume(TokenType::COMMA, "','");
    if (has_errors())
      return std::nullopt;
    fields.emplace_back(name_tok.lexeme,
                        std::make_unique<ast::Expr>(std::move(val->value)));
  }
  return fields;
}

std::optional<std::vector<std::unique_ptr<ast::TypeAnnot>>>
Parser::parse_type_arg_list() {
  std::vector<std::unique_ptr<ast::TypeAnnot>> types;
  while (!is_at_end()) {
    auto t = parse_type();
    if (!t)
      return std::nullopt;
    types.push_back(std::make_unique<ast::TypeAnnot>(std::move(t->value)));
    if (!match(TokenType::COMMA))
      break;
  }
  return types;
}

std::optional<Spanned<ast::Expr>> Parser::parse_struct_init() {
  // This is called from parse_primary, handled inline there
  report_error_at_current(
      "internal: parse_struct_init should not be called directly");
  return std::nullopt;
}

std::optional<Spanned<ast::Expr>> Parser::parse_scope_access() {
  // Union init is handled by postfix (field access / method call)
  return std::nullopt;
}

} // namespace shikimori
