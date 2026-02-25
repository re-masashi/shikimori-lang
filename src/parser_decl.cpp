#include "parser.h"

namespace shikimori {

std::optional<ast::Program> Parser::parse() {
  ast::Program program;
  size_t start = current_pos;

  while (!is_at_end()) {
    auto decl = parse_declaration();
    if (!decl) {
      if (has_errors())
        break;
      report_unexpected_token();
      advance();
      continue;
    }
    program.declarations.push_back(std::move(*decl));
  }

  program.span = get_span_for(start);
  if (has_errors())
    return std::nullopt;
  return program;
}

std::optional<ast::Decl> Parser::parse_declaration() {
  ast::Decl decl;

  if (check(TokenType::KW_FN)) {
    auto fn = parse_fn_decl();
    if (!fn)
      return std::nullopt;
    decl.span = fn->span;
    decl.value = std::move(*fn);
    return decl;
  }
  if (check(TokenType::KW_STRUCT)) {
    auto s = parse_struct_decl();
    if (!s)
      return std::nullopt;
    decl.span = s->span;
    decl.value = std::move(*s);
    return decl;
  }
  if (check(TokenType::KW_UNION)) {
    auto u = parse_union_decl();
    if (!u)
      return std::nullopt;
    decl.span = u->span;
    decl.value = std::move(*u);
    return decl;
  }
  if (check(TokenType::KW_INTERFACE)) {
    auto i = parse_interface_decl();
    if (!i)
      return std::nullopt;
    decl.span = i->span;
    decl.value = std::move(*i);
    return decl;
  }
  if (check(TokenType::KW_EXTERN)) {
    auto e = parse_extern_decl();
    if (!e)
      return std::nullopt;
    decl.span = e->span;
    decl.value = std::move(*e);
    return decl;
  }
  if (check(TokenType::KW_USE)) {
    auto u = parse_use_decl();
    if (!u)
      return std::nullopt;
    decl.span = u->span;
    decl.value = std::move(*u);
    return decl;
  }
  if (check(TokenType::KW_MACRO)) {
    auto m = parse_macro_decl();
    if (!m)
      return std::nullopt;
    decl.span = m->span;
    decl.value = std::move(*m);
    return decl;
  }

  return std::nullopt;
}

std::optional<ast::FnDecl> Parser::parse_fn_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_FN, "'fn'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "function name");
  if (has_errors())
    return std::nullopt;

  ast::FnDecl fn;
  fn.name = name_tok.lexeme;

  // optional generic params
  if (check(TokenType::LBRACKET)) {
    advance();
    while (!check(TokenType::RBRACKET) && !is_at_end()) {
      auto gp = parse_generic_param();
      if (!gp)
        return std::nullopt;
      fn.generic_params.push_back(std::move(*gp));
      if (!match(TokenType::COMMA))
        break;
    }
    consume(TokenType::RBRACKET, "']'");
    if (has_errors())
      return std::nullopt;
  }

  consume(TokenType::LPAREN, "'('");
  if (has_errors())
    return std::nullopt;

  if (!check(TokenType::RPAREN)) {
    while (!is_at_end()) {
      auto p = parse_param();
      if (!p)
        return std::nullopt;
      fn.params.push_back(std::move(*p));
      if (!match(TokenType::COMMA))
        break;
    }
  }
  consume(TokenType::RPAREN, "')'");
  if (has_errors())
    return std::nullopt;

  // optional return type
  if (match(TokenType::ARROW)) {
    auto rt = parse_type();
    if (!rt)
      return std::nullopt;
    fn.return_type = std::make_unique<ast::TypeAnnot>(std::move(rt->value));
  }

  // optional where clause
  if (check(TokenType::KW_WHERE)) {
    advance();
    std::vector<ast::WhereConstraint> constraints;
    while (!check(TokenType::LBRACE) && !is_at_end()) {
      auto wc = parse_where_constraint();
      if (!wc)
        return std::nullopt;
      constraints.push_back(std::move(*wc));
      if (!match(TokenType::COMMA))
        break;
    }
    fn.where_clause = std::move(constraints);
  }

  auto body = parse_block();
  if (!body)
    return std::nullopt;
  fn.body = std::move(*body);
  fn.span = get_span_for(start);
  return fn;
}

std::optional<ast::StructDecl> Parser::parse_struct_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_STRUCT, "'struct'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "struct name");
  if (has_errors())
    return std::nullopt;

  ast::StructDecl decl;
  decl.name = name_tok.lexeme;

  if (check(TokenType::LBRACKET)) {
    advance();
    while (!check(TokenType::RBRACKET) && !is_at_end()) {
      auto gp = parse_generic_param();
      if (!gp)
        return std::nullopt;
      decl.generic_params.push_back(std::move(*gp));
      if (!match(TokenType::COMMA))
        break;
    }
    consume(TokenType::RBRACKET, "']'");
    if (has_errors())
      return std::nullopt;
  }

  consume(TokenType::LBRACE, "'{'");
  if (has_errors())
    return std::nullopt;

  while (!check(TokenType::RBRACE) && !is_at_end()) {
    if (check(TokenType::KW_FN)) {
      auto fn = parse_fn_decl();
      if (!fn)
        return std::nullopt;
      decl.methods.push_back(std::move(*fn));
    } else {
      auto field = parse_field_decl();
      if (!field)
        return std::nullopt;
      decl.fields.push_back(std::move(*field));
    }
  }

  consume(TokenType::RBRACE, "'}'");
  if (has_errors())
    return std::nullopt;
  decl.span = get_span_for(start);
  return decl;
}

std::optional<ast::UnionDecl> Parser::parse_union_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_UNION, "'union'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "union name");
  if (has_errors())
    return std::nullopt;

  ast::UnionDecl decl;
  decl.name = name_tok.lexeme;

  if (check(TokenType::LBRACKET)) {
    advance();
    while (!check(TokenType::RBRACKET) && !is_at_end()) {
      auto gp = parse_generic_param();
      if (!gp)
        return std::nullopt;
      decl.generic_params.push_back(std::move(*gp));
      if (!match(TokenType::COMMA))
        break;
    }
    consume(TokenType::RBRACKET, "']'");
    if (has_errors())
      return std::nullopt;
  }

  consume(TokenType::LBRACE, "'{'");
  if (has_errors())
    return std::nullopt;

  while (!check(TokenType::RBRACE) && !is_at_end()) {
    if (check(TokenType::KW_FN)) {
      auto fn = parse_fn_decl();
      if (!fn)
        return std::nullopt;
      decl.methods.push_back(std::move(*fn));
    } else {
      auto variant = parse_variant_decl();
      if (!variant)
        return std::nullopt;
      decl.variants.push_back(std::move(*variant));
    }
  }

  consume(TokenType::RBRACE, "'}'");
  if (has_errors())
    return std::nullopt;
  decl.span = get_span_for(start);
  return decl;
}

std::optional<ast::InterfaceDecl> Parser::parse_interface_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_INTERFACE, "'interface'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "interface name");
  if (has_errors())
    return std::nullopt;

  ast::InterfaceDecl decl;
  decl.name = name_tok.lexeme;

  consume(TokenType::LBRACE, "'{'");
  if (has_errors())
    return std::nullopt;

  while (!check(TokenType::RBRACE) && !is_at_end()) {
    auto method = parse_interface_method();
    if (!method)
      return std::nullopt;
    decl.methods.push_back(std::move(*method));
  }

  consume(TokenType::RBRACE, "'}'");
  if (has_errors())
    return std::nullopt;
  decl.span = get_span_for(start);
  return decl;
}

std::optional<ast::ExternDecl> Parser::parse_extern_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_EXTERN, "'extern'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "extern function name");
  if (has_errors())
    return std::nullopt;

  ast::ExternDecl decl;
  decl.name = name_tok.lexeme;

  consume(TokenType::LPAREN, "'('");
  if (has_errors())
    return std::nullopt;

  if (!check(TokenType::RPAREN)) {
    while (!is_at_end()) {
      auto p = parse_extern_param();
      if (!p)
        return std::nullopt;
      decl.params.push_back(std::move(*p));
      if (!match(TokenType::COMMA))
        break;
    }
  }

  consume(TokenType::RPAREN, "')'");
  if (has_errors())
    return std::nullopt;

  if (match(TokenType::ARROW)) {
    auto rt = parse_type();
    if (!rt)
      return std::nullopt;
    decl.return_type = std::make_unique<ast::TypeAnnot>(std::move(rt->value));
  }

  decl.span = get_span_for(start);
  return decl;
}

std::optional<ast::UseDecl> Parser::parse_use_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_USE, "'use'");
  if (has_errors())
    return std::nullopt;

  auto path_tok = consume(TokenType::STRING_LIT, "module path string");
  if (has_errors())
    return std::nullopt;

  ast::UseDecl decl;
  decl.path = path_tok.lexeme;

  if (match(TokenType::LPAREN)) {
    while (!check(TokenType::RPAREN) && !is_at_end()) {
      ast::ImportItem item;
      size_t item_start = current_pos;
      auto name_tok2 = consume(TokenType::IDENT, "import name");
      if (has_errors())
        return std::nullopt;
      item.name = name_tok2.lexeme;

      if (match(TokenType::KW_AS)) {
        auto alias_tok = consume(TokenType::IDENT, "alias name");
        if (has_errors())
          return std::nullopt;
        item.alias = alias_tok.lexeme;
      }

      item.span = get_span_for(item_start);
      decl.items.push_back(std::move(item));
      if (!match(TokenType::COMMA))
        break;
    }
    consume(TokenType::RPAREN, "')'");
    if (has_errors())
      return std::nullopt;
  }

  decl.span = get_span_for(start);
  return decl;
}

std::optional<ast::MacroDecl> Parser::parse_macro_decl() {
  size_t start = current_pos;
  consume(TokenType::KW_MACRO, "'macro'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "macro name");
  if (has_errors())
    return std::nullopt;

  ast::MacroDecl decl;
  decl.name = name_tok.lexeme;

  consume(TokenType::LPAREN, "'('");
  if (has_errors())
    return std::nullopt;

  if (!check(TokenType::RPAREN)) {
    while (!is_at_end()) {
      auto p = parse_macro_param();
      if (!p)
        return std::nullopt;
      decl.params.push_back(std::move(*p));
      if (!match(TokenType::COMMA))
        break;
    }
  }

  consume(TokenType::RPAREN, "')'");
  if (has_errors())
    return std::nullopt;

  auto body = parse_block();
  if (!body)
    return std::nullopt;
  decl.body = std::move(*body);
  decl.span = get_span_for(start);
  return decl;
}

std::optional<Spanned<ast::PrimitiveType>> Parser::parse_primitive_type() {
  auto span = get_current_span();
  auto tok = current();
  switch (tok.type) {
  case TokenType::KW_I8:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::I8, span);
  case TokenType::KW_I16:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::I16, span);
  case TokenType::KW_I32:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::I32, span);
  case TokenType::KW_I64:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::I64, span);
  case TokenType::KW_U8:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::U8, span);
  case TokenType::KW_U16:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::U16, span);
  case TokenType::KW_U32:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::U32, span);
  case TokenType::KW_U64:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::U64, span);
  case TokenType::KW_F32:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::F32, span);
  case TokenType::KW_F64:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::F64, span);
  case TokenType::KW_BOOL:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::Bool, span);
  case TokenType::KW_USIZE:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::Usize, span);
  case TokenType::KW_STRING:
    advance();
    return Spanned<ast::PrimitiveType>(ast::PrimitiveType::String, span);
  default:
    return std::nullopt;
  }
}

std::optional<Spanned<ast::InterfaceTypeAnnot>> Parser::parse_interface_type() {
  size_t start = current_pos;
  consume(TokenType::KW_INTERFACE, "'interface'");
  if (has_errors())
    return std::nullopt;

  ast::InterfaceTypeAnnot iface;
  auto name_tok = consume(TokenType::IDENT, "interface name");
  if (has_errors())
    return std::nullopt;
  iface.names.push_back(name_tok.lexeme);

  while (match(TokenType::PLUS)) {
    auto next_name = consume(TokenType::IDENT, "interface name");
    if (has_errors())
      return std::nullopt;
    iface.names.push_back(next_name.lexeme);
  }

  iface.span = get_span_for(start);
  return Spanned<ast::InterfaceTypeAnnot>(std::move(iface), iface.span);
}

std::optional<Spanned<ast::TypeAnnot>> Parser::parse_named_type() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "type name");
  if (has_errors())
    return std::nullopt;

  ast::NamedTypeAnnot named;
  named.name = name_tok.lexeme;

  if (match(TokenType::LBRACKET)) {
    while (!check(TokenType::RBRACKET) && !is_at_end()) {
      auto t = parse_type();
      if (!t)
        return std::nullopt;
      named.generic_args.push_back(
          std::make_unique<ast::TypeAnnot>(std::move(t->value)));
      if (!match(TokenType::COMMA))
        break;
    }
    consume(TokenType::RBRACKET, "']'");
    if (has_errors())
      return std::nullopt;
  }

  named.span = get_span_for(start);
  ast::TypeAnnot ta;
  ta.span = named.span;
  ta.value = std::move(named);
  return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
}

std::optional<Spanned<ast::TypeAnnot>> Parser::parse_type() {
  return parse_type_with_prefix_operators();
}

std::optional<Spanned<ast::TypeAnnot>>
Parser::parse_type_with_prefix_operators() {
  size_t start = current_pos;

  // pointer type: *T
  if (match(TokenType::STAR)) {
    auto inner = parse_type();
    if (!inner)
      return std::nullopt;
    ast::PointerTypeAnnot ptr;
    ptr.inner = std::make_unique<ast::TypeAnnot>(std::move(inner->value));
    ptr.span = get_span_for(start);
    ast::TypeAnnot ta;
    ta.span = ptr.span;
    ta.value = std::move(ptr);
    return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
  }

  // slice type: []T  or  array type: [N]T
  if (match(TokenType::LBRACKET)) {
    if (match(TokenType::RBRACKET)) {
      // slice: []T
      auto inner = parse_type();
      if (!inner)
        return std::nullopt;
      ast::SliceTypeAnnot slice;
      slice.inner = std::make_unique<ast::TypeAnnot>(std::move(inner->value));
      slice.span = get_span_for(start);
      ast::TypeAnnot ta;
      ta.span = slice.span;
      ta.value = std::move(slice);
      return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
    }
    // array: [N]T
    auto size_expr = parse_expression();
    if (!size_expr)
      return std::nullopt;
    consume(TokenType::RBRACKET, "']'");
    if (has_errors())
      return std::nullopt;
    auto inner = parse_type();
    if (!inner)
      return std::nullopt;
    ast::ArrayTypeAnnot arr;
    arr.size = std::make_unique<ast::Expr>(std::move(size_expr->value));
    arr.inner = std::make_unique<ast::TypeAnnot>(std::move(inner->value));
    arr.span = get_span_for(start);
    ast::TypeAnnot ta;
    ta.span = arr.span;
    ta.value = std::move(arr);
    return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
  }

  // optional type: ?T
  if (match(TokenType::QUESTION)) {
    auto inner = parse_type();
    if (!inner)
      return std::nullopt;
    ast::OptionalTypeAnnot opt;
    opt.inner = std::make_unique<ast::TypeAnnot>(std::move(inner->value));
    opt.span = get_span_for(start);
    ast::TypeAnnot ta;
    ta.span = opt.span;
    ta.value = std::move(opt);
    return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
  }

  // interface type
  if (check(TokenType::KW_INTERFACE)) {
    auto iface = parse_interface_type();
    if (!iface)
      return std::nullopt;
    ast::TypeAnnot ta;
    ta.span = iface->span;
    ta.value = std::move(iface->value);
    return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
  }

  // primitive type
  if (is_primitive_keyword(current().type)) {
    auto prim = parse_primitive_type();
    if (!prim)
      return std::nullopt;
    ast::TypeAnnot ta;
    ta.span = prim->span;
    ta.value = prim->value;
    return Spanned<ast::TypeAnnot>(std::move(ta), ta.span);
  }

  // named type (includes generics)
  if (check(TokenType::IDENT)) {
    return parse_named_type();
  }

  report_error_at_current("expected type");
  return std::nullopt;
}

// Helper parsers

std::optional<ast::Param> Parser::parse_param() {
  size_t start = current_pos;

  auto name_tok = consume(TokenType::IDENT, "parameter name");
  if (has_errors())
    return std::nullopt;

  // Bare "self" parameter: no colon, no type
  if (name_tok.lexeme == "self" && !check(TokenType::COLON)) {
    ast::Param p;
    p.name = "self";
    p.span = get_span_for(start);
    p.type = nullptr;
    return p;
  }

  consume(TokenType::COLON, "':'");
  if (has_errors())
    return std::nullopt;

  auto t = parse_type();
  if (!t)
    return std::nullopt;

  ast::Param p;
  p.name = name_tok.lexeme;
  p.type = std::make_unique<ast::TypeAnnot>(std::move(t->value));
  p.span = get_span_for(start);
  return p;
}

std::optional<ast::GenericParam> Parser::parse_generic_param() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "generic parameter name");
  if (has_errors())
    return std::nullopt;

  ast::GenericParam gp;
  gp.name = name_tok.lexeme;
  gp.span = get_span_for(start);
  return gp;
}

std::optional<ast::WhereConstraint> Parser::parse_where_constraint() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "type parameter name");
  if (has_errors())
    return std::nullopt;

  consume(TokenType::COLON, "':'");
  if (has_errors())
    return std::nullopt;

  auto iface = parse_interface_type();
  if (!iface)
    return std::nullopt;

  ast::WhereConstraint wc;
  wc.name = name_tok.lexeme;
  wc.iface = std::move(iface->value);
  wc.span = get_span_for(start);
  return wc;
}

std::optional<ast::ExternParam> Parser::parse_extern_param() {
  size_t start = current_pos;
  auto t = parse_type();
  if (!t)
    return std::nullopt;

  ast::ExternParam ep;
  ep.type = std::make_unique<ast::TypeAnnot>(std::move(t->value));
  ep.span = get_span_for(start);
  return ep;
}

std::optional<ast::MacroParam> Parser::parse_macro_param() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "macro parameter name");
  if (has_errors())
    return std::nullopt;

  consume(TokenType::COLON, "':'");
  if (has_errors())
    return std::nullopt;

  ast::MacroParam mp;
  mp.name = name_tok.lexeme;

  auto kind_tok = current();
  if (kind_tok.lexeme == "expr") {
    mp.kind = ast::MacroKind::Expr;
    advance();
  } else if (kind_tok.lexeme == "stmt") {
    mp.kind = ast::MacroKind::Stmt;
    advance();
  } else if (kind_tok.lexeme == "type") {
    mp.kind = ast::MacroKind::Type;
    advance();
  } else if (kind_tok.lexeme == "ident") {
    mp.kind = ast::MacroKind::Ident;
    advance();
  } else {
    report_error_at_current(
        "expected macro parameter kind (expr, stmt, type, ident)");
    return std::nullopt;
  }

  mp.span = get_span_for(start);
  return mp;
}

std::optional<ast::InterfaceMethod> Parser::parse_interface_method() {
  size_t start = current_pos;
  consume(TokenType::KW_FN, "'fn'");
  if (has_errors())
    return std::nullopt;

  auto name_tok = consume(TokenType::IDENT, "method name");
  if (has_errors())
    return std::nullopt;

  ast::InterfaceMethod method;
  method.name = name_tok.lexeme;

  consume(TokenType::LPAREN, "'('");
  if (has_errors())
    return std::nullopt;

  if (!check(TokenType::RPAREN)) {
    while (!is_at_end()) {
      auto p = parse_param();
      if (!p)
        return std::nullopt;
      method.params.push_back(std::move(*p));
      if (!match(TokenType::COMMA))
        break;
    }
  }

  consume(TokenType::RPAREN, "')'");
  if (has_errors())
    return std::nullopt;

  if (match(TokenType::ARROW)) {
    auto rt = parse_type();
    if (!rt)
      return std::nullopt;
    method.return_type = std::make_unique<ast::TypeAnnot>(std::move(rt->value));
  }

  method.span = get_span_for(start);
  return method;
}

std::optional<ast::FieldDecl> Parser::parse_field_decl() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "field name");
  if (has_errors())
    return std::nullopt;

  consume(TokenType::COLON, "':'");
  if (has_errors())
    return std::nullopt;

  auto t = parse_type();
  if (!t)
    return std::nullopt;

  consume(TokenType::COMMA, "','");
  if (has_errors())
    return std::nullopt;

  ast::FieldDecl fd;
  fd.name = name_tok.lexeme;
  fd.type = std::make_unique<ast::TypeAnnot>(std::move(t->value));
  fd.span = get_span_for(start);
  return fd;
}

std::optional<ast::UnionVariant> Parser::parse_variant_decl() {
  size_t start = current_pos;
  auto name_tok = consume(TokenType::IDENT, "variant name");
  if (has_errors())
    return std::nullopt;

  ast::UnionVariant v;
  v.name = name_tok.lexeme;

  if (match(TokenType::COLON)) {
    auto t = parse_type();
    if (!t)
      return std::nullopt;
    v.type = std::make_unique<ast::TypeAnnot>(std::move(t->value));
  }

  consume(TokenType::COMMA, "','");
  if (has_errors())
    return std::nullopt;

  v.span = get_span_for(start);
  return v;
}

} // namespace shikimori
