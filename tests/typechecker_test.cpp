#include "parser/parser.h"
#include "typechecker/typechecker.h"
#include <doctest/doctest.h>
#include <iostream>

using namespace shikimori;

TEST_CASE("Typechecker: Resolve primitive types") {
  Typechecker tc;

  Tokenizer tokenizer("i32");
  auto tokens = tokenizer.tokenize();
  Parser parser(tokens, "i32");
  CHECK_FALSE(parser.has_errors());
  auto type_annot = parser.parse_type();
  REQUIRE_MESSAGE(type_annot.has_value(), "parse_type failed");
  CHECK_FALSE(parser.has_errors());

  auto resolved = tc.resolve_type(type_annot->value);
  REQUIRE(resolved != nullptr);

  auto *named = std::get_if<typed::TyNamed>(&resolved->ty);
  REQUIRE(named != nullptr);
  CHECK(named->name == "i32");
  CHECK(named->kind == typed::Primitive);
}

TEST_CASE("Typechecker: Resolve pointer type") {
  Typechecker tc;

  Tokenizer tokenizer("*i32");
  auto tokens = tokenizer.tokenize();
  Parser parser(tokens, "*i32");
  auto type_annot = parser.parse_type();
  REQUIRE(type_annot.has_value());

  auto resolved = tc.resolve_type(type_annot->value);
  REQUIRE(resolved != nullptr);

  auto *named = std::get_if<typed::TyNamed>(&resolved->ty);
  REQUIRE(named != nullptr);
  CHECK(named->name == "ptr");
  CHECK(named->kind == typed::Pointer);
  REQUIRE(named->args.size() == 1);
  auto *inner = std::get_if<typed::TyNamed>(&named->args[0]->ty);
  CHECK(inner->name == "i32");
}

TEST_CASE("Typechecker: Resolve slice type") {
  Typechecker tc;

  Tokenizer tokenizer("[]i32");
  auto tokens = tokenizer.tokenize();
  Parser parser(tokens, "[]i32");
  auto type_annot = parser.parse_type();
  REQUIRE(type_annot.has_value());

  auto resolved = tc.resolve_type(type_annot->value);
  REQUIRE(resolved != nullptr);

  auto *named = std::get_if<typed::TyNamed>(&resolved->ty);
  REQUIRE(named != nullptr);
  CHECK(named->name == "slice");
  CHECK(named->kind == typed::Slice);
  REQUIRE(named->args.size() == 1);
  auto *inner = std::get_if<typed::TyNamed>(&named->args[0]->ty);
  CHECK(inner->name == "i32");
}

TEST_CASE("Typechecker: Resolve optional type") {
  Typechecker tc;

  Tokenizer tokenizer("?i32");
  auto tokens = tokenizer.tokenize();
  Parser parser(tokens, "?i32");
  auto type_annot = parser.parse_type();
  REQUIRE(type_annot.has_value());

  auto resolved = tc.resolve_type(type_annot->value);
  REQUIRE(resolved != nullptr);

  auto *named = std::get_if<typed::TyNamed>(&resolved->ty);
  REQUIRE(named != nullptr);
  CHECK(named->name == "Option");
  CHECK(named->kind == typed::Union);
  REQUIRE(named->args.size() == 1);
  auto *inner = std::get_if<typed::TyNamed>(&named->args[0]->ty);
  CHECK(inner->name == "i32");
}

TEST_CASE("Typechecker: Unknown type throws") {
  Typechecker tc;

  Tokenizer tokenizer("UnknownType");
  Parser parser(tokenizer.tokenize(), "UnknownType");
  auto type_annot = parser.parse_type();
  REQUIRE(type_annot.has_value());

  CHECK_THROWS_AS(tc.resolve_type(type_annot->value), TypeError);
}

TEST_CASE("Typechecker: Unknown type in fn signature throws") {
  Typechecker tc;

  Tokenizer tok("fn foo(x: UnknownType) {}");
  Parser fn_parser(tok.tokenize(), "fn foo(x: UnknownType) {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  CHECK_THROWS_AS(tc.collect_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Generic struct Map[K, V] -> ForAll") {
  Typechecker tc;

  Tokenizer tok("struct Map[K, V] { key: K, value: V, }");
  Parser struct_parser(tok.tokenize(),
                       "struct Map[K, V] { key: K, value: V, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  auto it = tc.structs.find("Map");
  REQUIRE(it != tc.structs.end());
  REQUIRE(it->second.scheme.has_value());

  auto &forall = *it->second.scheme;

  CHECK(forall.vars.size() == 2);
  CHECK(forall.vars[0].first == "K");
  CHECK(forall.vars[0].second == 0);
  CHECK(forall.vars[1].first == "V");
  CHECK(forall.vars[1].second == 1);

  auto *body = std::get_if<typed::TyNamed>(&forall.body->ty);
  REQUIRE(body != nullptr);
  CHECK(body->name == "Map");
  CHECK(body->kind == typed::Struct);
  CHECK(body->args.size() == 2);

  auto *arg0 = std::get_if<typed::TyVar>(&body->args[0]->ty);
  REQUIRE(arg0 != nullptr);
  CHECK(arg0->id == 0);
  CHECK(arg0->name == "K");

  auto *arg1 = std::get_if<typed::TyVar>(&body->args[1]->ty);
  REQUIRE(arg1 != nullptr);
  CHECK(arg1->id == 1);
  CHECK(arg1->name == "V");
}

TEST_CASE("Typechecker: Generic union Result[T, E] -> ForAll") {
  Typechecker tc;

  Tokenizer tok("union Result[T, E] { Ok: T, Err: E, }");
  Parser union_parser(tok.tokenize(), "union Result[T, E] { Ok: T, Err: E, }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  auto it = tc.unions.find("Result");
  REQUIRE(it != tc.unions.end());
  REQUIRE(it->second.scheme.has_value());

  auto &forall = *it->second.scheme;

  CHECK(forall.vars.size() == 2);
  CHECK(forall.vars[0].first == "T");
  CHECK(forall.vars[0].second == 0);
  CHECK(forall.vars[1].first == "E");
  CHECK(forall.vars[1].second == 1);

  auto *body = std::get_if<typed::TyNamed>(&forall.body->ty);
  REQUIRE(body != nullptr);
  CHECK(body->name == "Result");
  CHECK(body->kind == typed::Union);
}

TEST_CASE("Typechecker: Generic fn zip[A, B] -> ForAll") {
  Typechecker tc;

  Tokenizer pair_tok("struct Pair[A, B] { first: A, second: B, }");
  Parser pair_parser(pair_tok.tokenize(),
                     "struct Pair[A, B] { first: A, second: B, }");
  auto pair_decl = pair_parser.parse_struct_decl();
  REQUIRE(pair_decl.has_value());
  tc.collect_struct(*pair_decl);

  Tokenizer tok("fn zip[A, B](a: A, b: B) -> Pair[A, B] {}");
  Parser fn_parser(tok.tokenize(), "fn zip[A, B](a: A, b: B) -> Pair[A, B] {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  tc.collect_fn(*fn_decl);

  auto it = tc.functions.find("zip");
  REQUIRE(it != tc.functions.end());

  auto *forall = std::get_if<typed::ForAll>(&it->second->ty);
  REQUIRE(forall != nullptr);

  CHECK(forall->vars.size() == 2);
  CHECK(forall->vars[0].first == "A");
  CHECK(forall->vars[0].second == 0);
  CHECK(forall->vars[1].first == "B");
  CHECK(forall->vars[1].second == 1);

  auto *fnty = std::get_if<typed::FnTy>(&forall->body->ty);
  REQUIRE(fnty != nullptr);

  CHECK(fnty->args.size() == 2);
  auto *arg0 = std::get_if<typed::TyVar>(&fnty->args[0]->ty);
  REQUIRE(arg0 != nullptr);
  CHECK(arg0->id == 0);
  CHECK(arg0->name == "A");

  auto *arg1 = std::get_if<typed::TyVar>(&fnty->args[1]->ty);
  REQUIRE(arg1 != nullptr);
  CHECK(arg1->id == 1);
  CHECK(arg1->name == "B");

  auto *ret = std::get_if<typed::TyNamed>(&fnty->return_type->ty);
  REQUIRE(ret != nullptr);
  CHECK(ret->name == "Pair");
  CHECK(ret->kind == typed::Struct);
  CHECK(ret->args.size() == 2);

  auto *ret_arg0 = std::get_if<typed::TyVar>(&ret->args[0]->ty);
  REQUIRE(ret_arg0 != nullptr);
  CHECK(ret_arg0->id == 0);
  CHECK(ret_arg0->name == "A");

  auto *ret_arg1 = std::get_if<typed::TyVar>(&ret->args[1]->ty);
  REQUIRE(ret_arg1 != nullptr);
  CHECK(ret_arg1->id == 1);
  CHECK(ret_arg1->name == "B");
}
