#include "parser/parser.h"
#include "typechecker/typechecker.h"
#include <doctest/doctest.h>

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
  CHECK(named->name == "Slice");
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

  auto *forall = std::get_if<typed::ForAll>(&it->second.ty->ty);
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

TEST_CASE("Typechecker: Interface method signature matching [Success]") {
  Typechecker tc;

  Tokenizer iface_tok("interface Drawable { fn draw(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Drawable { fn draw(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Circle { fn draw(self) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Circle { fn draw(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should not throw. Circle satisfies Drawable
  CHECK_NOTHROW(tc.check_satisfies("Circle", "Drawable", Span{}));
}

TEST_CASE("Typechecker: Interface method signature matching [Missing method]") {
  Typechecker tc;

  Tokenizer iface_tok("interface Drawable { fn draw(self) fn bounds(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Drawable { fn draw(self) fn bounds(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Circle { fn draw(self) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Circle { fn draw(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should throw. Circle missing bounds method
  CHECK_THROWS_AS(tc.check_satisfies("Circle", "Drawable", Span{}), TypeError);
}

TEST_CASE(
    "Typechecker: Interface method signature matching [Wrong return type]") {
  Typechecker tc;

  Tokenizer iface_tok("interface Getter { fn get(self) -> i32 }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Getter { fn get(self) -> i32 }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Bad { fn get(self) -> f32 {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Bad { fn get(self) -> f32 {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should throw. Return type mismatch
  CHECK_THROWS_AS(tc.check_satisfies("Bad", "Getter", Span{}), TypeError);
}

TEST_CASE(
    "Typechecker: Interface method signature matching [Wrong param type]") {
  Typechecker tc;

  Tokenizer iface_tok("interface Processor { fn process(self, x: i32) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Processor { fn process(self, x: i32) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Bad { fn process(self, x: f32) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Bad { fn process(self, x: f32) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should throw. Parameter type mismatch
  CHECK_THROWS_AS(tc.check_satisfies("Bad", "Processor", Span{}), TypeError);
}

TEST_CASE("Typechecker: Interface method signature matching. Correct") {
  Typechecker tc;

  Tokenizer iface_tok(
      "interface Adder { fn add(self, a: i32, b: i32) -> i32 }");
  Parser iface_parser(
      iface_tok.tokenize(),
      "interface Adder { fn add(self, a: i32, b: i32) -> i32 }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok(
      "struct Calculator { fn add(self, a: i32, b: i32) -> i32 {} }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Calculator { fn add(self, a: i32, b: i32) -> i32 {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should not throw. Signatures match
  CHECK_NOTHROW(tc.check_satisfies("Calculator", "Adder", Span{}));
}

TEST_CASE("Typechecker: Interface. Union satisfies interface") {
  Typechecker tc;

  Tokenizer iface_tok("interface Displayable { fn show(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Displayable { fn show(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer union_tok("union Shape { circle: f32, fn show(self) {} }");
  Parser union_parser(union_tok.tokenize(),
                      "union Shape { circle: f32, fn show(self) {} }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Should not throw. Shape satisfies Displayable
  CHECK_NOTHROW(tc.check_satisfies("Shape", "Displayable", Span{}));
}

TEST_CASE("Typechecker: Interface. Union missing method") {
  Typechecker tc;

  Tokenizer iface_tok("interface Displayable { fn show(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Displayable { fn show(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer union_tok("union Shape { circle: f32, }");
  Parser union_parser(union_tok.tokenize(), "union Shape { circle: f32, }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Should throw. Shape missing show method
  CHECK_THROWS_AS(tc.check_satisfies("Shape", "Displayable", Span{}),
                  TypeError);
}

TEST_CASE("Typechecker: Interface. Multiple methods all required") {
  Typechecker tc;

  Tokenizer iface_tok("interface Full { fn a(self) fn b(self) -> i32 fn "
                      "c(self, x: i32) -> i32 }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Full { fn a(self) fn b(self) -> i32 fn "
                      "c(self, x: i32) -> i32 }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok(
      "struct Partial { fn a(self) {} fn b(self) -> i32 { return 0; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Partial { fn a(self) {} fn b(self) -> i32 { return 0; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should throw. Missing method c
  CHECK_THROWS_AS(tc.check_satisfies("Partial", "Full", Span{}), TypeError);
}

TEST_CASE("Typechecker: Interface. Self type must match") {
  Typechecker tc;

  Tokenizer iface_tok("interface Foo { fn bar(self) }");
  Parser iface_parser(iface_tok.tokenize(), "interface Foo { fn bar(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct MyStruct { fn bar(self) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct MyStruct { fn bar(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should not throw
  CHECK_NOTHROW(tc.check_satisfies("MyStruct", "Foo", Span{}));
}

TEST_CASE("Typechecker: Interface. Method with multiple params") {
  Typechecker tc;

  Tokenizer iface_tok(
      "interface Math { fn compute(self, a: i32, b: i32, c: f32) -> f32 }");
  Parser iface_parser(
      iface_tok.tokenize(),
      "interface Math { fn compute(self, a: i32, b: i32, c: f32) -> f32 }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Calc { fn compute(self, a: i32, b: i32, c: f32) "
                       "-> f32 { return 0.0; } }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Calc { fn compute(self, a: i32, b: i32, c: f32) "
                       "-> f32 { return 0.0; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should not throw
  CHECK_NOTHROW(tc.check_satisfies("Calc", "Math", Span{}));
}

TEST_CASE("Typechecker: Interface. Method with wrong param count") {
  Typechecker tc;

  Tokenizer iface_tok("interface Multi { fn run(self, a: i32, b: i32) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Multi { fn run(self, a: i32, b: i32) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Bad { fn run(self, a: i32) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Bad { fn run(self, a: i32) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should throw. Wrong param count
  CHECK_THROWS_AS(tc.check_satisfies("Bad", "Multi", Span{}), TypeError);
}

TEST_CASE("Typechecker: Interface. Generic struct method") {
  Typechecker tc;

  Tokenizer iface_tok("interface Container { fn get(self) -> i32 }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Container { fn get(self) -> i32 }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok(
      "struct Box[T] { value: T, fn get(self) -> i32 { return 0; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Box[T] { value: T, fn get(self) -> i32 { return 0; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Should not throw
  CHECK_NOTHROW(tc.check_satisfies("Box", "Container", Span{}));
}

TEST_CASE("Typechecker: Where clause. Type satisfies constraint") {
  Typechecker tc;

  Tokenizer iface_tok("interface Printable { fn print(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Printable { fn print(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Foo { fn print(self) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Foo { fn print(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn print_it[T](val: T) where T: interface Printable {}");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn print_it[T](val: T) where T: interface Printable {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  tc.collect_fn(*fn_decl);

  // Should not throw. Foo satisfies Printable
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Where clause. Type does not satisfy constraint") {
  Typechecker tc;

  Tokenizer iface_tok("interface Printable { fn print(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Printable { fn print(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct NoPrint { }");
  Parser struct_parser(struct_tok.tokenize(), "struct NoPrint { }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn print_it[T](val: T) where T: interface Printable {}");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn print_it[T](val: T) where T: interface Printable {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  tc.collect_fn(*fn_decl);

  // The function itself should collect fine
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Where clause. Unknown interface in constraint") {
  Typechecker tc;

  Tokenizer fn_tok("fn foo[T](val: T) where T: interface UnknownInterface {}");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn foo[T](val: T) where T: interface UnknownInterface {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should throw. UnknownInterface doesn't exist
  CHECK_THROWS_AS(tc.collect_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Where clause. Multiple interface constraints") {
  Typechecker tc;

  Tokenizer iface1_tok("interface Printable { fn print(self) }");
  Parser iface1_parser(iface1_tok.tokenize(),
                       "interface Printable { fn print(self) }");
  auto iface1_decl = iface1_parser.parse_interface_decl();
  REQUIRE(iface1_decl.has_value());
  tc.collect_interface(*iface1_decl);

  Tokenizer iface2_tok("interface Serializable { fn serialize(self) }");
  Parser iface2_parser(iface2_tok.tokenize(),
                       "interface Serializable { fn serialize(self) }");
  auto iface2_decl = iface2_parser.parse_interface_decl();
  REQUIRE(iface2_decl.has_value());
  tc.collect_interface(*iface2_decl);

  Tokenizer struct_tok(
      "struct Both { fn print(self) {} fn serialize(self) {} }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Both { fn print(self) {} fn serialize(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn process[T](val: T) where T: interface Printable, T: "
                   "interface Serializable {}");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn process[T](val: T) where T: interface Printable, T: "
                   "interface Serializable {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw
  CHECK_NOTHROW(tc.collect_fn(*fn_decl));
}

TEST_CASE("Typechecker: Where clause enforcement at call site [Success]") {
  Typechecker tc;

  Tokenizer iface_tok("interface Printable { fn print(self) }");
  Parser iface_parser(iface_tok.tokenize(),
                      "interface Printable { fn print(self) }");
  auto iface_decl = iface_parser.parse_interface_decl();
  REQUIRE(iface_decl.has_value());
  tc.collect_interface(*iface_decl);

  Tokenizer struct_tok("struct Foo { fn print(self) {} }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Foo { fn print(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn print_it[T](val: T) where T: interface Printable {}");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn print_it[T](val: T) where T: interface Printable {}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  tc.collect_fn(*fn_decl);

  // Create a program and typecheck it
  ast::Program program;
  program.span = Span{};
  program.declarations.push_back(ast::Decl{fn_decl->span, std::move(*fn_decl)});

  typed::TypedProgram result = tc.run(program);

  // Should succeed
  CHECK(result.declarations.size() == 1);
}

TEST_CASE(
    "Typechecker: Generic instantiation. Multiple struct instantiations") {
  Typechecker tc;

  Tokenizer struct_tok("struct Box[T] { value: T, }");
  Parser struct_parser(struct_tok.tokenize(), "struct Box[T] { value: T, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn main() { let a = Box[i32] { value: 1, }; let b = "
                   "Box[string] { value: \"hi\", }; }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn main() { let a = Box[i32] { value: 1, }; let b = "
                   "Box[string] { value: \"hi\", }; }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Each instantiation is independent
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE(
    "Typechecker: Generic instantiation. Multiple function instantiations") {
  Typechecker tc;

  Tokenizer fn_tok("fn id[T](x: T) -> T { return x; }");
  Parser fn_parser(fn_tok.tokenize(), "fn id[T](x: T) -> T { return x; }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  tc.collect_fn(*fn_decl);

  Tokenizer main_tok("fn main() { let x = id(1); let y = id(false); }");
  Parser main_parser(main_tok.tokenize(),
                     "fn main() { let x = id(1); let y = id(false); }");
  auto main_decl = main_parser.parse_fn_decl();
  REQUIRE(main_decl.has_value());

  // Should not throw. Each call instantiates independently
  CHECK_NOTHROW(tc.check_fn(*main_decl));
}

TEST_CASE("Typechecker: Self parameter is reference (pointer)") {
  Typechecker tc;

  Tokenizer struct_tok("struct Foo { fn bar(self) {} }");
  Parser struct_parser(struct_tok.tokenize(), "struct Foo { fn bar(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Check that the method's self parameter is a pointer type
  auto it = tc.functions.find("Foo.bar");
  REQUIRE(it != tc.functions.end());

  auto fn_ty = std::get_if<typed::FnTy>(&it->second.ty->ty);
  REQUIRE(fn_ty != nullptr);
  REQUIRE(fn_ty->args.size() >= 1);

  // Self should be a pointer type
  auto self_ty = std::get_if<typed::TyNamed>(&fn_ty->args[0]->ty);
  REQUIRE(self_ty != nullptr);
  CHECK(self_ty->name == "ptr");
  CHECK(self_ty->kind == typed::Pointer);
}

TEST_CASE("Typechecker: Method call with self as reference") {
  Typechecker tc;

  Tokenizer struct_tok("struct Foo { fn bar(self) {} }");
  Parser struct_parser(struct_tok.tokenize(), "struct Foo { fn bar(self) {} }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn test(f: Foo) { f.bar(); }");
  Parser fn_parser(fn_tok.tokenize(), "fn test(f: Foo) { f.bar(); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Method call handles self reference correctly
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Generic method return type. swap() on Pair[A, B]") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Verify the method was collected with correct type
  auto it = tc.functions.find("Pair.swap");
  REQUIRE(it != tc.functions.end());

  // The method should have a ForAll type with vars [("A", 0), ("B", 1)]
  auto *forall = std::get_if<typed::ForAll>(&it->second.ty->ty);
  REQUIRE(forall != nullptr);
  REQUIRE(forall->vars.size() == 2);
  CHECK(forall->vars[0].first == "A");
  CHECK(forall->vars[1].first == "B");

  Tokenizer fn_tok("fn test() { let p = Pair[i32, string] { first: 42, second: "
                   "\"hello\", }; let swapped = p.swap(); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() { let p = Pair[i32, string] { first: 42, second: "
                   "\"hello\", }; let swapped = p.swap(); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. swap() should return Pair[string, i32]
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Generic method return type. Field access after swap") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok(
      "fn test() { "
      "let p = Pair[i32, string] { first: 42, second: \"hello\", }; "
      "let swapped = p.swap(); "
      "let x: string = swapped.first; "
      "let y: i32 = swapped.second; "
      "}");
  Parser fn_parser(
      fn_tok.tokenize(),
      "fn test() { "
      "let p = Pair[i32, string] { first: 42, second: \"hello\", }; "
      "let swapped = p.swap(); "
      "let x: string = swapped.first; "
      "let y: i32 = swapped.second; "
      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Types should match after swap
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Generic method return type. Wrong type after swap") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Pair[A, B] { first: A, second: B, fn swap(self) -> Pair[B, A] { "
      "return Pair[B, A] { first: self.second, second: self.first, }; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok(
      "fn test() { "
      "let p = Pair[i32, string] { first: 42, second: \"hello\", }; "
      "let swapped = p.swap(); "
      "let x: i32 = swapped.first; "
      "}");
  Parser fn_parser(
      fn_tok.tokenize(),
      "fn test() { "
      "let p = Pair[i32, string] { first: 42, second: \"hello\", }; "
      "let swapped = p.swap(); "
      "let x: i32 = swapped.first; "
      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should throw. swapped.first is string, not i32
  CHECK_THROWS_AS(tc.check_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Generic method with own type params") {
  Typechecker tc;

  Tokenizer pair_tok("struct Pair[A, B] { first: A, second: B, }");
  Parser pair_parser(pair_tok.tokenize(),
                     "struct Pair[A, B] { first: A, second: B, }");
  auto pair_decl = pair_parser.parse_struct_decl();
  REQUIRE(pair_decl.has_value());
  tc.collect_struct(*pair_decl);

  Tokenizer struct_tok(
      "struct Container[T] { value: T, fn convert[U](self, x: U) -> Pair[T, U] "
      "{ return Pair[T, U] { first: self.value, second: x, }; } }");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Container[T] { value: T, fn convert[U](self, x: U) -> Pair[T, U] "
      "{ return Pair[T, U] { first: self.value, second: x, }; } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Verify the method was collected
  auto it = tc.functions.find("Container.convert");
  REQUIRE(it != tc.functions.end());

  // The method should have a ForAll type with vars [("T", 0), ("U", 1)]
  auto *forall = std::get_if<typed::ForAll>(&it->second.ty->ty);
  REQUIRE(forall != nullptr);
  REQUIRE(forall->vars.size() == 2);
  CHECK(forall->vars[0].first == "T");
  CHECK(forall->vars[1].first == "U");

  Tokenizer fn_tok("fn test() { "
                   "let c = Container[i32] { value: 42, }; "
                   "let p = c.convert(\"hello\"); "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test() { "
                                      "let c = Container[i32] { value: 42, }; "
                                      "let p = c.convert(\"hello\"); "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Generic method return type [swap]") {
  Typechecker tc;

  Tokenizer struct_tok("struct Pair[A, B] { first: A, second: B, fn swap(self) "
                       "-> Pair[B, A] { } }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Pair[A, B] { first: A, second: B, fn swap(self) "
                       "-> Pair[B, A] { } }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn test() { let p = Pair[i32, f32] { first: 1, second: "
                   "2.0, }; let swapped = p.swap(); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() { let p = Pair[i32, f32] { first: 1, second: "
                   "2.0, }; let swapped = p.swap(); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. swap() should return Pair[f32, i32]
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Match literal patterns [int]") {
  Typechecker tc;

  Tokenizer union_tok("union Result { ok: i32, err: string, }");
  Parser union_parser(union_tok.tokenize(),
                      "union Result { ok: i32, err: string, }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Match with int literal patterns
  Tokenizer fn_tok("fn test(code: i32) -> string { "
                   "match code { "
                   "  0 => { return \"zero\"; }, "
                   "  42 => { return \"answer\"; }, "
                   "  _ => { return \"other\"; }, "
                   "} "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test(code: i32) -> string { "
                                      "match code { "
                                      "  0 => { return \"zero\"; }, "
                                      "  42 => { return \"answer\"; }, "
                                      "  _ => { return \"other\"; }, "
                                      "} "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. int literals match i32 subject
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Match literal patterns [bool]") {
  Typechecker tc;

  // Match with bool literal patterns
  Tokenizer fn_tok("fn test(flag: bool) -> string { "
                   "match flag { "
                   "  true => { return \"yes\"; }, "
                   "  false => { return \"no\"; }, "
                   "} "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test(flag: bool) -> string { "
                                      "match flag { "
                                      "  true => { return \"yes\"; }, "
                                      "  false => { return \"no\"; }, "
                                      "} "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. bool literals match bool subject
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Match literal patterns. type mismatch") {
  Typechecker tc;

  // Match with wrong literal type (string literal for i32 subject)
  Tokenizer fn_tok("fn test(code: i32) -> string { "
                   "match code { "
                   "  \"hello\" => { return \"string\"; }, "
                   "  _ => { return \"other\"; }, "
                   "} "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test(code: i32) -> string { "
                                      "match code { "
                                      "  \"hello\" => { return \"string\"; }, "
                                      "  _ => { return \"other\"; }, "
                                      "} "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should throw. string literal doesn't match i32 subject
  CHECK_THROWS_AS(tc.check_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Scope access. union variant init") {
  Typechecker tc;

  Tokenizer union_tok("union Result { ok: i32, err: string, }");
  Parser union_parser(union_tok.tokenize(),
                      "union Result { ok: i32, err: string, }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  Tokenizer fn_tok1("fn test1() -> Result { return Result::ok(42); }");
  Parser fn_parser1(fn_tok1.tokenize(),
                    "fn test1() -> Result { return Result::ok(42); }");
  auto fn_decl1 = fn_parser1.parse_fn_decl();
  REQUIRE(fn_decl1.has_value());
  CHECK_NOTHROW(tc.check_fn(*fn_decl1));

  Tokenizer fn_tok2("fn test2() -> Result { return Result::err(\"oops\"); }");
  Parser fn_parser2(fn_tok2.tokenize(),
                    "fn test2() -> Result { return Result::err(\"oops\"); }");
  auto fn_decl2 = fn_parser2.parse_fn_decl();
  REQUIRE(fn_decl2.has_value());
  CHECK_NOTHROW(tc.check_fn(*fn_decl2));
}

TEST_CASE("Typechecker: Scope access. union with static method") {
  Typechecker tc;

  Tokenizer union_tok(
      "union Result { "
      "  ok: i32, "
      "  err: string, "
      "  fn new_ok(val: i32) -> Result { return Result::ok(val); } "
      "}");
  Parser union_parser(
      union_tok.tokenize(),
      "union Result { "
      "  ok: i32, "
      "  err: string, "
      "  fn new_ok(val: i32) -> Result { return Result::ok(val); } "
      "}");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Test calling static method on union (this was the bug!)
  Tokenizer fn_tok("fn test() -> Result { return Result::new_ok(42); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> Result { return Result::new_ok(42); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. static method on union should work
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Scope access. struct static method") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Foo { "
      "  value: i32, "
      "  fn new(val: i32) -> Foo { return Foo { value: val, }; } "
      "}");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Foo { "
      "  value: i32, "
      "  fn new(val: i32) -> Foo { return Foo { value: val, }; } "
      "}");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Test calling static method
  Tokenizer fn_tok("fn test() -> Foo { return Foo::new(42); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> Foo { return Foo::new(42); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Scope access. Instance method with explicit self") {
  Typechecker tc;

  Tokenizer struct_tok("struct Foo { "
                       "  value: i32, "
                       "  fn get_value(self) -> i32 { return self.value; } "
                       "}");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Foo { "
                       "  value: i32, "
                       "  fn get_value(self) -> i32 { return self.value; } "
                       "}");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Instance method with explicit self should work (auto & taken)
  Tokenizer fn_tok("fn test(f: Foo) -> i32 { return Foo::get_value(f); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test(f: Foo) -> i32 { return Foo::get_value(f); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Instance method can be called with :: and explicit self
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE(
    "Typechecker: Scope access. Union instance method with explicit self") {
  Typechecker tc;

  Tokenizer union_tok(
      "union Shape { "
      "  circle: f32, "
      "  fn area(self) -> f32 { return 3.14 * self.circle * self.circle; } "
      "}");
  Parser union_parser(
      union_tok.tokenize(),
      "union Shape { "
      "  circle: f32, "
      "  fn area(self) -> f32 { return 3.14 * self.circle * self.circle; } "
      "}");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Instance method with explicit self should work (auto & taken)
  Tokenizer fn_tok("fn test(s: Shape) -> f32 { return Shape::area(s); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test(s: Shape) -> f32 { return Shape::area(s); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Instance method can be called with :: and explicit self
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Scope access. Static and instance methods both work") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Rectangle { "
      "  width: i32, "
      "  height: i32, "
      "  fn new(w: i32, h: i32) -> Rectangle { return Rectangle { width: w, "
      "height: h, }; } "
      "  fn area(self) -> i32 { return self.width * self.height; } "
      "}");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Rectangle { "
      "  width: i32, "
      "  height: i32, "
      "  fn new(w: i32, h: i32) -> Rectangle { return Rectangle { width: w, "
      "height: h, }; } "
      "  fn area(self) -> i32 { return self.width * self.height; } "
      "}");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Static method should work
  Tokenizer static_fn_tok(
      "fn test_static() -> Rectangle { return Rectangle::new(10, 20); }");
  Parser static_fn_parser(
      static_fn_tok.tokenize(),
      "fn test_static() -> Rectangle { return Rectangle::new(10, 20); }");
  auto static_fn_decl = static_fn_parser.parse_fn_decl();
  REQUIRE(static_fn_decl.has_value());
  CHECK_NOTHROW(tc.check_fn(*static_fn_decl));

  // Instance method with explicit self should also work (auto & taken)
  Tokenizer instance_fn_tok(
      "fn test_instance(r: Rectangle) -> i32 { return Rectangle::area(r); }");
  Parser instance_fn_parser(
      instance_fn_tok.tokenize(),
      "fn test_instance(r: Rectangle) -> i32 { return Rectangle::area(r); }");
  auto instance_fn_decl = instance_fn_parser.parse_fn_decl();
  REQUIRE(instance_fn_decl.has_value());
  CHECK_NOTHROW(tc.check_fn(*instance_fn_decl));
}

TEST_CASE(
    "Typechecker: Method call syntax. Instance method with dot notation") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Rectangle { "
      "  width: i32, "
      "  height: i32, "
      "  fn area(self) -> i32 { return self.width * self.height; } "
      "}");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Rectangle { "
      "  width: i32, "
      "  height: i32, "
      "  fn area(self) -> i32 { return self.width * self.height; } "
      "}");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Method call syntax should work
  Tokenizer fn_tok("fn test(r: Rectangle) -> i32 { return r.area(); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test(r: Rectangle) -> i32 { return r.area(); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Method with self and extra params") {
  Typechecker tc;

  Tokenizer struct_tok(
      "struct Calculator { "
      "  fn add(self, a: i32, b: i32) -> i32 { return a + b; } "
      "}");
  Parser struct_parser(
      struct_tok.tokenize(),
      "struct Calculator { "
      "  fn add(self, a: i32, b: i32) -> i32 { return a + b; } "
      "}");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Call with explicit self (auto & taken) and extra params
  Tokenizer fn_tok(
      "fn test(c: Calculator) -> i32 { return Calculator::add(c, 1, 2); }");
  Parser fn_parser(
      fn_tok.tokenize(),
      "fn test(c: Calculator) -> i32 { return Calculator::add(c, 1, 2); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());
  CHECK_NOTHROW(tc.check_fn(*fn_decl));

  // Call with method syntax
  Tokenizer fn_tok2("fn test2(c: Calculator) -> i32 { return c.add(1, 2); }");
  Parser fn_parser2(fn_tok2.tokenize(),
                    "fn test2(c: Calculator) -> i32 { return c.add(1, 2); }");
  auto fn_decl2 = fn_parser2.parse_fn_decl();
  REQUIRE(fn_decl2.has_value());
  CHECK_NOTHROW(tc.check_fn(*fn_decl2));
}

TEST_CASE("Typechecker: Scope access. Unknown variant error") {
  Typechecker tc;

  Tokenizer union_tok("union Result { ok: i32, err: string, }");
  Parser union_parser(union_tok.tokenize(),
                      "union Result { ok: i32, err: string, }");
  auto union_decl = union_parser.parse_union_decl();
  REQUIRE(union_decl.has_value());
  tc.collect_union(*union_decl);

  // Test unknown variant. Should give helpful error
  Tokenizer fn_tok("fn test() -> Result { return Result::invalid(42); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> Result { return Result::invalid(42); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should throw with helpful message about not being a variant
  CHECK_THROWS_AS(tc.check_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Built-in functions with type args [@sizeof]") {
  Typechecker tc;

  Tokenizer fn_tok("fn test() -> usize { return @sizeof(i32); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> usize { return @sizeof(i32); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. @sizeof takes a type argument
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Built-in functions with type args [@typeid]") {
  Typechecker tc;

  Tokenizer fn_tok("fn test() -> usize { return @typeid(f32); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> usize { return @typeid(f32); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. @typeid takes a type argument
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Built-in functions with type args [@alignof]") {
  Typechecker tc;

  Tokenizer fn_tok("fn test() -> usize { return @alignof(i64); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> usize { return @alignof(i64); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. @alignof takes a type argument
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Built-in functions with type args [@typename]") {
  Typechecker tc;

  Tokenizer struct_tok("struct Point { x: i32, y: i32, }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Point { x: i32, y: i32, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn test() -> string { return @typename(Point); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> string { return @typename(Point); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. @typename takes a type argument
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Built-in functions with type args [@has_field]") {
  Typechecker tc;

  Tokenizer struct_tok("struct Point { x: i32, y: i32, }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Point { x: i32, y: i32, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  Tokenizer fn_tok("fn test() -> bool { return @has_field(Point, \"x\"); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> bool { return @has_field(Point, \"x\"); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. @has_field takes a type arg and a field name
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Built-in functions. @sizeof missing type arg") {
  Typechecker tc;

  Tokenizer fn_tok("fn test() -> usize { return @sizeof(); }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> usize { return @sizeof(); }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should throw. @sizeof requires a type argument
  CHECK_THROWS_AS(tc.check_fn(*fn_decl), TypeError);
}

TEST_CASE("Typechecker: Range type with type argument") {
  Typechecker tc;

  Tokenizer struct_tok("struct Range[T] { first: T, second: T, }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Range[T] { first: T, second: T, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Range[i32] should work
  Tokenizer fn_tok("fn test() -> Range[i32] { return 0..10; }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> Range[i32] { return 0..10; }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Range[T] is valid
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: For loop over Range infers element type") {
  Typechecker tc;

  // For loop over Range[i32] should have i32 element type
  Tokenizer fn_tok("fn test() { "
                   "for i in 0..10 { "
                   "  let x: i32 = i; "
                   "} "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test() { "
                                      "for i in 0..10 { "
                                      "  let x: i32 = i; "
                                      "} "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Loop variable i should be i32
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: For loop over Slice infers element type") {
  Typechecker tc;

  // For loop over []i32 should have i32 element type
  Tokenizer fn_tok("fn test(data: []i32) { "
                   "for x in data { "
                   "  let y: i32 = x; "
                   "} "
                   "}");
  Parser fn_parser(fn_tok.tokenize(), "fn test(data: []i32) { "
                                      "for x in data { "
                                      "  let y: i32 = x; "
                                      "} "
                                      "}");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Loop variable x should be i32
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}

TEST_CASE("Typechecker: Slice type always has type argument") {
  Typechecker tc;

  // []i32 should resolve to Slice[i32]
  Tokenizer tokenizer("[]i32");
  auto tokens = tokenizer.tokenize();
  Parser parser(tokens, "[]i32");
  auto type_annot = parser.parse_type();
  REQUIRE(type_annot.has_value());

  auto resolved = tc.resolve_type(type_annot->value);
  REQUIRE(resolved != nullptr);

  auto *named = std::get_if<typed::TyNamed>(&resolved->ty);
  REQUIRE(named != nullptr);
  CHECK(named->name == "Slice");
  CHECK(named->kind == typed::Slice);
  REQUIRE(named->args.size() == 1); // Should always have type arg

  auto *inner = std::get_if<typed::TyNamed>(&named->args[0]->ty);
  REQUIRE(inner != nullptr);
  CHECK(inner->name == "i32");
}

TEST_CASE("Typechecker: Range type preserves element type") {
  Typechecker tc;

  Tokenizer struct_tok("struct Range[T] { first: T, second: T, }");
  Parser struct_parser(struct_tok.tokenize(),
                       "struct Range[T] { first: T, second: T, }");
  auto struct_decl = struct_parser.parse_struct_decl();
  REQUIRE(struct_decl.has_value());
  tc.collect_struct(*struct_decl);

  // Range[f64] should preserve f64
  Tokenizer fn_tok("fn test() -> Range[f64] { return 0.0..10.0; }");
  Parser fn_parser(fn_tok.tokenize(),
                   "fn test() -> Range[f64] { return 0.0..10.0; }");
  auto fn_decl = fn_parser.parse_fn_decl();
  REQUIRE(fn_decl.has_value());

  // Should not throw. Range[f64] is valid
  CHECK_NOTHROW(tc.check_fn(*fn_decl));
}
