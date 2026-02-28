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

// Forward declarations
struct Expr;
struct TypeAnnot;

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

} // namespace shikimori::ast
