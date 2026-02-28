#pragma once

#include "span.h"
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

using namespace std;

namespace shikimori {

struct Type;
using TypeRef = shared_ptr<Type>;

struct TyVar {
  uint32_t id;
  string name; // "T", "A", etc. For error messages
};

struct ETVar { // existential type var
  uint32_t id;
  string name; // "T", "A", etc. For error messages
};

enum NamedTyKind {
  Struct,
  Union,
  Primitive,
  Pointer,
  Slice,
  Interface, // fat pointer: { *data, *vtable }
};

struct TyNamed {
  string name;
  NamedTyKind kind;
  vector<TypeRef> args;
};

struct FnTy {
  vector<TypeRef> args;
  TypeRef return_type;
};

struct ForAll {
  vector<pair<string, uint32_t>> vars; // name, tyvar
  TypeRef body;
};

struct TyArray {
  TypeRef inner;
  uint64_t size;
};

// Interface trait object (fat pointer)
struct TyInterfaceObj {
  vector<string> interfaces; // interface A + B -> [A, B]
  TypeRef data_ty;           // concrete type being wrapped
};

struct TyInterface {
  string name;
  map<string, TypeRef> methods; // TypeRef not fnty because functions can be
                                // wrapped inside forall
};

struct Type {
  variant<TyVar, ETVar, TyNamed, FnTy, ForAll, TyArray, TyInterfaceObj>
      ty;
  Span span;
};

} // namespace shikimori
