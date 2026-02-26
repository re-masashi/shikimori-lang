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

struct TyInterface {
  string name;
  map<string, TypeRef> methods; // TypeRef not fnty because functions can be
                                // wrapped inside forall
};

struct InterfaceConstraint {
  vector<string> interfaces;
};

struct Type {
  // Slice, Pointer, Option, etc are named
  variant<TyVar, ETVar, TyNamed, FnTy, ForAll, TyArray,
          InterfaceConstraint>
      ty; // have to add forall here
  Span span;
};

} // namespace shikimori
