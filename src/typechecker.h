#include "types.h"
#include <cstdint>
#include <map>
#include <vector>

using namespace std;
using namespace shikimori;

struct StructDef {
  vector<pair<string, TypeRef>> fields;
  ForAll scheme;
};

struct UnionDef {
  vector<pair<string, vector<TypeRef>>> variants;
  ForAll scheme;
};

namespace shikimori {

struct Typechecker {
  map<string, TypeRef> functions; // name -> ForAll or FnTy
  map<string, StructDef> structs;
  map<string, StructDef> unions;
  map<string, TyInterface> interfaces;

  map<uint32_t, TypeRef> ty_solutions; // solved ETVars
  uint32_t next_id = 0;

  vector<map<string, TypeRef>> scopes;

  uint32_t fresh_id() { return next_id++; }
  TypeRef fresh_etvar(string name);
  TypeRef lookup_local(const string &name);
  void push_scope();
  void pop_scope();
  void define_local(const string &name, TypeRef ty);
};

} // namespace shikimori
