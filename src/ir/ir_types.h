#include "span.h"
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace shikimori::ir {

using namespace std;

struct IRType;
struct IRTyNamed;
struct IRFnTy;
struct IRTyArray;
struct IRTyInterfaceObj;

using IRTypeRef = shared_ptr<IRType>;

enum IRPrimTy {
  i1, // bool
  i8,
  i16,
  i32,
  i64,

  // f16,
  f32,
  f64,

  string,
};

struct IRStructTy {
  std::string name;
  vector<pair<std::string, IRTypeRef>> fields;
};

struct IRUnionTy {
  std::string name;
  vector<pair<std::string, IRTypeRef>> fields;
};

struct IRTyNamed {
  std::variant<IRUnionTy, IRStructTy> name;
  // vector<pair<std::string, IRFnTy>> methods;
  // methods should be lifted from instance_of_a.meth(args) to A::meth(instance_of_a, args)
  // then, A::b should be lowered to A_meth_b, ie, into a top level function.
};

struct IRFnTy {
  vector<IRType> args;
  IRTypeRef ret_type;
};

struct IRTyInterfaceObj {
  IRTypeRef functions;
};

struct IRTyArray {
  IRTypeRef inner;
};

struct IRTySlice {
  IRTypeRef inner;
};

struct IRTyPtr {
  IRTypeRef inner;
};

struct IRType {
  variant<
    IRPrimTy,
    IRTyNamed, 
    IRFnTy, 
    IRTyArray,
    IRTySlice,
    IRTyPtr,
    IRTyInterfaceObj>
      ty;
  Span span;
};

}
