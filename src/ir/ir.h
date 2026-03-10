#include "ir_types.h"
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace shikimori::ir {

struct Function;
struct Extern;
struct Struct;
struct Union;
struct Interface; // vtable approach
struct BasicBlock;
struct Instruction;

struct Module {
  std::string name;
  vector<Function> functions;
  vector<Extern> extern_functions;
  vector<Struct> structs;
  vector<Union> unions;
  vector<Interface> interfaces;
};

struct Function {
  std::string name;
  vector<pair<std::string, IRTypeRef>> args;
  IRTypeRef return_type;

  map<std::string, BasicBlock> bbs;
};

struct BasicBlock {
  std::string name; // redundant?

  vector<Instruction> insts;
};

}
