#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

#include "../span.h"
#include "ir_types.h"

using namespace std;

namespace shikimori::ir {

struct Value;
struct Instruction;
struct Block;
struct Function;
struct Module;

using ValueRef = shared_ptr<Value>;
using InstRef = shared_ptr<Instruction>;
using BlockRef = shared_ptr<Block>;

enum class Opcode {
    // Memory
    ALLOCA,
    LOAD,
    STORE,
    GEP,          // getelementptr
    
    // Control flow
    BR,
    CBR,          // conditional branch
    RET,
    SWITCH,       // for match
    
    // PHI
    PHI,
    
    // Select
    SELECT,
    
    // Casts
    BITCAST,
    SEXT,
    ZEXT,
    TRUNC,
    FPEXT,
    FPTRUNC,
    FPTOUI,
    FPTOSI,
    UITOPTR,
    PTRTOINT,
    
    // Arithmetic - integer
    ADD,
    SUB,
    MUL,
    SDIV,         // signed
    UDIV,
    SREM,
    UREM,
    AND,
    OR,
    XOR,
    SHL,
    LSHR,         // logical shift right
    ASHR,         // arithmetic shift right
    
    // Arithmetic - float
    FADD,
    FSUB,
    FMUL,
    FDIV,
    FREM,
    
    // Comparison
    ICMP,         // integer comparison
    FCMP,         // float comparison
    
    // Call
    CALL,
    
    // Aggregate
    INSERTVALUE,
    EXTRACTVALUE,
    
    // Vector/Array
    INLINECONSTANT,  // for small arrays
    
    // Debug
    DBG_VALUE,
    DBG_LOCATION,
};

enum class CompareKind {
    // Integer
    EQ, NE,
    SLT, SLE, SGT, SGE,  // signed
    ULT, ULE, UGT, UGE,  // unsigned
    
    // Float
    OEQ, ONE, OLT, OLE, OGT, OGE,  // ordered
    UNO,                         // unordered
    ORD, UNORD,                  // ordered/unordered
};

struct Use {
    ValueRef value;
    Instruction* user;
    uint32_t arg_index;
};

enum class ValueKind {
    CONSTANT,
    ARGUMENT,
    INSTRUCTION,
    BLOCK,
    FUNCTION,
    GLOBAL,
};

enum class ConstKind {
    INT,
    FLOAT,
    BOOL,
    NULLPTR,
    STRING,
    AGGREGATE,    // for arrays
    ZEROINIT,
};

struct Constant {
    ConstKind kind;
    IRTypeRef type;
    
    // For different constant types
    int64_t int_val;
    double float_val;
    bool bool_val;
    string string_val;
    vector<int64_t> aggregate_vals;  // for arrays
    
    Span span;
};

struct Value {
    ValueKind kind;
    IRTypeRef type;
    uint32_t id;  // for registers: %id, for blocks: label id
    
    // For instructions
    InstRef inst;
    
    // For arguments
    uint32_t arg_index;
    
    // For constants
    Constant constant;
    
    // Use list
    vector<Use> uses;
    
    string name() const;
};

struct Instruction {
    Opcode op;
    IRTypeRef type;
    vector<ValueRef> args;
    uint32_t id;
    
    // For PHI: pairs of (value, block)
    vector<pair<ValueRef, BlockRef>> phi_preds;
    
    // For switch
    vector<pair<int64_t, BlockRef>> switch_cases;
    BlockRef default_dest;
    
    // For call: callee function
    string callee_name;
    Function* callee_fn;
    
    // For GEP
    vector<uint64_t> indices;  // for struct field indices
    
    // For select/icmp/fcmp
    CompareKind cmp_kind;
    
    // For alloca
    uint64_t alloca_size;
    uint64_t alloca_align;
    
    Span span;
    
    string op_name() const;
};

struct Block {
    uint32_t id;
    vector<InstRef> instructions;
    vector<BlockRef> predecessors;
    Function* parent;
    
    // For phi placement
    vector<InstRef> phis;
    
    string name() const;
    string label() const;  // for LLVM emission
};

struct Function {
    string name;
    IRTypeRef fn_type;
    IRTypeRef return_type;
    vector<ValueRef> params;  // arguments
    vector<BlockRef> blocks;
    uint32_t next_block_id;
    uint32_t next_inst_id;
    
    // For emission
    string mangle_name() const;
};

struct GlobalVariable {
    string name;
    IRTypeRef type;
    Constant init;
    bool is_constant;  // true for string literals etc
    
    Span span;
};

struct Module {
    string name;
    
    // Definitions
    vector<Function> functions;
    vector<shikimori::IRStruct> structs;
    vector<shikimori::IRUnion> unions;
    vector<shikimori::IRExtern> externs;
    vector<GlobalVariable> globals;
    
    // Maps for lookup
    unordered_map<string, Function*> function_map;
    unordered_map<string, shikimori::IRStruct*> struct_map;
    unordered_map<string, shikimori::IRUnion*> union_map;
    
    // Emission
    string emit_llvm() const;
};

} // namespace shikimori::ir
