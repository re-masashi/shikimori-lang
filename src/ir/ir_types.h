#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "../span.h"

using namespace std;

namespace shikimori {

// Forward declare
struct IRBuilder;

struct IRType;
using IRTypeRef = shared_ptr<IRType>;

enum class PrimTy {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    BOOL,
    USIZE,
    STRING,   // fat pointer { *u8, len }
};

struct IRTyPointer {
    IRTypeRef inner;
};

struct IRTySlice {
    IRTypeRef inner;  // *T, len
};

struct IRTyOptional {
    IRTypeRef inner;  // union { some: T, none }
};

struct IRTyNamed {
    string name;
    vector<IRTypeRef> args;  // for generic instantiation
};

struct IRFnTy {
    vector<IRTypeRef> args;
    IRTypeRef return_type;
    bool is_vararg;
};

struct IRTyArray {
    IRTypeRef inner;
    uint64_t size;
};

struct IRTyInterfaceObj {
    vector<string> interfaces;
    IRTypeRef data_ty;
};

struct IRTyInterface {
    string name;
    map<string, IRTypeRef> methods;
};

struct IRType {
    variant<PrimTy, IRTyPointer, IRTySlice, IRTyOptional,
            IRTyNamed, IRFnTy, IRTyArray, IRTyInterfaceObj, IRTyInterface>
        ty;
    Span span;
    
    string name() const;
    uint64_t size() const;  // in bytes, 0 if unsized
    uint64_t alignment() const;
    bool is_sized() const;
};

struct IRField {
    string name;
    IRTypeRef type;
    uint64_t offset;
};

struct IRStruct {
    string name;
    vector<IRField> fields;
    vector<IRTypeRef> methods;  // method signatures
    uint64_t size;
    uint64_t alignment;
    bool is_packed;
    
    Span span;
};

struct IRUnionVariant {
    string name;
    IRTypeRef type;
    uint64_t offset;
};

struct IRUnion {
    string name;
    vector<IRUnionVariant> variants;
    uint64_t size;
    uint64_t alignment;
    
    Span span;
};

struct IRExtern {
    string name;
    string linkage;  // "C", "Rust", etc
    vector<IRTypeRef> params;
    IRTypeRef return_type;
    bool is_vararg;
    
    Span span;
};

} // namespace shikimori
