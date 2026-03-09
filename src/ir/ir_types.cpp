#include "ir/ir_types.h"
#include <cstdint>
#include <stdexcept>
#include <unordered_map>
#include <variant>
#include <functional>

namespace shikimori {

using namespace std;

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

static const unordered_map<PrimTy, pair<uint64_t, uint64_t>> prim_info = {
    {PrimTy::I8,   {1, 1}},
    {PrimTy::I16,  {2, 2}},
    {PrimTy::I32,  {4, 4}},
    {PrimTy::I64,  {8, 8}},
    {PrimTy::U8,   {1, 1}},
    {PrimTy::U16,  {2, 2}},
    {PrimTy::U32,  {4, 4}},
    {PrimTy::U64,  {8, 8}},
    {PrimTy::F32,  {4, 4}},
    {PrimTy::F64,  {8, 8}},
    {PrimTy::BOOL, {1, 1}},
    {PrimTy::USIZE,{8, 8}},
    {PrimTy::STRING,{16, 8}},
};

string IRType::name() const {
    return visit(overloaded {
        [](PrimTy p) -> string {
            switch (p) {
                case PrimTy::I8: return "i8";
                case PrimTy::I16: return "i16";
                case PrimTy::I32: return "i32";
                case PrimTy::I64: return "i64";
                case PrimTy::U8: return "u8";
                case PrimTy::U16: return "u16";
                case PrimTy::U32: return "u32";
                case PrimTy::U64: return "u64";
                case PrimTy::F32: return "f32";
                case PrimTy::F64: return "f64";
                case PrimTy::BOOL: return "bool";
                case PrimTy::USIZE: return "usize";
                case PrimTy::STRING: return "string";
            }
            return "?";
        },
        [](const IRTyPointer& p) -> string {
            return p.inner->name() + "*";
        },
        [](const IRTySlice& s) -> string {
            return "[]" + s.inner->name();
        },
        [](const IRTyOptional& o) -> string {
            return "?" + o.inner->name();
        },
        [](const IRTyNamed& n) -> string {
            if (n.args.empty()) {
                return n.name;
            }
            string result = n.name + "<";
            for (size_t i = 0; i < n.args.size(); i++) {
                if (i > 0) result += ", ";
                result += n.args[i]->name();
            }
            result += ">";
            return result;
        },
        [](const IRFnTy& f) -> string {
            string result = "(";
            for (size_t i = 0; i < f.args.size(); i++) {
                if (i > 0) result += ", ";
                result += f.args[i]->name();
            }
            result += ") -> ";
            result += f.return_type->name();
            return result;
        },
        [](const IRTyArray& a) -> string {
            return "[" + to_string(a.size) + "]" + a.inner->name();
        },
        [](const IRTyInterfaceObj& i) -> string {
            string result = "interface {";
            for (size_t j = 0; j < i.interfaces.size(); j++) {
                if (j > 0) result += " + ";
                result += i.interfaces[j];
            }
            result += "}";
            return result;
        },
        [](const IRTyInterface&) -> string {
            return "interface";
        },
    }, ty);
}

uint64_t IRType::size() const {
    return visit(overloaded {
        [&](PrimTy p) -> uint64_t {
            auto it = prim_info.find(p);
            return it != prim_info.end() ? it->second.first : 0;
        },
        [](const IRTyPointer&) -> uint64_t { return 8; },
        [](const IRTySlice&) -> uint64_t { return 16; },
        [](const IRTyOptional& o) -> uint64_t { return o.inner->size() + 1; },
        [](const IRTyNamed&) -> uint64_t { return 8; },
        [](const IRFnTy&) -> uint64_t { return 8; },
        [](const IRTyArray& a) -> uint64_t { 
            return a.size * a.inner->size();
        },
        [](const IRTyInterfaceObj&) -> uint64_t { return 16; },
        [](const IRTyInterface&) -> uint64_t { return 0; },
    }, ty);
}

uint64_t IRType::alignment() const {
    return visit(overloaded {
        [&](PrimTy p) -> uint64_t {
            auto it = prim_info.find(p);
            return it != prim_info.end() ? it->second.second : 8;
        },
        [](const IRTyPointer&) -> uint64_t { return 8; },
        [](const IRTySlice&) -> uint64_t { return 8; },
        [](const IRTyOptional& o) -> uint64_t { return o.inner->alignment(); },
        [](const IRTyNamed&) -> uint64_t { return 8; },
        [](const IRFnTy&) -> uint64_t { return 8; },
        [](const IRTyArray& a) -> uint64_t { return a.inner->alignment(); },
        [](const IRTyInterfaceObj&) -> uint64_t { return 8; },
        [](const IRTyInterface&) -> uint64_t { return 8; },
    }, ty);
}

bool IRType::is_sized() const {
    return visit(overloaded {
        [](PrimTy) -> bool { return true; },
        [](const IRTyPointer&) -> bool { return true; },
        [](const IRTySlice&) -> bool { return true; },
        [](const IRTyOptional& o) -> bool { return o.inner->is_sized(); },
        [](const IRTyNamed&) -> bool { return true; },
        [](const IRFnTy&) -> bool { return true; },
        [](const IRTyArray& a) -> bool { return a.inner->is_sized(); },
        [](const IRTyInterfaceObj&) -> bool { return true; },
        [](const IRTyInterface&) -> bool { return false; },
    }, ty);
}

} // namespace shikimori
