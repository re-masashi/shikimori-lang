#include "ir/ir.h"

namespace shikimori::ir {

string Value::name() const {
    switch (kind) {
        case ValueKind::CONSTANT:
            return "const";
        case ValueKind::ARGUMENT:
            return "%arg." + to_string(arg_index);
        case ValueKind::INSTRUCTION:
            return "%" + to_string(id);
        case ValueKind::BLOCK:
            return "block@" + to_string(id);
        case ValueKind::FUNCTION:
            return "@" + reinterpret_cast<const Function*>(this)->name;
        case ValueKind::GLOBAL:
            return "@global";
    }
    return "?";
}

string Instruction::op_name() const {
    switch (op) {
        case Opcode::ALLOCA: return "alloca";
        case Opcode::LOAD: return "load";
        case Opcode::STORE: return "store";
        case Opcode::GEP: return "getelementptr";
        case Opcode::BR: return "br";
        case Opcode::CBR: return "cbr";
        case Opcode::RET: return "ret";
        case Opcode::SWITCH: return "switch";
        case Opcode::PHI: return "phi";
        case Opcode::SELECT: return "select";
        case Opcode::BITCAST: return "bitcast";
        case Opcode::SEXT: return "sext";
        case Opcode::ZEXT: return "zext";
        case Opcode::TRUNC: return "trunc";
        case Opcode::FPEXT: return "fpext";
        case Opcode::FPTRUNC: return "fptrunc";
        case Opcode::FPTOUI: return "fptoui";
        case Opcode::FPTOSI: return "fptosi";
        case Opcode::UITOPTR: return "uitoptr";
        case Opcode::PTRTOINT: return "ptrtoint";
        case Opcode::ADD: return "add";
        case Opcode::SUB: return "sub";
        case Opcode::MUL: return "mul";
        case Opcode::SDIV: return "sdiv";
        case Opcode::UDIV: return "udiv";
        case Opcode::SREM: return "srem";
        case Opcode::UREM: return "urem";
        case Opcode::AND: return "and";
        case Opcode::OR: return "or";
        case Opcode::XOR: return "xor";
        case Opcode::SHL: return "shl";
        case Opcode::LSHR: return "lshr";
        case Opcode::ASHR: return "ashr";
        case Opcode::FADD: return "fadd";
        case Opcode::FSUB: return "fsub";
        case Opcode::FMUL: return "fmul";
        case Opcode::FDIV: return "fdiv";
        case Opcode::FREM: return "frem";
        case Opcode::ICMP: return "icmp";
        case Opcode::FCMP: return "fcmp";
        case Opcode::CALL: return "call";
        case Opcode::INSERTVALUE: return "insertvalue";
        case Opcode::EXTRACTVALUE: return "extractvalue";
        case Opcode::INLINECONSTANT: return "inlineconstant";
        case Opcode::DBG_VALUE: return "dbg.value";
        case Opcode::DBG_LOCATION: return "dbg.location";
    }
    return "?";
}

string Block::name() const {
    return "bb" + to_string(id);
}

string Block::label() const {
    return name() + ":";
}

string Function::mangle_name() const {
    return "_" + name;
}

} // namespace shikimori::ir
