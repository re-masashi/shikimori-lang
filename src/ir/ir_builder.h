#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>

#include "../span.h"
#include "ir.h"
#include "ir_types.h"

using namespace std;

namespace shikimori::ir {

struct IRBuilder {
    Module* module;
    Function* current_fn;
    BlockRef current_block;
    
    IRBuilder(Module* mod) : module(mod), current_fn(nullptr), current_block(nullptr) {}
    
    // Type creation
    IRTypeRef get_i8()  { return make_prim(PrimTy::I8); }
    IRTypeRef get_i16() { return make_prim(PrimTy::I16); }
    IRTypeRef get_i32() { return make_prim(PrimTy::I32); }
    IRTypeRef get_i64() { return make_prim(PrimTy::I64); }
    IRTypeRef get_u8()  { return make_prim(PrimTy::U8); }
    IRTypeRef get_u16() { return make_prim(PrimTy::U16); }
    IRTypeRef get_u32() { return make_prim(PrimTy::U32); }
    IRTypeRef get_u64() { return make_prim(PrimTy::U64); }
    IRTypeRef get_f32() { return make_prim(PrimTy::F32); }
    IRTypeRef get_f64() { return make_prim(PrimTy::F64); }
    IRTypeRef get_bool() { return make_prim(PrimTy::BOOL); }
    IRTypeRef get_usize() { return make_prim(PrimTy::USIZE); }
    IRTypeRef get_void() { return make_prim(PrimTy::I8); }  // TODO: void type
    
    IRTypeRef make_prim(PrimTy prim, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = prim;
        ty->span = span;
        return ty;
    }
    
    IRTypeRef make_pointer(IRTypeRef inner, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = IRTyPointer{inner};
        ty->span = span;
        return ty;
    }
    
    IRTypeRef make_slice(IRTypeRef inner, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = IRTySlice{inner};
        ty->span = span;
        return ty;
    }
    
    IRTypeRef make_array(IRTypeRef inner, uint64_t size, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = IRTyArray{inner, size};
        ty->span = span;
        return ty;
    }
    
    IRTypeRef make_named(string name, vector<IRTypeRef> args = {}, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = IRTyNamed{name, args};
        ty->span = span;
        return ty;
    }
    
    IRTypeRef make_fn(vector<IRTypeRef> args, IRTypeRef ret, bool vararg = false, Span span = Span{}) {
        auto ty = make_shared<IRType>();
        ty->ty = IRFnTy{args, ret, vararg};
        ty->span = span;
        return ty;
    }
    
    // Function building
    Function* create_function(string name, IRTypeRef fn_type, Span span = Span{}) {
        auto& fn = module->functions.emplace_back();
        fn.name = name;
        fn.fn_type = fn_type;
        
        // Extract return type and params from fn_type
        if (auto* ft = get_if<IRFnTy>(&fn_type->ty)) {
            fn.return_type = ft->return_type;
            for (size_t i = 0; i < ft->args.size(); i++) {
                auto arg = make_shared<Value>();
                arg->kind = ValueKind::ARGUMENT;
                arg->type = ft->args[i];
                arg->arg_index = (uint32_t)i;
                fn.params.push_back(arg);
            }
        }
        
        fn.next_block_id = 0;
        fn.next_inst_id = 0;
        
        module->function_map[name] = &fn;
        return &fn;
    }
    
    BlockRef create_block(Function* fn, string name = "") {
        auto block = make_shared<Block>();
        block->id = fn->next_block_id++;
        block->parent = fn;
        if (name.empty()) {
            name = "bb" + to_string(block->id);
        }
        fn->blocks.push_back(block);
        return block;
    }
    
    void set_insert_block(BlockRef block) {
        current_block = block;
        current_fn = block->parent;
    }
    
    ValueRef create_alloca(IRTypeRef type, uint64_t align = 8, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::ALLOCA;
        inst->type = type;
        inst->id = current_fn->next_inst_id++;
        inst->alloca_align = align;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    ValueRef create_load(IRTypeRef type, ValueRef ptr, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::LOAD;
        inst->type = type;
        inst->args.push_back(ptr);
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    ValueRef create_store(ValueRef val, ValueRef ptr, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::STORE;
        inst->type = nullptr;
        inst->args = {val, ptr};
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        current_block->instructions.push_back(inst);
        return nullptr;
    }
    
    ValueRef create_call(IRTypeRef ret_type, string callee, vector<ValueRef> args, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::CALL;
        inst->type = ret_type;
        inst->callee_name = callee;
        inst->args = args;
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = ret_type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    ValueRef create_ret(ValueRef val, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::RET;
        inst->type = val ? val->type : nullptr;
        if (val) inst->args.push_back(val);
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        current_block->instructions.push_back(inst);
        return nullptr;
    }
    
    ValueRef create_br(BlockRef dest, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::BR;
        inst->type = nullptr;
        inst->args.push_back(make_shared<Value>());  // dummy
        inst->default_dest = dest;
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        current_block->instructions.push_back(inst);
        return nullptr;
    }
    
    ValueRef create_cbr(ValueRef cond, BlockRef true_br, BlockRef false_br, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::CBR;
        inst->type = nullptr;
        inst->args = {cond};
        inst->default_dest = true_br;
        inst->default_dest = false_br;
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        current_block->instructions.push_back(inst);
        return nullptr;
    }
    
    ValueRef create_phi(IRTypeRef type, vector<pair<ValueRef, BlockRef>> preds, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::PHI;
        inst->type = type;
        inst->phi_preds = preds;
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->phis.push_back(inst);
        return val;
    }
    
    ValueRef create_binop(Opcode op, IRTypeRef type, ValueRef lhs, ValueRef rhs, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = op;
        inst->type = type;
        inst->args = {lhs, rhs};
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    ValueRef create_icmp(CompareKind kind, IRTypeRef type, ValueRef lhs, ValueRef rhs, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::ICMP;
        inst->type = get_bool();
        inst->cmp_kind = kind;
        inst->args = {lhs, rhs};
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = get_bool();
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    ValueRef create_select(ValueRef cond, ValueRef true_val, ValueRef false_val, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::SELECT;
        inst->type = true_val->type;
        inst->args = {cond, true_val, false_val};
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = true_val->type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    // GEP for struct field access
    ValueRef create_gep(IRTypeRef result_type, ValueRef ptr, vector<uint64_t> indices, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = Opcode::GEP;
        inst->type = result_type;
        inst->args.push_back(ptr);
        inst->indices = indices;
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto val = make_shared<Value>();
        val->kind = ValueKind::INSTRUCTION;
        val->type = result_type;
        val->id = inst->id;
        val->inst = inst;
        
        current_block->instructions.push_back(inst);
        return val;
    }
    
    // Casts
    ValueRef create_cast(Opcode op, IRTypeRef dest_type, ValueRef val, Span span = Span{}) {
        auto inst = make_shared<Instruction>();
        inst->op = op;
        inst->type = dest_type;
        inst->args.push_back(val);
        inst->id = current_fn->next_inst_id++;
        inst->span = span;
        
        auto result = make_shared<Value>();
        result->kind = ValueKind::INSTRUCTION;
        result->type = dest_type;
        result->id = inst->id;
        result->inst = inst;
        
        current_block->instructions.push_back(inst);
        return result;
    }
    
    // Constants
    ValueRef make_int(int64_t val, IRTypeRef type, Span span = Span{}) {
        auto c = make_shared<Constant>();
        c->kind = ConstKind::INT;
        c->type = type;
        c->int_val = val;
        c->span = span;
        
        auto v = make_shared<Value>();
        v->kind = ValueKind::CONSTANT;
        v->type = type;
        v->constant = *c;
        return v;
    }
    
    ValueRef make_bool(bool val, Span span = Span{}) {
        return make_int(val ? 1 : 0, get_bool(), span);
    }
    
    ValueRef make_null(IRTypeRef type, Span span = Span{}) {
        auto c = make_shared<Constant>();
        c->kind = ConstKind::NULLPTR;
        c->type = type;
        c->span = span;
        
        auto v = make_shared<Value>();
        v->kind = ValueKind::CONSTANT;
        v->type = type;
        v->constant = *c;
        return v;
    }
    
    // Lookup
    Function* get_function(string name) {
        auto it = module->function_map.find(name);
        return it != module->function_map.end() ? it->second : nullptr;
    }
};

} // namespace shikimori::ir
