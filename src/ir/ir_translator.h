#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "../ast/typedast.h"
#include "ir.h"
#include "ir_builder.h"
#include "ir_types.h"

using namespace std;

namespace shikimori::ir {

struct IRTranslator {
    Module* module;
    IRBuilder builder;
    
    // Context
    Function* current_fn = nullptr;
    BlockRef current_block = nullptr;
    
    // Local mappings
    unordered_map<string, ValueRef> locals;  // local variable name -> value
    unordered_map<string, ValueRef> params;   // param name -> value
    
    // For match compilation
    struct MatchContext {
        ValueRef switch_val;
        vector<pair<int64_t, BlockRef>> cases;
        BlockRef default_block;
        BlockRef end_block;
    };
    vector<MatchContext> match_stack;
    
    // Defer handling
    struct DeferEntry {
        BlockRef cleanup_block;
        vector<InstRef> cleanup_insts;
    };
    vector<DeferEntry> defer_stack;
    
    IRTranslator(Module* mod) : module(mod), builder(mod) {}
    
    // Translation entry point
    void translate_program(const typed::TypedProgram& prog);
    
    // Top-level declarations
    void translate_decl(const typed::TypedDecl& decl);
    void translate_fn(const typed::TypedFnDecl& fn);
    void translate_struct(const typed::TypedStructDecl& s);
    void translate_union(const typed::TypedUnionDecl& u);
    void translate_extern(const typed::TypedExternDecl& e);
    
    // Statements
    ValueRef translate_stmt(const typed::TypedStmt& stmt);
    ValueRef translate_let(const typed::LetStmt& let);
    ValueRef translate_return(const typed::ReturnStmt& ret);
    ValueRef translate_defer(const typed::DeferStmt& defer);
    ValueRef translate_loop(const typed::LoopStmt& loop);
    ValueRef translate_while(const typed::WhileStmt& while_stmt);
    ValueRef translate_for(const typed::ForStmt& for_stmt);
    ValueRef translate_expr_stmt(unique_ptr<typed::TypedExpr> expr);
    
    // Expressions
    ValueRef translate_expr(const typed::TypedExpr& expr);
    
    // Literals
    ValueRef translate_int(const typed::IntLiteral& lit);
    ValueRef translate_float(const typed::FloatLiteral& lit);
    ValueRef translate_bool(const typed::BoolLiteral& lit);
    ValueRef translate_string(const typed::StringLiteral& lit);
    ValueRef translate_null(const typed::NullLiteral& lit);
    
    // Complex exprs
    ValueRef translate_ident(const typed::IdentifierExpr& expr);
    ValueRef translate_struct_init(const typed::StructInit& init);
    ValueRef translate_union_init(const typed::UnionVariantInit& init);
    ValueRef translate_field_access(const typed::FieldAccess& access);
    ValueRef translate_method_call(const typed::MethodCall& call);
    ValueRef translate_index(const typed::IndexAccess& index);
    ValueRef translate_call(const typed::Call& call);
    ValueRef translate_unary(const typed::UnaryExpr& unary);
    ValueRef translate_binary(const typed::BinaryExpr& binary);
    ValueRef translate_if(const typed::IfExpr& ifexpr);
    ValueRef translate_match(const typed::MatchExpr& match);
    ValueRef translate_range(const typed::RangeExpr& range);
    ValueRef translate_type_init(const typed::TypeInit& init);
    ValueRef translate_as(const typed::AsExpr& as);
    ValueRef translate_builtin(const typed::BuiltinCall& builtin);
    
    // Control flow helpers
    BlockRef create_block(string name = "");
    void set_block(BlockRef block);
    void br(BlockRef dest);
    void cbr(ValueRef cond, BlockRef true_br, BlockRef false_br);
    ValueRef phi(IRTypeRef type, vector<pair<ValueRef, BlockRef>> preds);
    
    // Helpers
    IRTypeRef convert_type(TypeRef ty);
    ValueRef load_if_needed(ValueRef val);
    ValueRef ensure_ptr(ValueRef val, IRTypeRef expected_ptr_type);
    
    // For deferred cleanup
    void emit_defer_cleanup();
};

} // namespace shikimori::ir
