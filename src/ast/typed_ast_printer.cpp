#include "ast/typed_ast_printer.h"
#include <memory>
#include <optional>
#include <stddef.h>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "ast/typedast.h"
#include "color.hpp"
#include "span.h"
#include "types.h"
#include "utils.h"

using namespace std;

namespace shikimori {

struct PrinterContext {
    ostream &os;
    int indent_level;

    PrinterContext(ostream &os) : os(os), indent_level(0) {}

    void indent() {
        for (int i = 0; i < indent_level; i++)
            os << "  ";
    }

    void print_span(const Span &span) {
        os << Color::GRAY << " [" << span.start << ".." << span.end << "]"
           << Color::RESET;
    }
};

static void dump_type(PrinterContext &ctx, const TypeRef &type);
static void dump_expr(PrinterContext &ctx, const typed::TypedExpr &expr);
static void dump_stmt(PrinterContext &ctx, const typed::TypedStmt &stmt);
static void dump_block(PrinterContext &ctx, const typed::TypedBlock &block);
static void dump_pattern(PrinterContext &ctx, const typed::TypedPattern &pat);
static void dump_fn(PrinterContext &ctx, const typed::TypedFnDecl &fn);
static void dump_comptime_stmt(PrinterContext &ctx, const typed::ComptimeStmt &stmt);
static void dump_decl(PrinterContext &ctx, const typed::TypedDecl &decl);

static void dump_type(PrinterContext &ctx, const TypeRef &type) {
    if (!type) {
        ctx.os << Color::GRAY << "<null type>" << Color::RESET;
        return;
    }

    visit(
        overload{
            [&](const TyVar &ty) {
                ctx.os << Color::CYAN << "TyVar" << Color::RESET << " " << ty.name << " #"
                   << ty.id;
            },
            [&](const ETVar &ty) {
                ctx.os << Color::CYAN << "ETVar" << Color::RESET << " " << ty.name << " #"
                   << ty.id;
            },
            [&](const TyNamed &ty) {
                ctx.os << Color::CYAN << "TyNamed" << Color::RESET << " " << ty.name;
                switch (ty.kind) {
                case NamedTyKind::Struct:
                    ctx.os << Color::GRAY << " (struct)" << Color::RESET;
                    break;
                case NamedTyKind::Union:
                    ctx.os << Color::GRAY << " (union)" << Color::RESET;
                    break;
                case NamedTyKind::Primitive:
                    ctx.os << Color::GRAY << " (primitive)" << Color::RESET;
                    break;
                case NamedTyKind::Pointer:
                    ctx.os << Color::GRAY << " (pointer)" << Color::RESET;
                    break;
                case NamedTyKind::Slice:
                    ctx.os << Color::GRAY << " (slice)" << Color::RESET;
                    break;
                case NamedTyKind::Interface:
                    ctx.os << Color::GRAY << " (interface)" << Color::RESET;
                    break;
                }
                if (!ty.args.empty()) {
                    ctx.os << "[";
                    for (size_t i = 0; i < ty.args.size(); i++) {
                        if (i > 0)
                            ctx.os << ", ";
                        dump_type(ctx, ty.args[i]);
                    }
                    ctx.os << "]";
                }
            },
            [&](const FnTy &ty) {
                ctx.os << Color::CYAN << "FnTy" << Color::RESET << "(";
                for (size_t i = 0; i < ty.args.size(); i++) {
                    if (i > 0)
                        ctx.os << ", ";
                    dump_type(ctx, ty.args[i]);
                }
                ctx.os << ") -> ";
                dump_type(ctx, ty.return_type);
            },
            [&](const ForAll &ty) {
                ctx.os << Color::CYAN << "ForAll" << Color::RESET << "[";
                for (size_t i = 0; i < ty.vars.size(); i++) {
                    if (i > 0)
                        ctx.os << ", ";
                    ctx.os << ty.vars[i].first << " #" << ty.vars[i].second;
                }
                ctx.os << "] ";
                dump_type(ctx, ty.body);
            },
            [&](const TyArray &ty) {
                ctx.os << Color::CYAN << "TyArray" << Color::RESET << "[" << ty.size
                   << "]";
                dump_type(ctx, ty.inner);
            },
            [&](const TyInterfaceObj &ty) {
                ctx.os << Color::CYAN << "TyInterfaceObj" << Color::RESET << " { ";
                for (size_t i = 0; i < ty.interfaces.size(); i++) {
                    if (i > 0)
                        ctx.os << " + ";
                    ctx.os << ty.interfaces[i];
                }
                ctx.os << " }";
                dump_type(ctx, ty.data_ty);
            },
            [&](const TyInterface &ty) {
                ctx.os << Color::CYAN << "TyInterface" << Color::RESET << " " << ty.name;
            }
        },
        type->ty);
}

static void dump_literal_pattern_value(PrinterContext &ctx, const ast::LiteralPattern &pat) {
    visit(
        overload{
            [&](const ast::IntLiteral &lit) {
                ctx.indent();
                ctx.os << Color::YELLOW << "IntLiteral" << Color::RESET << " " << lit.value;
                ctx.print_span(lit.span);
                ctx.os << "\n";
            },
            [&](const ast::FloatLiteral &lit) {
                ctx.indent();
                ctx.os << Color::YELLOW << "FloatLiteral" << Color::RESET << " " << lit.value;
                ctx.print_span(lit.span);
                ctx.os << "\n";
            },
            [&](const ast::BoolLiteral &lit) {
                ctx.indent();
                ctx.os << Color::YELLOW << "BoolLiteral" << Color::RESET << " "
                   << (lit.value ? "true" : "false");
                ctx.print_span(lit.span);
                ctx.os << "\n";
            },
            [&](const ast::StringLiteral &lit) {
                ctx.indent();
                ctx.os << Color::GREEN << "StringLiteral" << Color::RESET << " \"" << lit.value
                   << "\"";
                ctx.print_span(lit.span);
                ctx.os << "\n";
            },
            [&](const ast::NullLiteral &lit) {
                ctx.indent();
                ctx.os << Color::YELLOW << "NullLiteral" << Color::RESET;
                ctx.print_span(lit.span);
                ctx.os << "\n";
            }
        },
        pat.value);
}

static void dump_expr(PrinterContext &ctx, const typed::TypedExpr &expr) {
    const TypeRef &ty = expr.ty;

    visit(
        overload{
            [&](const typed::IntLiteral &arg) {
                ctx.indent();
                ctx.os << Color::YELLOW << "IntLiteral" << Color::RESET << " " << arg.value;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::FloatLiteral &arg) {
                ctx.indent();
                ctx.os << Color::YELLOW << "FloatLiteral" << Color::RESET << " " << arg.value;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::BoolLiteral &arg) {
                ctx.indent();
                ctx.os << Color::YELLOW << "BoolLiteral" << Color::RESET << " "
                   << (arg.value ? "true" : "false");
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::StringLiteral &arg) {
                ctx.indent();
                ctx.os << Color::GREEN << "StringLiteral" << Color::RESET << " \"" << arg.value
                   << "\"";
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::NullLiteral &arg) {
                ctx.indent();
                ctx.os << Color::YELLOW << "NullLiteral" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::IdentifierExpr &arg) {
                ctx.indent();
                ctx.os << Color::WHITE << "IdentifierExpr" << Color::RESET << " " << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::StructInit &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "StructInit" << Color::RESET << " " << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                for (auto &[name, val] : arg.fields) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "field " << name << ":" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_expr(ctx, *val);
                    ctx.indent_level--;
                }
            },
            [&](const typed::ScopeAccess &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "ScopeAccess" << Color::RESET << " " << arg.scope
                   << "::" << arg.member;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                for (auto &p : arg.payload) {
                    ctx.indent_level++;
                    dump_expr(ctx, *p);
                    ctx.indent_level--;
                }
            },
            [&](const typed::UnionVariantInit &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "UnionVariantInit" << Color::RESET << " "
                   << arg.union_name << "::" << arg.variant;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                for (auto &p : arg.payload) {
                    ctx.indent_level++;
                    dump_expr(ctx, *p);
                    ctx.indent_level--;
                }
            },
            [&](const typed::FieldAccess &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "FieldAccess" << Color::RESET << " ." << arg.field;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "object:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.object);
                ctx.indent_level--;
            },
            [&](const typed::MethodCall &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "MethodCall" << Color::RESET << " ." << arg.method
                   << "()";
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "object:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.object);
                ctx.indent_level--;
                for (auto &a : arg.args) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "arg:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_expr(ctx, *a);
                    ctx.indent_level--;
                }
            },
            [&](const typed::IndexAccess &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "IndexAccess" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "object:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.object);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "index:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.index);
                ctx.indent_level--;
            },
            [&](const typed::Call &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "Call" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "callee:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.callee);
                ctx.indent_level--;
                for (auto &a : arg.args) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "arg:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_expr(ctx, *a);
                    ctx.indent_level--;
                }
            },
            [&](const typed::UnaryExpr &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "UnaryExpr" << Color::RESET << " ";
                switch (arg.op) {
                case ast::UnaryOp::Not:
                    ctx.os << "!";
                    break;
                case ast::UnaryOp::Neg:
                    ctx.os << "-";
                    break;
                case ast::UnaryOp::Deref:
                    ctx.os << "*";
                    break;
                case ast::UnaryOp::AddrOf:
                    ctx.os << "&";
                    break;
                case ast::UnaryOp::BitNot:
                    ctx.os << "~";
                    break;
                }
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.operand);
                ctx.indent_level--;
            },
            [&](const typed::BinaryExpr &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "BinaryExpr" << Color::RESET << " ";
                switch (arg.op) {
                case ast::BinaryOp::Add:
                    ctx.os << "+";
                    break;
                case ast::BinaryOp::Sub:
                    ctx.os << "-";
                    break;
                case ast::BinaryOp::Mul:
                    ctx.os << "*";
                    break;
                case ast::BinaryOp::Div:
                    ctx.os << "/";
                    break;
                case ast::BinaryOp::Mod:
                    ctx.os << "%";
                    break;
                case ast::BinaryOp::LShift:
                    ctx.os << "<<";
                    break;
                case ast::BinaryOp::RShift:
                    ctx.os << ">>";
                    break;
                case ast::BinaryOp::Lt:
                    ctx.os << "<";
                    break;
                case ast::BinaryOp::Gt:
                    ctx.os << ">";
                    break;
                case ast::BinaryOp::Lte:
                    ctx.os << "<=";
                    break;
                case ast::BinaryOp::Gte:
                    ctx.os << ">=";
                    break;
                case ast::BinaryOp::Eq:
                    ctx.os << "==";
                    break;
                case ast::BinaryOp::Neq:
                    ctx.os << "!=";
                    break;
                case ast::BinaryOp::BitAnd:
                    ctx.os << "&";
                    break;
                case ast::BinaryOp::BitXor:
                    ctx.os << "^";
                    break;
                case ast::BinaryOp::BitOr:
                    ctx.os << "|";
                    break;
                case ast::BinaryOp::And:
                    ctx.os << "&&";
                    break;
                case ast::BinaryOp::Or:
                    ctx.os << "||";
                    break;
                }
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "left:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.left);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "right:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.right);
                ctx.indent_level--;
            },
            [&](const typed::Assignment &arg) {
                ctx.indent();
                ctx.os << Color::RED << "Assignment" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "target:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.target);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "value:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.value);
                ctx.indent_level--;
            },
            [&](const typed::IfExpr &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "IfExpr" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                for (size_t i = 0; i < arg.branches.size(); i++) {
                    ctx.indent();
                    ctx.os << Color::GRAY << (i == 0 ? "if:" : "else if:") << Color::RESET
                       << "\n";
                    ctx.indent_level++;
                    ctx.indent();
                    ctx.os << Color::GRAY << "cond:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_expr(ctx, *arg.branches[i].condition);
                    ctx.indent_level--;
                    ctx.indent();
                    ctx.os << Color::GRAY << "body:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_block(ctx, arg.branches[i].body);
                    ctx.indent_level--;
                    ctx.indent_level--;
                }
                if (arg.else_branch) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "else:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_block(ctx, *arg.else_branch);
                    ctx.indent_level--;
                }
            },
            [&](const typed::MatchExpr &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "MatchExpr" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "subject:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.subject);
                ctx.indent_level--;
                for (auto &arm : arg.arms) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "arm:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_pattern(ctx, arm.pattern);
                    dump_block(ctx, arm.body);
                    ctx.indent_level--;
                }
            },
            [&](const typed::Break &arg) {
                ctx.indent();
                ctx.os << Color::RED << "Break" << Color::RESET;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                if (arg.value) {
                    ctx.indent_level++;
                    dump_expr(ctx, **arg.value);
                    ctx.indent_level--;
                }
            },
            [&](const typed::Continue &arg) {
                ctx.indent();
                ctx.os << Color::RED << "Continue" << Color::RESET;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
            },
            [&](const typed::BuiltinCall &arg) {
                ctx.indent();
                ctx.os << Color::CYAN << "BuiltinCall" << Color::RESET << " @" << arg.name;
                if (!arg.type_args.empty()) {
                    ctx.os << "[";
                    for (size_t i = 0; i < arg.type_args.size(); i++) {
                        if (i > 0)
                            ctx.os << ", ";
                        dump_type(ctx, arg.type_args[i]);
                    }
                    ctx.os << "]";
                }
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                for (auto &a : arg.args) {
                    ctx.indent_level++;
                    dump_expr(ctx, *a);
                    ctx.indent_level--;
                }
            },
            [&](const typed::RangeExpr &arg) {
                ctx.indent();
                ctx.os << Color::BLUE << "RangeExpr" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "start:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.start);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "end:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.end);
                ctx.indent_level--;
            },
            [&](const typed::TypeInit &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "TypeInit" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.ty);
                ctx.os << "\n";
                for (auto &val : arg.fields) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "field:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_expr(ctx, *val);
                    ctx.indent_level--;
                }
            },
            [&](const typed::AsExpr &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "AsExpr" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "expr:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.expr);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "target_ty:" << Color::RESET << " ";
                dump_type(ctx, arg.target_ty);
                ctx.os << "\n";
            },
            [&](const typed::ComptimeExpr &arg) {
                ctx.indent();
                ctx.os << Color::CYAN << "ComptimeExpr" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, ty);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.expr);
                ctx.indent_level--;
            }
        },
        expr.value);
}

static void dump_pattern(PrinterContext &ctx, const typed::TypedPattern &pat) {
    ctx.indent();
    ctx.os << Color::GREEN << "TypedPattern" << Color::RESET;
    ctx.print_span(pat.span);
    ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
    dump_type(ctx, pat.ty);
    ctx.os << "\n";

    visit(
        overload{
            [&](const ast::VariantPattern &arg) {
                ctx.indent();
                ctx.os << Color::GREEN << "VariantPattern" << Color::RESET << " "
                   << arg.variant;
                if (arg.binding)
                    ctx.os << "(" << *arg.binding << ")";
                ctx.print_span(arg.span);
                ctx.os << "\n";
            },
            [&](const ast::LiteralPattern &arg) {
                ctx.indent();
                ctx.os << Color::GREEN << "LiteralPattern" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_literal_pattern_value(ctx, arg);
                ctx.indent_level--;
            },
            [&](const ast::WildcardPattern &arg) {
                ctx.indent();
                ctx.os << Color::GREEN << "WildcardPattern" << Color::RESET << " _";
                ctx.print_span(arg.span);
                ctx.os << "\n";
            },
            [&](const ast::IdentPattern &arg) {
                ctx.indent();
                ctx.os << Color::GREEN << "IdentPattern" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << "\n";
            }
        },
        pat.value);
}

static void dump_block(PrinterContext &ctx, const typed::TypedBlock &block) {
    ctx.indent();
    ctx.os << Color::GRAY << "TypedBlock" << Color::RESET;
    ctx.print_span(block.span);
    ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
    dump_type(ctx, block.ty);
    ctx.os << "\n";
    for (auto &stmt : block.stmts) {
        ctx.indent_level++;
        dump_stmt(ctx, *stmt);
        ctx.indent_level--;
    }
}

static void dump_stmt(PrinterContext &ctx, const typed::TypedStmt &stmt) {
    visit(
        overload{
            [&](const typed::LetStmt &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_BLUE << "LetStmt" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "init:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.init);
                ctx.indent_level--;
            },
            [&](const typed::ReturnStmt &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_RED << "ReturnStmt" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                if (arg.value) {
                    ctx.indent_level++;
                    dump_expr(ctx, **arg.value);
                    ctx.indent_level--;
                }
            },
            [&](const typed::DeferStmt &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_YELLOW << "DeferStmt" << Color::RESET;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_stmt(ctx, *arg.stmt);
                ctx.indent_level--;
            },
            [&](const typed::LoopStmt &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "LoopStmt" << Color::RESET;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_block(ctx, arg.body);
                ctx.indent_level--;
            },
            [&](const typed::WhileStmt &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "WhileStmt" << Color::RESET;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "cond:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.condition);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "body:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_block(ctx, arg.body);
                ctx.indent_level--;
            },
            [&](const typed::ForStmt &arg) {
                ctx.indent();
                ctx.os << Color::MAGENTA << "ForStmt" << Color::RESET << " " << arg.var;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.var_ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "iterable:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.iterable);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "body:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_block(ctx, arg.body);
                ctx.indent_level--;
            },
            [&](const unique_ptr<typed::TypedExpr> &arg) {
                ctx.indent();
                ctx.os << Color::GRAY << "ExprStmt" << Color::RESET;
                ctx.print_span(arg->span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg->ty);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg);
                ctx.indent_level--;
            }
        },
        stmt.value);
}

static void dump_fn(PrinterContext &ctx, const typed::TypedFnDecl &fn) {
    ctx.indent();
    ctx.os << Color::BOLD_GREEN << "TypedFnDecl" << Color::RESET << " " << fn.name;
    ctx.print_span(fn.span);
    ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
    dump_type(ctx, fn.ty);
    ctx.os << "\n";
    if (!fn.params.empty()) {
        ctx.indent();
        ctx.os << Color::GRAY << "params:" << Color::RESET << "\n";
        ctx.indent_level++;
        for (auto &p : fn.params) {
            ctx.indent();
            ctx.os << p.name << ": ";
            dump_type(ctx, p.ty);
            ctx.os << "\n";
        }
        ctx.indent_level--;
    }
    ctx.indent();
    ctx.os << Color::GRAY << "return_type:" << Color::RESET << " ";
    dump_type(ctx, fn.return_type);
    ctx.os << "\n";
    ctx.indent();
    ctx.os << Color::GRAY << "body:" << Color::RESET << "\n";
    ctx.indent_level++;
    dump_block(ctx, fn.body);
    ctx.indent_level--;
}

[[maybe_unused]] static void dump_comptime_stmt(PrinterContext &ctx, const typed::ComptimeStmt &stmt) {
    visit(
        overload{
            [&](const typed::LetStmt &arg) {
                ctx.indent();
                ctx.os << Color::CYAN << "ComptimeLet" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "init:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.init);
                ctx.indent_level--;
            },
            [&](const unique_ptr<typed::TypedExpr> &arg) {
                ctx.indent();
                ctx.os << Color::CYAN << "ComptimeIf" << Color::RESET;
                ctx.print_span(arg->span);
                ctx.os << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg);
                ctx.indent_level--;
            },
            [&](const typed::ForStmt &arg) {
                ctx.indent();
                ctx.os << Color::CYAN << "ComptimeFor" << Color::RESET << " " << arg.var;
                if (arg.label)
                    ctx.os << " :" << *arg.label;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.var_ty);
                ctx.os << "\n";
                ctx.indent();
                ctx.os << Color::GRAY << "iterable:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_expr(ctx, *arg.iterable);
                ctx.indent_level--;
                ctx.indent();
                ctx.os << Color::GRAY << "body:" << Color::RESET << "\n";
                ctx.indent_level++;
                dump_block(ctx, arg.body);
                ctx.indent_level--;
            }
        },
        stmt.stmt);
}

static void dump_decl(PrinterContext &ctx, const typed::TypedDecl &decl) {
    visit(
        overload{
            [&](const typed::TypedFnDecl &arg) {
                dump_fn(ctx, arg);
            },
            [&](const typed::TypedStructDecl &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_GREEN << "TypedStructDecl" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.ty);
                ctx.os << "\n";
                for (auto &f : arg.fields) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "field " << f.name << ":" << Color::RESET
                       << "\n";
                    ctx.indent_level++;
                    dump_type(ctx, f.ty);
                    ctx.indent_level--;
                }
                for (auto &m : arg.methods)
                    dump_fn(ctx, m);
            },
            [&](const typed::TypedUnionDecl &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_GREEN << "TypedUnionDecl" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << Color::GRAY << " ty:" << Color::RESET << " ";
                dump_type(ctx, arg.ty);
                ctx.os << "\n";
                for (auto &v : arg.variants) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "variant " << v.name << ":" << Color::RESET
                       << "\n";
                    ctx.indent_level++;
                    if (v.ty) {
                        dump_type(ctx, *v.ty);
                    } else {
                        ctx.indent();
                        ctx.os << Color::GRAY << "<unit>" << Color::RESET;
                        ctx.os << "\n";
                    }
                    ctx.indent_level--;
                }
                for (auto &m : arg.methods)
                    dump_fn(ctx, m);
            },
            [&](const typed::TypedInterfaceDecl &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_GREEN << "TypedInterfaceDecl" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                for (auto &m : arg.methods) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "method " << m.name << Color::RESET;
                    ctx.print_span(m.span);
                    ctx.os << "\n";
                    ctx.indent_level++;
                    for (auto &p : m.params) {
                        ctx.indent();
                        ctx.os << p.name << ": ";
                        dump_type(ctx, p.ty);
                        ctx.os << "\n";
                    }
                    ctx.indent();
                    ctx.os << Color::GRAY << "-> " << Color::RESET;
                    dump_type(ctx, m.return_type);
                    ctx.os << "\n";
                    ctx.indent_level--;
                }
            },
            [&](const typed::TypedExternDecl &arg) {
                ctx.indent();
                ctx.os << Color::BOLD_GREEN << "TypedExternDecl" << Color::RESET << " "
                   << arg.name;
                ctx.print_span(arg.span);
                ctx.os << "\n";
                ctx.indent_level++;
                for (auto &p : arg.params) {
                    ctx.indent();
                    ctx.os << Color::GRAY << "param:" << Color::RESET << "\n";
                    ctx.indent_level++;
                    dump_type(ctx, p);
                    ctx.indent_level--;
                }
                ctx.indent();
                ctx.os << Color::GRAY << "-> " << Color::RESET << " ";
                dump_type(ctx, arg.return_type);
                ctx.os << "\n";
                ctx.indent_level--;
            },
            [&](const typed::ComptimeStmt &arg) {
                dump_comptime_stmt(ctx, arg);
            }
        },
        decl.value);
}

void dump_typed_ast(const typed::TypedProgram &program, ostream &os) {
    PrinterContext ctx(os);
    ctx.os << Color::BOLD << "TypedProgram" << Color::RESET;
    ctx.os << "\n";
    ctx.indent_level = 1;
    for (auto &decl : program.declarations)
        dump_decl(ctx, decl);
}

} // namespace shikimori
