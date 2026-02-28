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

using namespace std;

namespace shikimori {

static int typed_indent_level = 0;

static void typed_indent(ostream &os) {
  for (int i = 0; i < typed_indent_level; i++)
    os << "  ";
}

static void print_span(ostream &os, const Span &span) {
  os << Color::GRAY << " [" << span.start << ".." << span.end << "]"
     << Color::RESET;
}

static void dump_type(const TypeRef &type, ostream &os);
static void dump_expr(const typed::TypedExpr &expr, ostream &os);
static void dump_stmt(const typed::TypedStmt &stmt, ostream &os);
static void dump_block(const typed::TypedBlock &block, ostream &os);
static void dump_pattern(const typed::TypedPattern &pat, ostream &os);
static void dump_decl(const typed::TypedDecl &decl, ostream &os);

static void dump_type(const TypeRef &type, ostream &os) {
  if (!type) {
    typed_indent(os);
    os << Color::GRAY << "<null type>" << Color::RESET << "\n";
    return;
  }

  visit(
      [&](auto &&ty) {
        using T = decay_t<decltype(ty)>;
        if constexpr (is_same_v<T, TyVar>) {
          typed_indent(os);
          os << Color::CYAN << "TyVar" << Color::RESET << " " << ty.name << " #"
             << ty.id;
          os << "\n";
        } else if constexpr (is_same_v<T, ETVar>) {
          typed_indent(os);
          os << Color::CYAN << "ETVar" << Color::RESET << " " << ty.name << " #"
             << ty.id;
          os << "\n";
        } else if constexpr (is_same_v<T, TyNamed>) {
          typed_indent(os);
          os << Color::CYAN << "TyNamed" << Color::RESET << " " << ty.name;
          switch (ty.kind) {
          case NamedTyKind::Struct:
            os << Color::GRAY << " (struct)" << Color::RESET;
            break;
          case NamedTyKind::Union:
            os << Color::GRAY << " (union)" << Color::RESET;
            break;
          case NamedTyKind::Primitive:
            os << Color::GRAY << " (primitive)" << Color::RESET;
            break;
          case NamedTyKind::Pointer:
            os << Color::GRAY << " (pointer)" << Color::RESET;
            break;
          case NamedTyKind::Slice:
            os << Color::GRAY << " (slice)" << Color::RESET;
            break;
          case NamedTyKind::Interface:
            os << Color::GRAY << " (interface)" << Color::RESET;
            break;
          }
          if (!ty.args.empty()) {
            os << "[";
            for (size_t i = 0; i < ty.args.size(); i++) {
              if (i > 0)
                os << ", ";
              dump_type(ty.args[i], os);
            }
            os << "]";
          }
          os << "\n";
        } else if constexpr (is_same_v<T, FnTy>) {
          typed_indent(os);
          os << Color::CYAN << "FnTy" << Color::RESET << "(";
          for (size_t i = 0; i < ty.args.size(); i++) {
            if (i > 0)
              os << ", ";
            dump_type(ty.args[i], os);
          }
          os << ") -> ";
          dump_type(ty.return_type, os);
          os << "\n";
        } else if constexpr (is_same_v<T, ForAll>) {
          typed_indent(os);
          os << Color::CYAN << "ForAll" << Color::RESET << "[";
          for (size_t i = 0; i < ty.vars.size(); i++) {
            if (i > 0)
              os << ", ";
            os << ty.vars[i].first << " #" << ty.vars[i].second;
          }
          os << "] ";
          dump_type(ty.body, os);
          os << "\n";
        } else if constexpr (is_same_v<T, TyArray>) {
          typed_indent(os);
          os << Color::CYAN << "TyArray" << Color::RESET << "[" << ty.size
             << "]";
          os << "\n";
          typed_indent_level++;
          dump_type(ty.inner, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, TyInterfaceObj>) {
          typed_indent(os);
          os << Color::CYAN << "TyInterfaceObj" << Color::RESET << " { ";
          for (size_t i = 0; i < ty.interfaces.size(); i++) {
            if (i > 0)
              os << " + ";
            os << ty.interfaces[i];
          }
          os << " }";
          os << "\n";
          typed_indent_level++;
          dump_type(ty.data_ty, os);
          typed_indent_level--;
        }
      },
      type->ty);
}

static void dump_expr_inner(const auto &arg, ostream &os, const TypeRef &ty);

static void dump_expr(const typed::TypedExpr &expr, ostream &os) {
  visit([&](auto &&arg) { dump_expr_inner(arg, os, expr.ty); }, expr.value);
}

static void dump_expr_inner(const auto &arg, ostream &os, const TypeRef &ty) {
  using T = decay_t<decltype(arg)>;
  if constexpr (is_same_v<T, typed::IntLiteral>) {
    typed_indent(os);
    os << Color::YELLOW << "IntLiteral" << Color::RESET << " " << arg.value;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::FloatLiteral>) {
    typed_indent(os);
    os << Color::YELLOW << "FloatLiteral" << Color::RESET << " " << arg.value;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::BoolLiteral>) {
    typed_indent(os);
    os << Color::YELLOW << "BoolLiteral" << Color::RESET << " "
       << (arg.value ? "true" : "false");
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::StringLiteral>) {
    typed_indent(os);
    os << Color::GREEN << "StringLiteral" << Color::RESET << " \"" << arg.value
       << "\"";
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::NullLiteral>) {
    typed_indent(os);
    os << Color::YELLOW << "NullLiteral" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::IdentifierExpr>) {
    typed_indent(os);
    os << Color::WHITE << "IdentifierExpr" << Color::RESET << " " << arg.name;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(ty, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::StructInit>) {
    typed_indent(os);
    os << Color::MAGENTA << "StructInit" << Color::RESET << " " << arg.name;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    for (auto &[name, val] : arg.fields) {
      typed_indent(os);
      os << Color::GRAY << name << ":" << Color::RESET << "\n";
      typed_indent_level++;
      dump_expr(*val, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::ScopeAccess>) {
    typed_indent(os);
    os << Color::MAGENTA << "ScopeAccess" << Color::RESET << " " << arg.scope
       << "::" << arg.member;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    for (auto &p : arg.payload) {
      dump_expr(*p, os);
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::UnionVariantInit>) {
    typed_indent(os);
    os << Color::MAGENTA << "UnionVariantInit" << Color::RESET << " "
       << arg.union_name << "::" << arg.variant;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    for (auto &p : arg.payload) {
      dump_expr(*p, os);
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::FieldAccess>) {
    typed_indent(os);
    os << Color::BLUE << "FieldAccess" << Color::RESET << " ." << arg.field;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "object:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.object, os);
    typed_indent_level--;
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::MethodCall>) {
    typed_indent(os);
    os << Color::BLUE << "MethodCall" << Color::RESET << " ." << arg.method
       << "()";
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "object:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.object, os);
    typed_indent_level--;
    for (auto &a : arg.args) {
      typed_indent(os);
      os << Color::GRAY << "arg:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_expr(*a, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::IndexAccess>) {
    typed_indent(os);
    os << Color::BLUE << "IndexAccess" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "object:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.object, os);
    typed_indent_level--;
    typed_indent(os);
    os << Color::GRAY << "index:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.index, os);
    typed_indent_level--;
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::Call>) {
    typed_indent(os);
    os << Color::BLUE << "Call" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "callee:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.callee, os);
    typed_indent_level--;
    for (auto &a : arg.args) {
      typed_indent(os);
      os << Color::GRAY << "arg:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_expr(*a, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::UnaryExpr>) {
    typed_indent(os);
    os << Color::BLUE << "UnaryExpr" << Color::RESET << " ";
    switch (arg.op) {
    case ast::UnaryOp::Not:
      os << "!";
      break;
    case ast::UnaryOp::Neg:
      os << "-";
      break;
    case ast::UnaryOp::Deref:
      os << "*";
      break;
    case ast::UnaryOp::AddrOf:
      os << "&";
      break;
    case ast::UnaryOp::BitNot:
      os << "~";
      break;
    }
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    dump_expr(*arg.operand, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::BinaryExpr>) {
    typed_indent(os);
    os << Color::BLUE << "BinaryExpr" << Color::RESET << " ";
    switch (arg.op) {
    case ast::BinaryOp::Add:
      os << "+";
      break;
    case ast::BinaryOp::Sub:
      os << "-";
      break;
    case ast::BinaryOp::Mul:
      os << "*";
      break;
    case ast::BinaryOp::Div:
      os << "/";
      break;
    case ast::BinaryOp::Mod:
      os << "%";
      break;
    case ast::BinaryOp::LShift:
      os << "<<";
      break;
    case ast::BinaryOp::RShift:
      os << ">>";
      break;
    case ast::BinaryOp::Lt:
      os << "<";
      break;
    case ast::BinaryOp::Gt:
      os << ">";
      break;
    case ast::BinaryOp::Lte:
      os << "<=";
      break;
    case ast::BinaryOp::Gte:
      os << ">=";
      break;
    case ast::BinaryOp::Eq:
      os << "==";
      break;
    case ast::BinaryOp::Neq:
      os << "!=";
      break;
    case ast::BinaryOp::BitAnd:
      os << "&";
      break;
    case ast::BinaryOp::BitXor:
      os << "^";
      break;
    case ast::BinaryOp::BitOr:
      os << "|";
      break;
    case ast::BinaryOp::And:
      os << "&&";
      break;
    case ast::BinaryOp::Or:
      os << "||";
      break;
    }
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "left:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.left, os);
    typed_indent_level--;
    typed_indent(os);
    os << Color::GRAY << "right:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.right, os);
    typed_indent_level--;
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::Assignment>) {
    typed_indent(os);
    os << Color::RED << "Assignment" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "target:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.target, os);
    typed_indent_level--;
    typed_indent(os);
    os << Color::GRAY << "value:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.value, os);
    typed_indent_level--;
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::IfExpr>) {
    typed_indent(os);
    os << Color::MAGENTA << "IfExpr" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    for (size_t i = 0; i < arg.branches.size(); i++) {
      typed_indent(os);
      os << Color::GRAY << (i == 0 ? "if:" : "else if:") << Color::RESET
         << "\n";
      typed_indent_level++;
      typed_indent(os);
      os << Color::GRAY << "cond:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_expr(*arg.branches[i].condition, os);
      typed_indent_level--;
      typed_indent(os);
      os << Color::GRAY << "body:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_block(arg.branches[i].body, os);
      typed_indent_level--;
      typed_indent_level--;
    }
    if (arg.else_branch) {
      typed_indent(os);
      os << Color::GRAY << "else:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_block(*arg.else_branch, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::MatchExpr>) {
    typed_indent(os);
    os << Color::MAGENTA << "MatchExpr" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "subject:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.subject, os);
    typed_indent_level--;
    for (auto &arm : arg.arms) {
      typed_indent(os);
      os << Color::GRAY << "arm:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_pattern(arm.pattern, os);
      dump_block(arm.body, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::Break>) {
    typed_indent(os);
    os << Color::RED << "Break" << Color::RESET;
    if (arg.label)
      os << " :" << *arg.label;
    print_span(os, arg.span);
    os << "\n";
    if (arg.value) {
      typed_indent_level++;
      dump_expr(**arg.value, os);
      typed_indent_level--;
    }
  } else if constexpr (is_same_v<T, typed::Continue>) {
    typed_indent(os);
    os << Color::RED << "Continue" << Color::RESET;
    if (arg.label)
      os << " :" << *arg.label;
    print_span(os, arg.span);
    os << "\n";
  } else if constexpr (is_same_v<T, typed::BuiltinCall>) {
    typed_indent(os);
    os << Color::CYAN << "BuiltinCall" << Color::RESET << " @" << arg.name;
    if (!arg.type_args.empty()) {
      os << "[";
      for (size_t i = 0; i < arg.type_args.size(); i++) {
        if (i > 0)
          os << ", ";
        dump_type(arg.type_args[i], os);
      }
      os << "]";
    }
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    for (auto &a : arg.args)
      dump_expr(*a, os);
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::RangeExpr>) {
    typed_indent(os);
    os << Color::BLUE << "RangeExpr" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "start:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.start, os);
    typed_indent_level--;
    typed_indent(os);
    os << Color::GRAY << "end:" << Color::RESET << "\n";
    typed_indent_level++;
    dump_expr(*arg.end, os);
    typed_indent_level--;
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::TypeInit>) {
    typed_indent(os);
    os << Color::MAGENTA << "TypeInit" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    typed_indent(os);
    os << Color::GRAY << "ty:" << Color::RESET << " ";
    dump_type(arg.ty, os);
    typed_indent_level--;
    for (auto &val : arg.fields) {
      typed_indent(os);
      os << Color::GRAY << "field:" << Color::RESET << "\n";
      typed_indent_level++;
      dump_expr(*val, os);
      typed_indent_level--;
    }
    typed_indent_level--;
  } else if constexpr (is_same_v<T, typed::ComptimeExpr>) {
    typed_indent(os);
    os << Color::CYAN << "ComptimeExpr" << Color::RESET;
    print_span(os, arg.span);
    os << "\n";
    typed_indent_level++;
    dump_expr(*arg.expr, os);
    typed_indent_level--;
  }
}

static void dump_pattern(const typed::TypedPattern &pat, ostream &os) {
  typed_indent(os);
  os << Color::GREEN << "TypedPattern" << Color::RESET;
  print_span(os, pat.span);
  os << "\n";
  typed_indent_level++;
  typed_indent(os);
  os << Color::GRAY << "ty:" << Color::RESET << " ";
  dump_type(pat.ty, os);
  typed_indent_level--;

  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::VariantPattern>) {
          typed_indent(os);
          os << Color::GREEN << "VariantPattern" << Color::RESET << " "
             << arg.variant;
          if (arg.binding)
            os << "(" << *arg.binding << ")";
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::LiteralPattern>) {
          typed_indent(os);
          os << Color::GREEN << "LiteralPattern" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::WildcardPattern>) {
          typed_indent(os);
          os << Color::GREEN << "WildcardPattern" << Color::RESET << " _";
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::IdentPattern>) {
          typed_indent(os);
          os << Color::GREEN << "IdentPattern" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
        }
      },
      pat.value);
}

static void dump_block(const typed::TypedBlock &block, ostream &os) {
  typed_indent(os);
  os << Color::GRAY << "TypedBlock" << Color::RESET;
  print_span(os, block.span);
  os << "\n";
  typed_indent_level++;
  typed_indent(os);
  os << Color::GRAY << "ty:" << Color::RESET << " ";
  dump_type(block.ty, os);
  typed_indent_level--;
  for (auto &stmt : block.stmts)
    dump_stmt(*stmt, os);
}

static void dump_stmt(const typed::TypedStmt &stmt, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, typed::LetStmt>) {
          typed_indent(os);
          os << Color::BOLD_BLUE << "LetStmt" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          typed_indent(os);
          os << Color::GRAY << "ty:" << Color::RESET << " ";
          dump_type(arg.ty, os);
          typed_indent_level--;
          typed_indent(os);
          os << Color::GRAY << "init:" << Color::RESET << "\n";
          typed_indent_level++;
          dump_expr(*arg.init, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::ReturnStmt>) {
          typed_indent(os);
          os << Color::BOLD_RED << "ReturnStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          if (arg.value) {
            typed_indent_level++;
            dump_expr(**arg.value, os);
            typed_indent_level--;
          }
        } else if constexpr (is_same_v<T, typed::DeferStmt>) {
          typed_indent(os);
          os << Color::BOLD_YELLOW << "DeferStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          dump_stmt(*arg.stmt, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::LoopStmt>) {
          typed_indent(os);
          os << Color::MAGENTA << "LoopStmt" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          dump_block(arg.body, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::WhileStmt>) {
          typed_indent(os);
          os << Color::MAGENTA << "WhileStmt" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          typed_indent(os);
          os << Color::GRAY << "cond:" << Color::RESET << "\n";
          typed_indent_level++;
          dump_expr(*arg.condition, os);
          typed_indent_level--;
          typed_indent(os);
          os << Color::GRAY << "body:" << Color::RESET << "\n";
          typed_indent_level++;
          dump_block(arg.body, os);
          typed_indent_level--;
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::ForStmt>) {
          typed_indent(os);
          os << Color::MAGENTA << "ForStmt" << Color::RESET << " " << arg.var;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          typed_indent(os);
          os << Color::GRAY << "var_ty:" << Color::RESET << " ";
          dump_type(arg.var_ty, os);
          typed_indent_level--;
          typed_indent(os);
          os << Color::GRAY << "iterable:" << Color::RESET << "\n";
          typed_indent_level++;
          dump_expr(*arg.iterable, os);
          typed_indent_level--;
          typed_indent(os);
          os << Color::GRAY << "body:" << Color::RESET << "\n";
          typed_indent_level++;
          dump_block(arg.body, os);
          typed_indent_level--;
          typed_indent_level--;
        } else if constexpr (is_same_v<T, unique_ptr<typed::TypedExpr>>) {
          typed_indent(os);
          os << Color::GRAY << "ExprStmt" << Color::RESET;
          print_span(os, arg->span);
          os << "\n";
          typed_indent_level++;
          dump_expr(*arg, os);
          typed_indent_level--;
        }
      },
      stmt.value);
}

static void dump_fn(const typed::TypedFnDecl &fn, ostream &os) {
  typed_indent(os);
  os << Color::BOLD_GREEN << "TypedFnDecl" << Color::RESET << " " << fn.name;
  print_span(os, fn.span);
  os << "\n";
  typed_indent_level++;
  typed_indent(os);
  os << Color::GRAY << "ty:" << Color::RESET << " ";
  dump_type(fn.ty, os);
  typed_indent_level--;
  if (!fn.params.empty()) {
    typed_indent(os);
    os << Color::GRAY << "params:" << Color::RESET << "\n";
    typed_indent_level++;
    for (auto &p : fn.params) {
      typed_indent(os);
      os << p.name;
      os << ": ";
      dump_type(p.ty, os);
      os << "\n";
    }
    typed_indent_level--;
  }
  typed_indent(os);
  os << Color::GRAY << "return_type:" << Color::RESET << "\n";
  typed_indent_level++;
  dump_type(fn.return_type, os);
  typed_indent_level--;
  typed_indent(os);
  os << Color::GRAY << "body:" << Color::RESET << "\n";
  typed_indent_level++;
  dump_block(fn.body, os);
  typed_indent_level--;
  typed_indent_level--;
}

static void dump_decl(const typed::TypedDecl &decl, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, typed::TypedFnDecl>) {
          dump_fn(arg, os);
        } else if constexpr (is_same_v<T, typed::TypedStructDecl>) {
          typed_indent(os);
          os << Color::BOLD_GREEN << "TypedStructDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          typed_indent(os);
          os << Color::GRAY << "ty:" << Color::RESET << " ";
          dump_type(arg.ty, os);
          typed_indent_level--;
          for (auto &f : arg.fields) {
            typed_indent(os);
            os << Color::GRAY << "field " << f.name << ":" << Color::RESET
               << "\n";
            typed_indent_level++;
            dump_type(f.ty, os);
            typed_indent_level--;
          }
          for (auto &m : arg.methods)
            dump_fn(m, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::TypedUnionDecl>) {
          typed_indent(os);
          os << Color::BOLD_GREEN << "TypedUnionDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          typed_indent(os);
          os << Color::GRAY << "ty:" << Color::RESET << " ";
          dump_type(arg.ty, os);
          typed_indent_level--;
          for (auto &v : arg.variants) {
            typed_indent(os);
            os << Color::GRAY << "variant " << v.name << ":" << Color::RESET
               << "\n";
            typed_indent_level++;
            if (v.ty) {
              dump_type(*v.ty, os);
            } else {
              typed_indent(os);
              os << Color::GRAY << "<unit>" << Color::RESET;
              os << "\n";
            }
            typed_indent_level--;
          }
          for (auto &m : arg.methods)
            dump_fn(m, os);
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::TypedInterfaceDecl>) {
          typed_indent(os);
          os << Color::BOLD_GREEN << "TypedInterfaceDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          for (auto &m : arg.methods) {
            typed_indent(os);
            os << Color::GRAY << "method " << m.name << Color::RESET;
            print_span(os, m.span);
            os << "\n";
            typed_indent_level++;
            for (auto &p : m.params) {
              typed_indent(os);
              os << p.name << ": ";
              dump_type(p.ty, os);
              os << "\n";
            }
            typed_indent(os);
            os << Color::GRAY << "-> " << Color::RESET;
            dump_type(m.return_type, os);
            os << "\n";
            typed_indent_level--;
          }
          typed_indent_level--;
        } else if constexpr (is_same_v<T, typed::TypedExternDecl>) {
          typed_indent(os);
          os << Color::BOLD_GREEN << "TypedExternDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          typed_indent_level++;
          for (auto &p : arg.params) {
            typed_indent(os);
            os << Color::GRAY << "param:" << Color::RESET << "\n";
            typed_indent_level++;
            dump_type(p, os);
            typed_indent_level--;
          }
          typed_indent(os);
          os << Color::GRAY << "-> " << Color::RESET << "\n";
          typed_indent_level++;
          dump_type(arg.return_type, os);
          typed_indent_level--;
          typed_indent_level--;
        }
      },
      decl.value);
}

void dump_typed_ast(const typed::TypedProgram &program, ostream &os) {
  os << Color::BOLD << "TypedProgram" << Color::RESET;
  os << "\n";
  typed_indent_level = 0;
  typed_indent_level++;
  for (auto &decl : program.declarations)
    dump_decl(decl, os);
  typed_indent_level--;
}

} // namespace shikimori
