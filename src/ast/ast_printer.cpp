#include "ast_printer.h"
#include <memory>
#include <optional>
#include <stddef.h>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "ast/ast.h"
#include "color.hpp"
#include "span.h"

using namespace std;

namespace shikimori {

static int indent_level = 0;

static void indent(ostream &os) {
  for (int i = 0; i < indent_level; i++)
    os << "  ";
}

static void print_span(ostream &os, const Span &span) {
  os << Color::GRAY << " [" << span.start << ".." << span.end << "]"
     << Color::RESET;
}

static void dump_type(const ast::TypeAnnot &type, ostream &os);
static void dump_expr(const ast::Expr &expr, ostream &os);
static void dump_stmt(const ast::Stmt &stmt, ostream &os);
static void dump_block(const ast::Block &block, ostream &os);
static void dump_pattern(const ast::Pattern &pat, ostream &os);

static void dump_type(const ast::TypeAnnot &type, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::PrimitiveType>) {
          indent(os);
          os << Color::CYAN << "PrimitiveType" << Color::RESET << " ";
          switch (arg) {
          case ast::PrimitiveType::I8:
            os << "i8";
            break;
          case ast::PrimitiveType::I16:
            os << "i16";
            break;
          case ast::PrimitiveType::I32:
            os << "i32";
            break;
          case ast::PrimitiveType::I64:
            os << "i64";
            break;
          case ast::PrimitiveType::U8:
            os << "u8";
            break;
          case ast::PrimitiveType::U16:
            os << "u16";
            break;
          case ast::PrimitiveType::U32:
            os << "u32";
            break;
          case ast::PrimitiveType::U64:
            os << "u64";
            break;
          case ast::PrimitiveType::F32:
            os << "f32";
            break;
          case ast::PrimitiveType::F64:
            os << "f64";
            break;
          case ast::PrimitiveType::Bool:
            os << "bool";
            break;
          case ast::PrimitiveType::Usize:
            os << "usize";
            break;
          case ast::PrimitiveType::String:
            os << "string";
            break;
          }
          print_span(os, type.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::PointerTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "PointerType" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_type(*arg.inner, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::SliceTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "SliceType" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_type(*arg.inner, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ArrayTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "ArrayType" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "size:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.size, os);
          indent_level--;
          dump_type(*arg.inner, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::OptionalTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "OptionalType" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_type(*arg.inner, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::InterfaceTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "InterfaceType" << Color::RESET << " ";
          for (size_t i = 0; i < arg.names.size(); i++) {
            if (i > 0)
              os << " + ";
            os << arg.names[i];
          }
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::NamedTypeAnnot>) {
          indent(os);
          os << Color::CYAN << "NamedType" << Color::RESET << " " << arg.name;
          print_span(os, arg.span);
          os << "\n";
          if (!arg.generic_args.empty()) {
            indent_level++;
            for (auto &ga : arg.generic_args)
              dump_type(*ga, os);
            indent_level--;
          }
        }
      },
      type.value);
}

static void dump_expr(const ast::Expr &expr, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::IntLiteral>) {
          indent(os);
          os << Color::YELLOW << "IntLiteral" << Color::RESET << " "
             << arg.value;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::FloatLiteral>) {
          indent(os);
          os << Color::YELLOW << "FloatLiteral" << Color::RESET << " "
             << arg.value;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::BoolLiteral>) {
          indent(os);
          os << Color::YELLOW << "BoolLiteral" << Color::RESET << " "
             << (arg.value ? "true" : "false");
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::StringLiteral>) {
          indent(os);
          os << Color::GREEN << "StringLiteral" << Color::RESET << " \""
             << arg.value << "\"";
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::NullLiteral>) {
          indent(os);
          os << Color::YELLOW << "NullLiteral" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::IdentifierExpr>) {
          indent(os);
          os << Color::WHITE << "Ident" << Color::RESET << " " << arg.name;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::GenericIdent>) {
          indent(os);
          os << Color::WHITE << "GenericIdent" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &ta : arg.type_args)
            dump_type(*ta, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::StructInit>) {
          indent(os);
          os << Color::MAGENTA << "StructInit" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &[name, val] : arg.fields) {
            indent(os);
            os << Color::GRAY << name << ":" << Color::RESET << "\n";
            indent_level++;
            dump_expr(*val, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ScopeAccess>) {
          indent(os);
          os << Color::MAGENTA << "ScopeAccess" << Color::RESET << " "
             << arg.scope << "::" << arg.member;
          if (!arg.generic_args.empty()) {
            os << "[";
            for (size_t i = 0; i < arg.generic_args.size(); i++) {
              if (i > 0)
                os << ", ";
              dump_type(*arg.generic_args[i], os);
            }
            os << "]";
          }
          print_span(os, arg.span);
          os << "\n";
          if (!arg.payload.empty()) {
            indent_level++;
            for (auto &p : arg.payload) {
              dump_expr(*p, os);
            }
            indent_level--;
          }
        } else if constexpr (is_same_v<T, ast::FieldAccess>) {
          indent(os);
          os << Color::BLUE << "FieldAccess" << Color::RESET << " ."
             << arg.field;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_expr(*arg.object, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::MethodCall>) {
          indent(os);
          os << Color::BLUE << "MethodCall" << Color::RESET << " ."
             << arg.method << "()";
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "object:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.object, os);
          indent_level--;
          for (auto &a : arg.args) {
            indent(os);
            os << Color::GRAY << "arg:" << Color::RESET << "\n";
            indent_level++;
            dump_expr(*a, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::IndexAccess>) {
          indent(os);
          os << Color::BLUE << "IndexAccess" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_expr(*arg.object, os);
          dump_expr(*arg.index, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::Call>) {
          indent(os);
          os << Color::BLUE << "Call" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "callee:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.callee, os);
          indent_level--;
          for (auto &a : arg.args) {
            indent(os);
            os << Color::GRAY << "arg:" << Color::RESET << "\n";
            indent_level++;
            dump_expr(*a, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::UnaryExpr>) {
          indent(os);
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
          indent_level++;
          dump_expr(*arg.operand, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::BinaryExpr>) {
          indent(os);
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
          indent_level++;
          dump_expr(*arg.left, os);
          dump_expr(*arg.right, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::Assignment>) {
          indent(os);
          os << Color::RED << "Assignment" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "target:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.target, os);
          indent_level--;
          indent(os);
          os << Color::GRAY << "value:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.value, os);
          indent_level--;
          indent_level--;
        } else if constexpr (is_same_v<T, ast::IfExpr>) {
          indent(os);
          os << Color::MAGENTA << "IfExpr" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (size_t i = 0; i < arg.branches.size(); i++) {
            indent(os);
            os << Color::GRAY << (i == 0 ? "if:" : "else if:") << Color::RESET
               << "\n";
            indent_level++;
            indent(os);
            os << Color::GRAY << "cond:" << Color::RESET << "\n";
            indent_level++;
            dump_expr(*arg.branches[i].condition, os);
            indent_level--;
            dump_block(arg.branches[i].body, os);
            indent_level--;
          }
          if (arg.else_branch) {
            indent(os);
            os << Color::GRAY << "else:" << Color::RESET << "\n";
            indent_level++;
            dump_block(*arg.else_branch, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::MatchExpr>) {
          indent(os);
          os << Color::MAGENTA << "MatchExpr" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "subject:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.subject, os);
          indent_level--;
          for (auto &arm : arg.arms) {
            indent(os);
            os << Color::GRAY << "arm:" << Color::RESET << "\n";
            indent_level++;
            dump_pattern(arm.pattern, os);
            dump_block(arm.body, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::Break>) {
          indent(os);
          os << Color::RED << "Break" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          if (arg.value) {
            indent_level++;
            dump_expr(**arg.value, os);
            indent_level--;
          }
        } else if constexpr (is_same_v<T, ast::Continue>) {
          indent(os);
          os << Color::RED << "Continue" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::ComptimeExpr>) {
          indent(os);
          os << Color::CYAN << "ComptimeExpr" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_expr(*arg.expr, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::BuiltinCall>) {
          indent(os);
          os << Color::CYAN << "BuiltinCall" << Color::RESET << " @"
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &a : arg.args)
            dump_expr(*a, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::RangeExpr>) {
          indent(os);
          os << Color::BLUE << "RangeExpr" << Color::RESET
             << (arg.inclusive ? " ..=" : " ..");
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_expr(*arg.start, os);
          dump_expr(*arg.end, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::TypeInit>) {
          indent(os);
          os << Color::MAGENTA << "TypeInit" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_type(*arg.type, os);
          for (auto &[name, val] : arg.fields) {
            indent(os);
            os << Color::GRAY << name << ":" << Color::RESET << "\n";
            indent_level++;
            dump_expr(*val, os);
            indent_level--;
          }
          indent_level--;
        }
      },
      expr.value);
}

static void dump_pattern(const ast::Pattern &pat, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::VariantPattern>) {
          indent(os);
          os << Color::GREEN << "VariantPattern" << Color::RESET << " "
             << arg.variant;
          if (arg.binding)
            os << "(" << *arg.binding << ")";
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::LiteralPattern>) {
          indent(os);
          os << Color::GREEN << "LiteralPattern" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::WildcardPattern>) {
          indent(os);
          os << Color::GREEN << "WildcardPattern" << Color::RESET << " _";
          print_span(os, arg.span);
          os << "\n";
        } else if constexpr (is_same_v<T, ast::IdentPattern>) {
          indent(os);
          os << Color::GREEN << "IdentPattern" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
        }
      },
      pat.value);
}

static void dump_block(const ast::Block &block, ostream &os) {
  indent(os);
  os << Color::GRAY << "Block" << Color::RESET;
  print_span(os, block.span);
  os << "\n";
  indent_level++;
  for (auto &stmt : block.statements)
    dump_stmt(*stmt, os);
  indent_level--;
}

static void dump_stmt(const ast::Stmt &stmt, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::LetStmt>) {
          indent(os);
          os << Color::BOLD_BLUE << "LetStmt" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          if (arg.type) {
            indent(os);
            os << Color::GRAY << "type:" << Color::RESET << "\n";
            indent_level++;
            dump_type(**arg.type, os);
            indent_level--;
          }
          indent(os);
          os << Color::GRAY << "init:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.init, os);
          indent_level--;
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ReturnStmt>) {
          indent(os);
          os << Color::BOLD_RED << "ReturnStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          if (arg.value) {
            indent_level++;
            dump_expr(**arg.value, os);
            indent_level--;
          }
        } else if constexpr (is_same_v<T, ast::DeferStmt>) {
          indent(os);
          os << Color::BOLD_YELLOW << "DeferStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_stmt(*arg.stmt, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::LoopStmt>) {
          indent(os);
          os << Color::MAGENTA << "LoopStmt" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_block(arg.body, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::WhileStmt>) {
          indent(os);
          os << Color::MAGENTA << "WhileStmt" << Color::RESET;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "cond:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.condition, os);
          indent_level--;
          dump_block(arg.body, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ForStmt>) {
          indent(os);
          os << Color::MAGENTA << "ForStmt" << Color::RESET << " " << arg.var;
          if (arg.label)
            os << " :" << *arg.label;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          indent(os);
          os << Color::GRAY << "iterable:" << Color::RESET << "\n";
          indent_level++;
          dump_expr(*arg.iterable, os);
          indent_level--;
          dump_block(arg.body, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ComptimeStmt>) {
          indent(os);
          os << Color::CYAN << "ComptimeStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_stmt(*arg.stmt, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ExprStmt>) {
          indent(os);
          os << Color::GRAY << "ExprStmt" << Color::RESET;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          dump_expr(*arg.expr, os);
          indent_level--;
        }
      },
      stmt.value);
}

static void dump_fn(const ast::FnDecl &fn, ostream &os) {
  indent(os);
  os << Color::BOLD_GREEN << "FnDecl" << Color::RESET << " " << fn.name;
  if (!fn.generic_params.empty()) {
    os << "[";
    for (size_t i = 0; i < fn.generic_params.size(); i++) {
      if (i > 0)
        os << ", ";
      os << fn.generic_params[i].name;
    }
    os << "]";
  }
  print_span(os, fn.span);
  os << "\n";
  indent_level++;
  if (!fn.params.empty()) {
    indent(os);
    os << Color::GRAY << "params:" << Color::RESET << "\n";
    indent_level++;
    for (auto &p : fn.params) {
      indent(os);
      os << p.name;
      if (p.type) {
        os << ": ";
        os << "\n";
        indent_level++;
        dump_type(*p.type, os);
        indent_level--;
      } else {
        os << "\n";
      }
    }
    indent_level--;
  }
  if (fn.return_type) {
    indent(os);
    os << Color::GRAY << "returns:" << Color::RESET << "\n";
    indent_level++;
    dump_type(**fn.return_type, os);
    indent_level--;
  }
  if (fn.where_clause) {
    indent(os);
    os << Color::GRAY << "where:" << Color::RESET << "\n";
    indent_level++;
    for (auto &wc : *fn.where_clause) {
      indent(os);
      os << wc.name << ": interface ";
      for (size_t i = 0; i < wc.iface.names.size(); i++) {
        if (i > 0)
          os << " + ";
        os << wc.iface.names[i];
      }
      os << "\n";
    }
    indent_level--;
  }
  dump_block(fn.body, os);
  indent_level--;
}

static void dump_decl(const ast::Decl &decl, ostream &os) {
  visit(
      [&](auto &&arg) {
        using T = decay_t<decltype(arg)>;
        if constexpr (is_same_v<T, ast::FnDecl>) {
          dump_fn(arg, os);
        } else if constexpr (is_same_v<T, ast::StructDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "StructDecl" << Color::RESET << " "
             << arg.name;
          if (!arg.generic_params.empty()) {
            os << "[";
            for (size_t i = 0; i < arg.generic_params.size(); i++) {
              if (i > 0)
                os << ", ";
              os << arg.generic_params[i].name;
            }
            os << "]";
          }
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &f : arg.fields) {
            indent(os);
            os << Color::GRAY << "field " << f.name << ":" << Color::RESET
               << "\n";
            indent_level++;
            dump_type(*f.type, os);
            indent_level--;
          }
          for (auto &m : arg.methods)
            dump_fn(m, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::UnionDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "UnionDecl" << Color::RESET << " "
             << arg.name;
          if (!arg.generic_params.empty()) {
            os << "[";
            for (size_t i = 0; i < arg.generic_params.size(); i++) {
              if (i > 0)
                os << ", ";
              os << arg.generic_params[i].name;
            }
            os << "]";
          }
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &v : arg.variants) {
            indent(os);
            os << Color::GRAY << "variant " << v.name << Color::RESET;
            if (v.type) {
              os << ":\n";
              indent_level++;
              dump_type(**v.type, os);
              indent_level--;
            } else {
              os << "\n";
            }
          }
          for (auto &m : arg.methods)
            dump_fn(m, os);
          indent_level--;
        } else if constexpr (is_same_v<T, ast::InterfaceDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "InterfaceDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &m : arg.methods) {
            indent(os);
            os << Color::GRAY << "method " << m.name << Color::RESET;
            print_span(os, m.span);
            os << "\n";
            indent_level++;
            for (auto &p : m.params) {
              indent(os);
              os << p.name;
              if (p.type) {
                os << ": \n";
                indent_level++;
                dump_type(*p.type, os);
                indent_level--;
              } else {
                os << "\n";
              }
            }
            if (m.return_type) {
              indent(os);
              os << Color::GRAY << "-> " << Color::RESET << "\n";
              indent_level++;
              dump_type(**m.return_type, os);
              indent_level--;
            }
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::ExternDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "ExternDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &p : arg.params) {
            indent(os);
            os << Color::GRAY << "param:" << Color::RESET << "\n";
            indent_level++;
            dump_type(*p.type, os);
            indent_level--;
          }
          if (arg.return_type) {
            indent(os);
            os << Color::GRAY << "-> " << Color::RESET << "\n";
            indent_level++;
            dump_type(**arg.return_type, os);
            indent_level--;
          }
          indent_level--;
        } else if constexpr (is_same_v<T, ast::UseDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "UseDecl" << Color::RESET << " \""
             << arg.path << "\"";
          print_span(os, arg.span);
          os << "\n";
          if (!arg.items.empty()) {
            indent_level++;
            for (auto &item : arg.items) {
              indent(os);
              os << item.name;
              if (item.alias)
                os << " as " << *item.alias;
              os << "\n";
            }
            indent_level--;
          }
        } else if constexpr (is_same_v<T, ast::MacroDecl>) {
          indent(os);
          os << Color::BOLD_GREEN << "MacroDecl" << Color::RESET << " "
             << arg.name;
          print_span(os, arg.span);
          os << "\n";
          indent_level++;
          for (auto &p : arg.params) {
            indent(os);
            os << p.name << ": ";
            switch (p.kind) {
            case ast::MacroKind::Expr:
              os << "expr";
              break;
            case ast::MacroKind::Stmt:
              os << "stmt";
              break;
            case ast::MacroKind::Type:
              os << "type";
              break;
            case ast::MacroKind::Ident:
              os << "ident";
              break;
            }
            os << "\n";
          }
          dump_block(arg.body, os);
          indent_level--;
        }
      },
      decl.value);
}

void dump_ast(const ast::Program &program, ostream &os) {
  os << Color::BOLD << "Program" << Color::RESET;
  print_span(os, program.span);
  os << "\n";
  indent_level = 0;
  indent_level++;
  for (auto &decl : program.declarations)
    dump_decl(decl, os);
  indent_level--;
}

} // namespace shikimori
