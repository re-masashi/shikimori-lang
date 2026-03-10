#include "monomorphizer.h"
#include <algorithm>
#include <functional>

#include "utils.h"

namespace shikimori {

bool InstantiationKey::operator==(const InstantiationKey &other) const {
  if (base_name != other.base_name)
    return false;
  if (type_args.size() != other.type_args.size())
    return false;
  for (size_t i = 0; i < type_args.size(); i++) {
    if (!types_equal(type_args[i], other.type_args[i]))
      return false;
  }
  return true;
}

bool InstantiationKey::operator<(const InstantiationKey &other) const {
  if (base_name != other.base_name)
    return base_name < other.base_name;
  if (type_args.size() != other.type_args.size())
    return type_args.size() < other.type_args.size();
  for (size_t i = 0; i < type_args.size(); i++) {
    int cmp = compare_types(type_args[i], other.type_args[i]);
    if (cmp != 0)
      return cmp < 0;
  }
  return false;
}

size_t InstantiationKeyHash::operator()(const InstantiationKey &key) const {
  size_t h = std::hash<std::string>{}(key.base_name);
  for (auto &ty : key.type_args) {
    if (auto *named = std::get_if<TyNamed>(&ty->ty)) {
      h ^= std::hash<std::string>{}(named->name);
    }
  }
  return h;
}

bool InstantiationKey::types_equal(const TypeRef &a, const TypeRef &b) {
  if (!a && !b)
    return true;
  if (!a || !b)
    return false;

  return std::visit(
      [&](auto &&ta) -> bool {
        using T = std::decay_t<decltype(ta)>;
        if constexpr (std::is_same_v<T, TyVar>) {
          if (auto *tb = std::get_if<TyVar>(&b->ty))
            return ta.id == tb->id;
          return false;
        } else if constexpr (std::is_same_v<T, ETVar>) {
          if (auto *tb = std::get_if<ETVar>(&b->ty))
            return ta.id == tb->id;
          return false;
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          if (auto *tb = std::get_if<TyNamed>(&b->ty)) {
            if (ta.name != tb->name || ta.kind != tb->kind)
              return false;
            if (ta.args.size() != tb->args.size())
              return false;
            for (size_t i = 0; i < ta.args.size(); i++) {
              if (!types_equal(ta.args[i], tb->args[i]))
                return false;
            }
            return true;
          }
          return false;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          if (auto *tb = std::get_if<FnTy>(&b->ty)) {
            if (ta.args.size() != tb->args.size())
              return false;
            if (!types_equal(ta.return_type, tb->return_type))
              return false;
            for (size_t i = 0; i < ta.args.size(); i++) {
              if (!types_equal(ta.args[i], tb->args[i]))
                return false;
            }
            return true;
          }
          return false;
        } else if constexpr (std::is_same_v<T, ForAll>) {
          return false;
        } else if constexpr (std::is_same_v<T, TyArray>) {
          if (auto *tb = std::get_if<TyArray>(&b->ty))
            return ta.size == tb->size && types_equal(ta.inner, tb->inner);
          return false;
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          if (auto *tb = std::get_if<TyInterfaceObj>(&b->ty)) {
            if (ta.interfaces.size() != tb->interfaces.size())
              return false;
            auto sorted_a = ta.interfaces;
            auto sorted_b = tb->interfaces;
            std::sort(sorted_a.begin(), sorted_a.end());
            std::sort(sorted_b.begin(), sorted_b.end());
            return sorted_a == sorted_b && types_equal(ta.data_ty, tb->data_ty);
          }
          return false;
        }
        return false;
      },
      a->ty);
}

int InstantiationKey::compare_types(const TypeRef &a, const TypeRef &b) {
  if (!a && !b)
    return 0;
  if (!a)
    return -1;
  if (!b)
    return 1;

  size_t idx_a = a->ty.index();
  size_t idx_b = b->ty.index();
  if (idx_a != idx_b)
    return static_cast<int>(idx_a) - static_cast<int>(idx_b);

  return std::visit(
      [&](auto &&ta) -> int {
        using T = std::decay_t<decltype(ta)>;
        if constexpr (std::is_same_v<T, TyVar>) {
          auto *tb = std::get_if<TyVar>(&b->ty);
          return tb ? static_cast<int>(ta.id) - static_cast<int>(tb->id) : 0;
        } else if constexpr (std::is_same_v<T, ETVar>) {
          auto *tb = std::get_if<ETVar>(&b->ty);
          return tb ? static_cast<int>(ta.id) - static_cast<int>(tb->id) : 0;
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          auto *tb = std::get_if<TyNamed>(&b->ty);
          if (!tb)
            return 0;
          if (ta.name != tb->name)
            return ta.name < tb->name ? -1 : 1;
          if (ta.args.size() != tb->args.size())
            return static_cast<int>(ta.args.size()) -
                   static_cast<int>(tb->args.size());
          for (size_t i = 0; i < ta.args.size(); i++) {
            int cmp = compare_types(ta.args[i], tb->args[i]);
            if (cmp != 0)
              return cmp;
          }
          return 0;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          auto *tb = std::get_if<FnTy>(&b->ty);
          if (!tb)
            return 0;
          if (ta.args.size() != tb->args.size())
            return static_cast<int>(ta.args.size()) -
                   static_cast<int>(tb->args.size());
          for (size_t i = 0; i < ta.args.size(); i++) {
            int cmp = compare_types(ta.args[i], tb->args[i]);
            if (cmp != 0)
              return cmp;
          }
          return compare_types(ta.return_type, tb->return_type);
        } else if constexpr (std::is_same_v<T, TyArray>) {
          auto *tb = std::get_if<TyArray>(&b->ty);
          if (!tb)
            return 0;
          if (ta.size != tb->size)
            return ta.size < tb->size ? -1 : 1;
          return compare_types(ta.inner, tb->inner);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          auto *tb = std::get_if<TyInterfaceObj>(&b->ty);
          if (!tb)
            return 0;
          if (ta.interfaces.size() != tb->interfaces.size())
            return static_cast<int>(ta.interfaces.size()) -
                   static_cast<int>(tb->interfaces.size());
          auto sorted_a = ta.interfaces;
          auto sorted_b = tb->interfaces;
          std::sort(sorted_a.begin(), sorted_a.end());
          std::sort(sorted_b.begin(), sorted_b.end());
          for (size_t i = 0; i < sorted_a.size(); i++) {
            if (sorted_a[i] != sorted_b[i])
              return sorted_a[i] < sorted_b[i] ? -1 : 1;
          }
          return compare_types(ta.data_ty, tb->data_ty);
        }
        return 0;
      },
      a->ty);
}

void Monomorphizer::resolve_etvars(const typed::TypedProgram &program) {
  // Copy ETVar solutions from typechecker
  // The typechecker solves ETVars during unification, and we need to use those
  // solutions in the monomorphizer
  for (const auto &[id, ty] : typechecker.ty_solutions) {
    etvar_resolutions[id] = ty;
  }

  // First pass: collect ETVar resolutions from expressions
  for (auto &decl : program.declarations) {
    std::visit(
        [&](auto &&d) {
          using T = std::decay_t<decltype(d)>;
          if constexpr (std::is_same_v<T, typed::TypedFnDecl>) {
            if (!std::get_if<ForAll>(&d.ty->ty)) {
              resolve_etvars_in_fn(d);
            }
          }
        },
        decl.value);
  }
}

void Monomorphizer::resolve_etvars_in_fn(const typed::TypedFnDecl &fn) {
  for (auto &param : fn.params) {
    // Check param types for ETVars
  }
  resolve_etvars_in_block(fn.body);
}

void Monomorphizer::resolve_etvars_in_block(const typed::TypedBlock &block) {
  for (auto &stmt : block.stmts) {
    resolve_etvars_in_stmt(*stmt);
  }
}

void Monomorphizer::resolve_etvars_in_stmt(const typed::TypedStmt &stmt) {
  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          resolve_etvars_in_expr(*s.init);

          // Track variables with ETVar types
          if (auto *let_named = std::get_if<TyNamed>(&s.ty->ty)) {
            if ((let_named->kind == NamedTyKind::Struct ||
                 let_named->kind == NamedTyKind::Union) &&
                !let_named->args.empty()) {
              std::vector<uint32_t> etvar_ids;
              for (auto &arg : let_named->args) {
                if (auto *etvar = std::get_if<ETVar>(&arg->ty)) {
                  etvar_ids.push_back(etvar->id);
                }
              }
              if (!etvar_ids.empty()) {
                var_etvar_map[s.name] = etvar_ids;
              }
            }
          }

          // If the init expression has a concrete type but the let variable
          // has ETVar type, resolve the ETVars
          auto init_ty = s.init->ty;
          if (init_ty && s.ty) {
            auto resolved_init = apply_etvar_resolutions(init_ty);
            if (auto *let_named = std::get_if<TyNamed>(&s.ty->ty)) {
              if (auto *init_named = std::get_if<TyNamed>(&resolved_init->ty)) {
                // Match type args if the type names match
                if (let_named->name == init_named->name &&
                    let_named->args.size() == init_named->args.size()) {
                  for (size_t i = 0; i < let_named->args.size(); i++) {
                    if (auto *etvar =
                            std::get_if<ETVar>(&let_named->args[i]->ty)) {
                      // Resolve from init type
                      etvar_resolutions[etvar->id] = init_named->args[i];
                    }
                  }
                }
              }
            }
          }

          // Resolve null ETVar in init from let type
          auto *init_etvar = std::get_if<ETVar>(&s.init->ty->ty);
          if (init_etvar && init_etvar->name == "null") {
            etvar_resolutions[init_etvar->id] = s.ty;
          }
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value) {
            resolve_etvars_in_expr(**s.value);
            // Resolve null ETVar in return value - the value's type should
            // already be resolved from context during typechecking
          }
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          resolve_etvars_in_stmt(*s.stmt);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          resolve_etvars_in_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          resolve_etvars_in_expr(*s.condition);
          resolve_etvars_in_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          resolve_etvars_in_expr(*s.iterable);
          resolve_etvars_in_block(s.body);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          resolve_etvars_in_expr(*s);
        }
      },
      stmt.value);
}

void Monomorphizer::resolve_etvars_in_expr(const typed::TypedExpr &expr) {
  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          // Key insight: Option[T]::some(value) means T = typeof(value)
          // The expr.ty is Option[ETVar], and payload[0].ty is the concrete
          // type

          if (!e.payload.empty() && expr.ty) {
            auto *union_ty = std::get_if<TyNamed>(&expr.ty->ty);
            if (union_ty && union_ty->kind == NamedTyKind::Union &&
                !union_ty->args.empty()) {
              // Get the payload type
              auto payload_ty = e.payload[0]->ty;

              // Match ETVar positions with concrete types
              for (size_t i = 0; i < union_ty->args.size(); i++) {
                if (auto *etvar = std::get_if<ETVar>(&union_ty->args[i]->ty)) {
                  // Resolve this ETVar to the payload type
                  if (payload_ty) {
                    etvar_resolutions[etvar->id] = payload_ty;
                  }
                }
              }
            }
          }

          for (auto &p : e.payload) {
            resolve_etvars_in_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                             std::is_same_v<T, typed::FloatLiteral> ||
                             std::is_same_v<T, typed::BoolLiteral> ||
                             std::is_same_v<T, typed::StringLiteral> ||
                             std::is_same_v<T, typed::NullLiteral>) {
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          // Track identifier types for ETVar resolution
          if (auto *named = std::get_if<TyNamed>(&expr.ty->ty)) {
            if ((named->kind == NamedTyKind::Struct ||
                 named->kind == NamedTyKind::Union) &&
                !named->args.empty()) {

              // Apply ETVar resolutions to get the resolved type
              auto resolved_ty = apply_etvar_resolutions(expr.ty);

              // If this identifier was already tracked with a different type,
              // propagate the resolution
              auto it = var_resolved_types.find(e.name);
              if (it != var_resolved_types.end()) {
                // We've seen this variable before with a possibly-resolved type
                // Check if the current type is more concrete
                if (auto *resolved_named =
                        std::get_if<TyNamed>(&resolved_ty->ty)) {
                  if (auto *prev_named =
                          std::get_if<TyNamed>(&it->second->ty)) {
                    // Match ETVars from previous to current concrete types
                    if (resolved_named->name == prev_named->name &&
                        resolved_named->args.size() ==
                            prev_named->args.size()) {
                      for (size_t i = 0; i < prev_named->args.size(); i++) {
                        if (auto *etvar =
                                std::get_if<ETVar>(&prev_named->args[i]->ty)) {
                          // This ETVar should resolve to the concrete type
                          auto concrete = resolved_named->args[i];
                          if (concrete && !std::get_if<ETVar>(&concrete->ty) &&
                              !std::get_if<TyVar>(&concrete->ty)) {
                            etvar_resolutions[etvar->id] = concrete;
                          }
                        }
                      }
                    }
                  }
                }
              }

              // Store this type (might be used later for resolution)
              var_resolved_types[e.name] = resolved_ty;
            }
          }
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          for (auto &[name, val] : e.fields) {
            resolve_etvars_in_expr(*val);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          for (auto &p : e.payload) {
            resolve_etvars_in_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          resolve_etvars_in_expr(*e.object);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          resolve_etvars_in_expr(*e.object);

          // Key insight: If object has ETVar type and args provide concrete
          // types, we can resolve the ETVars. E.g., list.push(1) where list:
          // List[ETVar T] and arg is i32 => T = i32
          if (e.object->ty) {
            if (auto *obj_ty = std::get_if<TyNamed>(&e.object->ty->ty)) {
              if ((obj_ty->kind == NamedTyKind::Struct ||
                   obj_ty->kind == NamedTyKind::Union) &&
                  !obj_ty->args.empty()) {
                // Look for ETVars in the object type
                for (size_t i = 0; i < obj_ty->args.size() && i < e.args.size();
                     i++) {
                  if (auto *etvar = std::get_if<ETVar>(&obj_ty->args[i]->ty)) {
                    // This ETVar can be resolved from the method argument type
                    // For push(val: T), the arg type tells us T
                    auto arg_ty = e.args[i]->ty;
                    if (arg_ty && !std::get_if<ETVar>(&arg_ty->ty) &&
                        !std::get_if<TyVar>(&arg_ty->ty)) {
                      etvar_resolutions[etvar->id] = arg_ty;
                    }
                  }
                }
              }
            }
          }

          for (auto &a : e.args) {
            resolve_etvars_in_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          resolve_etvars_in_expr(*e.object);
          resolve_etvars_in_expr(*e.index);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          resolve_etvars_in_expr(*e.callee);

          // For static method calls like List[i32]::new(), the callee might be
          // a ScopeAccess with type args
          if (auto *scope = std::get_if<typed::ScopeAccess>(&e.callee->value)) {
            // Check the return type for ETVars
            if (auto *ret_ty = std::get_if<TyNamed>(&expr.ty->ty)) {
              if ((ret_ty->kind == NamedTyKind::Struct ||
                   ret_ty->kind == NamedTyKind::Union) &&
                  !ret_ty->args.empty()) {
                // Look for explicit type args in the scope access pattern
                // The pattern List[i32]::new() should give us T = i32
              }
            }
          }

          for (auto &a : e.args) {
            resolve_etvars_in_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          resolve_etvars_in_expr(*e.operand);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          resolve_etvars_in_expr(*e.left);
          resolve_etvars_in_expr(*e.right);

          // Key insight: In comparisons like `ptr == null`, resolve null's
          // ETVar from the other operand's concrete type
          auto *left_named = std::get_if<TyNamed>(&e.left->ty->ty);
          auto *right_named = std::get_if<TyNamed>(&e.right->ty->ty);

          if (left_named && right_named) {
            // Check if one side is null (ETVar named "null")
            auto *left_etvar = std::get_if<ETVar>(&e.left->ty->ty);
            auto *right_etvar = std::get_if<ETVar>(&e.right->ty->ty);

            if (left_etvar && left_etvar->name == "null" && !right_etvar) {
              // left is null, resolve from right
              etvar_resolutions[left_etvar->id] = e.right->ty;
            } else if (right_etvar && right_etvar->name == "null" &&
                       !left_etvar) {
              // right is null, resolve from left
              etvar_resolutions[right_etvar->id] = e.left->ty;
            }
          }
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          resolve_etvars_in_expr(*e.target);
          resolve_etvars_in_expr(*e.value);

          // Resolve null ETVar from assignment target type
          auto *val_etvar = std::get_if<ETVar>(&e.value->ty->ty);
          if (val_etvar && val_etvar->name == "null" && e.target->ty) {
            etvar_resolutions[val_etvar->id] = e.target->ty;
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          // Type cast: null as *Type should resolve the null ETVar
          auto *expr_etvar = std::get_if<ETVar>(&e.expr->ty->ty);
          if (expr_etvar && expr_etvar->name == "null" && e.target_ty) {
            etvar_resolutions[expr_etvar->id] = e.target_ty;
          }
          resolve_etvars_in_expr(*e.expr);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            resolve_etvars_in_expr(*branch.condition);
            resolve_etvars_in_block(branch.body);
          }
          if (e.else_branch) {
            resolve_etvars_in_block(*e.else_branch);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          resolve_etvars_in_expr(*e.subject);
          for (auto &arm : e.arms) {
            resolve_etvars_in_block(arm.body);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value)
            resolve_etvars_in_expr(**e.value);
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          // Nothing
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &a : e.args) {
            resolve_etvars_in_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          resolve_etvars_in_expr(*e.start);
          resolve_etvars_in_expr(*e.end);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          for (auto &f : e.fields) {
            resolve_etvars_in_expr(*f);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          resolve_etvars_in_expr(*e.expr);
        }
      },
      expr.value);
}

TypeRef Monomorphizer::apply_etvar_resolutions(const TypeRef &ty) {
  if (!ty)
    return ty;

  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, ETVar>) {
          // if we're already resolving this ETVar, stop
          if (resolving_etvars.count(t.id)) {
            return ty; // Return unresolved to break cycle
          }

          auto it = etvar_resolutions.find(t.id);
          if (it != etvar_resolutions.end()) {
            // Mark this ETVar as being resolved
            resolving_etvars.insert(t.id);
            auto result = apply_etvar_resolutions(it->second);
            resolving_etvars.erase(t.id);
            return result;
          }
          return ty; // Keep unresolved
        } else if constexpr (std::is_same_v<T, TyVar>) {
          return ty; // TyVar should be handled by substitution
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          std::vector<TypeRef> new_args;
          bool changed = false;
          for (auto &arg : t.args) {
            auto resolved = apply_etvar_resolutions(arg);
            if (resolved != arg)
              changed = true;
            new_args.push_back(resolved);
          }
          if (changed) {
            auto result = std::make_shared<Type>();
            result->ty = TyNamed{t.name, t.kind, new_args};
            result->span = ty->span;
            return result;
          }
          return ty;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          bool changed = false;
          std::vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            auto resolved = apply_etvar_resolutions(arg);
            if (resolved != arg)
              changed = true;
            new_args.push_back(resolved);
          }
          auto resolved_ret = apply_etvar_resolutions(t.return_type);
          if (resolved_ret != t.return_type)
            changed = true;
          if (changed) {
            auto result = std::make_shared<Type>();
            result->ty = FnTy{new_args, resolved_ret};
            result->span = ty->span;
            return result;
          }
          return ty;
        } else if constexpr (std::is_same_v<T, TyArray>) {
          auto resolved = apply_etvar_resolutions(t.inner);
          if (resolved != t.inner) {
            auto result = std::make_shared<Type>();
            result->ty = TyArray{resolved, t.size};
            result->span = ty->span;
            return result;
          }
          return ty;
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          auto resolved =
              t.data_ty ? apply_etvar_resolutions(t.data_ty) : nullptr;
          if (resolved != t.data_ty) {
            auto result = std::make_shared<Type>();
            result->ty = TyInterfaceObj{t.interfaces, resolved};
            result->span = ty->span;
            return result;
          }
          return ty;
        }
        return ty;
      },
      ty->ty);
}

void Monomorphizer::apply_resolutions_to_fn(typed::TypedFnDecl &fn) {
  fn.ty = apply_etvar_resolutions(fn.ty);
  fn.return_type = apply_etvar_resolutions(fn.return_type);
  for (auto &param : fn.params) {
    param.ty = apply_etvar_resolutions(param.ty);
  }
  apply_resolutions_to_block(fn.body);
}

void Monomorphizer::apply_resolutions_to_block(typed::TypedBlock &block) {
  block.ty = apply_etvar_resolutions(block.ty);
  for (auto &stmt : block.stmts) {
    apply_resolutions_to_stmt(*stmt);
  }
}

void Monomorphizer::apply_resolutions_to_stmt(typed::TypedStmt &stmt) {
  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          s.ty = apply_etvar_resolutions(s.ty);
          apply_resolutions_to_expr(*s.init);
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value)
            apply_resolutions_to_expr(**s.value);
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          apply_resolutions_to_stmt(*s.stmt);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          apply_resolutions_to_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          apply_resolutions_to_expr(*s.condition);
          apply_resolutions_to_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          s.var_ty = apply_etvar_resolutions(s.var_ty);
          apply_resolutions_to_expr(*s.iterable);
          apply_resolutions_to_block(s.body);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          apply_resolutions_to_expr(*s);
        }
      },
      stmt.value);
}

void Monomorphizer::apply_resolutions_to_expr(typed::TypedExpr &expr) {
  expr.ty = apply_etvar_resolutions(expr.ty);

  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                      std::is_same_v<T, typed::FloatLiteral> ||
                      std::is_same_v<T, typed::BoolLiteral> ||
                      std::is_same_v<T, typed::StringLiteral> ||
                      std::is_same_v<T, typed::NullLiteral>) {
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          // Type already resolved
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          for (auto &[name, val] : e.fields) {
            apply_resolutions_to_expr(*val);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          for (auto &p : e.payload) {
            apply_resolutions_to_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          for (auto &p : e.payload) {
            apply_resolutions_to_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          apply_resolutions_to_expr(*e.object);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          apply_resolutions_to_expr(*e.object);
          for (auto &a : e.args) {
            apply_resolutions_to_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          apply_resolutions_to_expr(*e.object);
          apply_resolutions_to_expr(*e.index);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          apply_resolutions_to_expr(*e.callee);
          for (auto &a : e.args) {
            apply_resolutions_to_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          apply_resolutions_to_expr(*e.operand);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          apply_resolutions_to_expr(*e.left);
          apply_resolutions_to_expr(*e.right);
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          apply_resolutions_to_expr(*e.target);
          apply_resolutions_to_expr(*e.value);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            apply_resolutions_to_expr(*branch.condition);
            apply_resolutions_to_block(branch.body);
          }
          if (e.else_branch) {
            apply_resolutions_to_block(*e.else_branch);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          apply_resolutions_to_expr(*e.subject);
          for (auto &arm : e.arms) {
            arm.pattern.ty = apply_etvar_resolutions(arm.pattern.ty);
            apply_resolutions_to_block(arm.body);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value)
            apply_resolutions_to_expr(**e.value);
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          // No nested expressions
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &ta : e.type_args) {
            ta = apply_etvar_resolutions(ta);
          }
          for (auto &a : e.args) {
            apply_resolutions_to_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          apply_resolutions_to_expr(*e.start);
          apply_resolutions_to_expr(*e.end);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          e.ty = apply_etvar_resolutions(e.ty);
          for (auto &f : e.fields) {
            apply_resolutions_to_expr(*f);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          apply_resolutions_to_expr(*e.expr);
          e.target_ty = apply_etvar_resolutions(e.target_ty);
        }
      },
      expr.value);
}

void Monomorphizer::validate_no_typevars(const typed::TypedProgram &program) {
  for (auto &decl : program.declarations) {
    std::visit(
        [&](auto &&d) {
          using T = std::decay_t<decltype(d)>;
          if constexpr (std::is_same_v<T, typed::TypedFnDecl>) {
            // Skip generic functions. They will be instantiated
            if (!std::get_if<ForAll>(&d.ty->ty)) {
              validate_fn(d);
            }
          } else if constexpr (std::is_same_v<T, typed::TypedStructDecl>) {
            if (!std::get_if<ForAll>(&d.ty->ty)) {
              for (auto &f : d.fields) {
                validate_type(f.ty, f.span);
              }
            }
          } else if constexpr (std::is_same_v<T, typed::TypedUnionDecl>) {
            if (!std::get_if<ForAll>(&d.ty->ty)) {
              for (auto &v : d.variants) {
                if (v.ty) {
                  validate_type(*v.ty, v.span);
                }
              }
            }
          }
        },
        decl.value);
  }
}

void Monomorphizer::validate_fn(const typed::TypedFnDecl &fn) {
  for (auto &param : fn.params) {
    validate_type(param.ty, param.span);
  }

  validate_type(fn.return_type, fn.span);

  validate_block(fn.body);
}

void Monomorphizer::validate_block(const typed::TypedBlock &block) {
  validate_type(block.ty, block.span);
  for (auto &stmt : block.stmts) {
    validate_stmt(*stmt);
  }
}

void Monomorphizer::validate_stmt(const typed::TypedStmt &stmt) {
  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          validate_type(s.ty, s.span);
          validate_expr(*s.init);
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value)
            validate_expr(**s.value);
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          validate_stmt(*s.stmt);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          validate_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          validate_expr(*s.condition);
          validate_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          validate_type(s.var_ty, s.span);
          validate_expr(*s.iterable);
          validate_block(s.body);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          validate_expr(*s);
        }
      },
      stmt.value);
}

void Monomorphizer::validate_expr(const typed::TypedExpr &expr) {
  validate_type(expr.ty, expr.span);

  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                      std::is_same_v<T, typed::FloatLiteral> ||
                      std::is_same_v<T, typed::BoolLiteral> ||
                      std::is_same_v<T, typed::StringLiteral> ||
                      std::is_same_v<T, typed::NullLiteral>) {
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          for (auto &[name, val] : e.fields) {
            validate_expr(*val);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          for (auto &p : e.payload) {
            validate_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          for (auto &p : e.payload) {
            validate_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          validate_expr(*e.object);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          validate_expr(*e.object);
          for (auto &a : e.args) {
            validate_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          validate_expr(*e.object);
          validate_expr(*e.index);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          validate_expr(*e.callee);
          for (auto &a : e.args) {
            validate_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          validate_expr(*e.operand);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          validate_expr(*e.left);
          validate_expr(*e.right);
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          validate_expr(*e.target);
          validate_expr(*e.value);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            validate_expr(*branch.condition);
            validate_block(branch.body);
          }
          if (e.else_branch) {
            validate_block(*e.else_branch);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          validate_expr(*e.subject);
          for (auto &arm : e.arms) {
            validate_pattern(arm.pattern);
            validate_block(arm.body);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value)
            validate_expr(**e.value);
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &ta : e.type_args) {
            validate_type(ta, e.span);
          }
          for (auto &a : e.args) {
            validate_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          validate_expr(*e.start);
          validate_expr(*e.end);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          validate_type(e.ty, e.span);
          for (auto &f : e.fields) {
            validate_expr(*f);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          validate_expr(*e.expr);
          validate_type(e.target_ty, e.span);
        }
      },
      expr.value);
}

void Monomorphizer::validate_type(const TypeRef &ty, Span span) {
  if (!ty)
    return;

  TypeRef resolved = apply_etvar_resolutions(ty);

  std::visit(
      [&](auto &&t) {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, TyVar>) {
          throw MonoError("Type variable #" + std::to_string(t.id) + " (" +
                              t.name +
                              ") was not resolved. All type variables must be "
                              "concrete before monomorphization.",
                          span);
        } else if constexpr (std::is_same_v<T, ETVar>) {
          auto it = etvar_resolutions.find(t.id);
          if (it == etvar_resolutions.end()) {
            fprintf(stderr, "DEBUG: ETVar #%d (%s) not resolved\n", t.id,
                    t.name.c_str());
            throw MonoError("Early type variable #" + std::to_string(t.id) +
                                " (" + t.name +
                                ") was not resolved. All type variables must "
                                "be concrete before monomorphization.",
                            span);
          }
          validate_type(it->second, span);
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          for (auto &arg : t.args) {
            validate_type(arg, span);
          }
        } else if constexpr (std::is_same_v<T, FnTy>) {
          for (auto &arg : t.args) {
            validate_type(arg, span);
          }
          validate_type(t.return_type, span);
        } else if constexpr (std::is_same_v<T, ForAll>) {
          // Skip validation under forall - generic types are validated when
          // instantiated
        } else if constexpr (std::is_same_v<T, TyArray>) {
          validate_type(t.inner, span);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          if (t.data_ty)
            validate_type(t.data_ty, span);
        }
      },
      resolved->ty);
}

void Monomorphizer::validate_pattern(const typed::TypedPattern &pat) {
  validate_type(pat.ty, pat.span);
}

std::string Monomorphizer::mangle_type(const TypeRef &ty) {
  if (!ty)
    return "void";

  return std::visit(
      [&](auto &&t) -> std::string {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, TyVar>) {
          return "T" + std::to_string(t.id);
        } else if constexpr (std::is_same_v<T, ETVar>) {
          return "E" + std::to_string(t.id);
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          std::string result;
          switch (t.kind) {
          case NamedTyKind::Primitive:
            result = t.name;
            break;
          case NamedTyKind::Pointer:
            result = "ptr_" + mangle_type(t.args[0]);
            break;
          case NamedTyKind::Slice:
            result = "Slice_" + mangle_type(t.args[0]);
            break;
          case NamedTyKind::Struct:
          case NamedTyKind::Union:
            if (t.args.empty()) {
              result = t.name;
            } else {
              result = t.name;
              for (auto &arg : t.args) {
                result += "_" + mangle_type(arg);
              }
            }
            break;
          case NamedTyKind::Interface:
            result = "iface_" + t.name;
            break;
          }
          return result;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          std::string result = "fn";
          for (auto &arg : t.args) {
            result += "_" + mangle_type(arg);
          }
          result += "_ret_" + mangle_type(t.return_type);
          return result;
        } else if constexpr (std::is_same_v<T, ForAll>) {
          return mangle_type(t.body);
        } else if constexpr (std::is_same_v<T, TyArray>) {
          return "Arr_" + std::to_string(t.size) + "_" + mangle_type(t.inner);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          std::string result = "dyn";
          for (auto &iface : t.interfaces) {
            result += "_" + iface;
          }
          return result;
        }
        return "unknown";
      },
      ty->ty);
}

std::string Monomorphizer::mangle_name(const std::string &base,
                                       const std::vector<TypeRef> &type_args) {
  if (type_args.empty()) {
    return base;
  }

  std::string result = base;
  for (auto &arg : type_args) {
    result += "_" + mangle_type(arg);
  }
  return result;
}

bool Monomorphizer::is_generic_fn(const std::string &name) {
  if (!typechecker.functions.contains(name))
    return false;
  return std::get_if<ForAll>(&typechecker.functions.at(name).ty->ty) != nullptr;
}

bool Monomorphizer::is_generic_struct(const std::string &name) {
  if (!typechecker.structs.contains(name))
    return false;
  return typechecker.structs.at(name).scheme.has_value();
}

bool Monomorphizer::is_generic_union(const std::string &name) {
  if (!typechecker.unions.contains(name))
    return false;
  return typechecker.unions.at(name).scheme.has_value();
}

bool Monomorphizer::is_concrete_type(const TypeRef &ty) {
  return !has_typevars(ty);
}

bool Monomorphizer::has_typevars(const TypeRef &ty) {
  if (!ty)
    return false;

  return std::visit(
      [&](auto &&t) -> bool {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, TyVar> || std::is_same_v<T, ETVar>) {
          return true;
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          for (auto &arg : t.args) {
            if (has_typevars(arg))
              return true;
          }
          return false;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          for (auto &arg : t.args) {
            if (has_typevars(arg))
              return true;
          }
          return has_typevars(t.return_type);
        } else if constexpr (std::is_same_v<T, ForAll>) {
          return true;
        } else if constexpr (std::is_same_v<T, TyArray>) {
          return has_typevars(t.inner);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          return t.data_ty && has_typevars(t.data_ty);
        }
        return false;
      },
      ty->ty);
}

std::optional<std::string>
Monomorphizer::get_mangled_type_name(const TypeRef &ty) {
  if (!ty)
    return std::nullopt;

  auto *named = std::get_if<TyNamed>(&ty->ty);
  if (!named)
    return std::nullopt;

  if (named->args.empty()) {
    return named->name;
  }

  InstantiationKey key{named->name, named->args};

  if (named->kind == NamedTyKind::Struct) {
    auto it = struct_instances.find(key);
    if (it != struct_instances.end())
      return it->second;
  } else if (named->kind == NamedTyKind::Union) {
    auto it = union_instances.find(key);
    if (it != union_instances.end())
      return it->second;
  }

  return mangle_name(named->name, named->args);
}

void Monomorphizer::register_fn_instantiation(
    const std::string &name, const std::vector<TypeRef> &type_args) {
  InstantiationKey key{name, type_args};

  if (function_instances.find(key) != function_instances.end())
    return;

  std::string mangled = mangle_name(name, type_args);
  function_instances[key] = mangled;
  pending_functions.insert(key);
}

void Monomorphizer::register_struct_instantiation(
    const std::string &name, const std::vector<TypeRef> &type_args) {
  InstantiationKey key{name, type_args};

  if (struct_instances.find(key) != struct_instances.end())
    return;

  std::string mangled = mangle_name(name, type_args);
  struct_instances[key] = mangled;
  pending_structs.insert(key);
}

void Monomorphizer::register_union_instantiation(
    const std::string &name, const std::vector<TypeRef> &type_args) {
  InstantiationKey key{name, type_args};

  if (union_instances.find(key) != union_instances.end())
    return;

  std::string mangled = mangle_name(name, type_args);
  union_instances[key] = mangled;
  pending_unions.insert(key);
}

std::vector<TypeRef>
Monomorphizer::extract_type_args(const ForAll &forall,
                                 const TypeRef &instantiated) {
  std::map<uint32_t, TypeRef> var_to_type;

  std::function<void(TypeRef, TypeRef)> build_map;
  build_map = [&](TypeRef pattern, TypeRef concrete) {
    if (auto *tvar = std::get_if<TyVar>(&pattern->ty)) {
      var_to_type[tvar->id] = concrete;
    } else if (auto *named_p = std::get_if<TyNamed>(&pattern->ty)) {
      if (auto *named_c = std::get_if<TyNamed>(&concrete->ty)) {
        for (size_t j = 0; j < named_p->args.size() && j < named_c->args.size();
             j++) {
          build_map(named_p->args[j], named_c->args[j]);
        }
      }
    } else if (auto *fn_p = std::get_if<FnTy>(&pattern->ty)) {
      if (auto *fn_c = std::get_if<FnTy>(&concrete->ty)) {
        for (size_t j = 0; j < fn_p->args.size() && j < fn_c->args.size();
             j++) {
          build_map(fn_p->args[j], fn_c->args[j]);
        }
        build_map(fn_p->return_type, fn_c->return_type);
      }
    } else if (auto *arr_p = std::get_if<TyArray>(&pattern->ty)) {
      if (auto *arr_c = std::get_if<TyArray>(&concrete->ty)) {
        build_map(arr_p->inner, arr_c->inner);
      }
    }
  };

  build_map(forall.body, instantiated);

  std::vector<TypeRef> result;
  for (auto &[var_name, var_id] : forall.vars) {
    if (var_to_type.count(var_id)) {
      result.push_back(var_to_type[var_id]);
    }
  }

  return result;
}

typed::TypedProgram Monomorphizer::run(const typed::TypedProgram &program) {
  TRACE_SCOPE();
  // Phase 0a: Resolve ETVars from context (iterate until fixed point)
  size_t prev_resolutions = 0;
  size_t max_iterations = 100; // Prevent infinite loops
  size_t iteration = 0;
  do {
    prev_resolutions = etvar_resolutions.size();
    resolve_etvars(program);
    iteration++;
  } while (etvar_resolutions.size() > prev_resolutions &&
           iteration < max_iterations);

  // Phase 0b: Validate no typevars in non-generic code
  // (ETVar resolutions are applied during clone operations via clone_type)
  validate_no_typevars(program);

  // Phase 1: Store original declarations and collect initial instantiations
  // (ETVar resolutions are applied during collection via clone_type)
  collect_from_program(program);

  // Phase 2: Generate monomorphized definitions (iteratively until no pending)
  generate_pending_instantiations();

  // Phase 3: Replace all references with mangled names
  replace_references(result_program);

  return std::move(result_program);
}

void Monomorphizer::collect_from_program(const typed::TypedProgram &program) {
  // Collect methods from typechecker's functions map (they're stored with names
  // like "Type.method")
  for (auto &[name, fn_def] : typechecker.functions) {
    // Check if this is a method (contains a dot)
    auto dot_pos = name.find('.');
    if (dot_pos != std::string::npos) {
      std::string type_name = name.substr(0, dot_pos);
      std::string method_name = name.substr(dot_pos + 1);

      // Store method info for monomorphization
      // We need to track that this method belongs to a type
      if (typechecker.structs.contains(type_name) ||
          typechecker.unions.contains(type_name)) {
        method_names[type_name].push_back(method_name);
        original_methods[name] = &fn_def;
      }
    }
  }

  for (auto &decl : program.declarations) {
    std::visit(
        [&](auto &&d) {
          using T = std::decay_t<decltype(d)>;
          if constexpr (std::is_same_v<T, typed::TypedFnDecl>) {
            // Store pointer to the original
            original_fns[d.name] = &d;

            // Check if generic
            bool is_gen = std::get_if<ForAll>(&d.ty->ty) != nullptr;

            if (!is_gen) {
              // Non-generic: add directly and collect from body
              result_program.declarations.push_back(
                  typed::TypedDecl{d.span, clone_fn(d)});
              collect_from_fn(d);
            }
          } else if constexpr (std::is_same_v<T, typed::TypedStructDecl>) {
            original_structs[d.name] = &d;

            bool is_gen = std::get_if<ForAll>(&d.ty->ty) != nullptr;
            if (!is_gen) {
              result_program.declarations.push_back(
                  typed::TypedDecl{d.span, clone_struct(d)});
            }
          } else if constexpr (std::is_same_v<T, typed::TypedUnionDecl>) {
            original_unions[d.name] = &d;

            bool is_gen = std::get_if<ForAll>(&d.ty->ty) != nullptr;
            if (!is_gen) {
              result_program.declarations.push_back(
                  typed::TypedDecl{d.span, clone_union(d)});
            }
          } else if constexpr (std::is_same_v<T, typed::TypedInterfaceDecl>) {
            result_program.declarations.push_back(typed::TypedDecl{d.span, d});
          } else if constexpr (std::is_same_v<T, typed::TypedExternDecl>) {
            result_program.declarations.push_back(typed::TypedDecl{d.span, d});
          }
        },
        decl.value);
  }
}

void Monomorphizer::collect_from_fn(const typed::TypedFnDecl &fn) {
  collect_from_block(fn.body);
}

void Monomorphizer::collect_from_block(const typed::TypedBlock &block) {
  for (auto &stmt : block.stmts) {
    collect_from_stmt(*stmt);
  }
}

void Monomorphizer::collect_from_stmt(const typed::TypedStmt &stmt) {
  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          collect_from_type(s.ty);
          collect_from_expr(*s.init);
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value)
            collect_from_expr(**s.value);
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          collect_from_stmt(*s.stmt);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          collect_from_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          collect_from_expr(*s.condition);
          collect_from_block(s.body);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          collect_from_type(s.var_ty);
          collect_from_expr(*s.iterable);
          collect_from_block(s.body);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          collect_from_expr(*s);
        }
      },
      stmt.value);
}

void Monomorphizer::collect_from_type(const TypeRef &ty) {
  if (!ty)
    return;

  // First apply ETVar resolutions
  TypeRef resolved = apply_etvar_resolutions(ty);

  std::visit(
      [&](auto &&t) {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, TyNamed>) {
          // Register generic struct/union instantiations with resolved type
          // args
          if (t.kind == NamedTyKind::Struct && is_generic_struct(t.name)) {
            // Resolve ETVars in type args
            std::vector<TypeRef> resolved_args;
            for (auto &arg : t.args) {
              resolved_args.push_back(apply_etvar_resolutions(arg));
            }
            register_struct_instantiation(t.name, resolved_args);
          } else if (t.kind == NamedTyKind::Union &&
                     (is_generic_union(t.name) || t.name == "Option")) {
            // Handle both regular generic unions and built-in Option
            std::vector<TypeRef> resolved_args;
            for (auto &arg : t.args) {
              resolved_args.push_back(apply_etvar_resolutions(arg));
            }
            register_union_instantiation(t.name, resolved_args);
          }
          // Recursively collect from type args (nested generics)
          for (auto &arg : t.args) {
            collect_from_type(arg);
          }
        } else if constexpr (std::is_same_v<T, FnTy>) {
          for (auto &arg : t.args) {
            collect_from_type(arg);
          }
          collect_from_type(t.return_type);
        } else if constexpr (std::is_same_v<T, TyArray>) {
          collect_from_type(t.inner);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          if (t.data_ty)
            collect_from_type(t.data_ty);
        }
      },
      resolved->ty);
}

void Monomorphizer::collect_from_expr(const typed::TypedExpr &expr) {
  collect_from_type(expr.ty);

  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                      std::is_same_v<T, typed::FloatLiteral> ||
                      std::is_same_v<T, typed::BoolLiteral> ||
                      std::is_same_v<T, typed::StringLiteral> ||
                      std::is_same_v<T, typed::NullLiteral>) {
          // No collection needed
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          // Check if this is a reference to a generic function
          if (typechecker.functions.contains(e.name)) {
            auto &fn_def = typechecker.functions.at(e.name);
            if (auto *forall = std::get_if<ForAll>(&fn_def.ty->ty)) {
              // Extract type args from the instantiated type
              auto type_args = extract_type_args(*forall, expr.ty);
              if (!type_args.empty()) {
                register_fn_instantiation(e.name, type_args);
              }
            }
          }
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          for (auto &[name, val] : e.fields) {
            collect_from_expr(*val);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          // Static method call or union variant
          for (auto &p : e.payload) {
            collect_from_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          for (auto &p : e.payload) {
            collect_from_expr(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          collect_from_expr(*e.object);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          collect_from_expr(*e.object);
          for (auto &a : e.args) {
            collect_from_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          collect_from_expr(*e.object);
          collect_from_expr(*e.index);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          collect_from_expr(*e.callee);
          for (auto &a : e.args) {
            collect_from_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          collect_from_expr(*e.operand);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          collect_from_expr(*e.left);
          collect_from_expr(*e.right);
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          collect_from_expr(*e.target);
          collect_from_expr(*e.value);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            collect_from_expr(*branch.condition);
            collect_from_block(branch.body);
          }
          if (e.else_branch) {
            collect_from_block(*e.else_branch);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          collect_from_expr(*e.subject);
          for (auto &arm : e.arms) {
            collect_from_block(arm.body);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value)
            collect_from_expr(**e.value);
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          // Nothing
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &ta : e.type_args) {
            collect_from_type(ta);
          }
          for (auto &a : e.args) {
            collect_from_expr(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          collect_from_expr(*e.start);
          collect_from_expr(*e.end);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          for (auto &f : e.fields) {
            collect_from_expr(*f);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          collect_from_expr(*e.expr);
        }
      },
      expr.value);
}

std::map<uint32_t, TypeRef>
Monomorphizer::build_substitution(const ForAll &forall,
                                  const std::vector<TypeRef> &type_args) {
  std::map<uint32_t, TypeRef> subst;
  for (size_t i = 0; i < forall.vars.size() && i < type_args.size(); i++) {
    subst[forall.vars[i].second] = type_args[i];
  }
  return subst;
}

void Monomorphizer::generate_pending_instantiations() {
  // Keep processing until nothing is pending
  // This handles cases where generating one type creates new dependencies
  while (!pending_structs.empty() || !pending_unions.empty() ||
         !pending_functions.empty()) {
    // Process structs
    while (!pending_structs.empty()) {
      auto key = *pending_structs.begin();
      pending_structs.erase(pending_structs.begin());

      // Generate the struct
      auto struct_decl = monomorphize_struct(key);

      // Generate all methods for this struct
      auto methods = monomorphize_struct_methods(
          key, build_substitution(*typechecker.structs.at(key.base_name).scheme,
                                  key.type_args));

      // Add struct declaration
      result_program.declarations.push_back(
          typed::TypedDecl{struct_decl.span, std::move(struct_decl)});

      // Add method declarations
      for (auto &m : methods) {
        result_program.declarations.push_back(
            typed::TypedDecl{m.span, std::move(m)});
      }
    }

    // Process unions
    while (!pending_unions.empty()) {
      auto key = *pending_unions.begin();
      pending_unions.erase(pending_unions.begin());

      auto union_decl = monomorphize_union(key);

      // Only get methods for user-defined unions (not built-in Option)
      std::vector<typed::TypedFnDecl> methods;
      if (key.base_name != "Option" || typechecker.unions.contains("Option")) {
        methods = monomorphize_union_methods(
            key,
            build_substitution(*typechecker.unions.at(key.base_name).scheme,
                               key.type_args));
      }

      result_program.declarations.push_back(
          typed::TypedDecl{union_decl.span, std::move(union_decl)});

      for (auto &m : methods) {
        result_program.declarations.push_back(
            typed::TypedDecl{m.span, std::move(m)});
      }
    }

    // Process functions (may recurse due to new instantiations in bodies)
    while (!pending_functions.empty()) {
      auto key = *pending_functions.begin();
      pending_functions.erase(pending_functions.begin());

      std::string mangled = function_instances[key];
      if (processed_functions.count(mangled))
        continue;
      processed_functions.insert(mangled);

      auto fn_decl = monomorphize_fn(key);

      // Collect any new instantiations from the monomorphized body
      collect_from_fn(fn_decl);

      result_program.declarations.push_back(
          typed::TypedDecl{fn_decl.span, std::move(fn_decl)});
    }
  }
}

typed::TypedStructDecl
Monomorphizer::monomorphize_struct(const InstantiationKey &key) {
  if (!typechecker.structs.contains(key.base_name)) {
    throw MonoError("Unknown struct: " + key.base_name);
  }

  auto &struct_def = typechecker.structs.at(key.base_name);
  if (!struct_def.scheme) {
    throw MonoError("Struct is not generic: " + key.base_name);
  }

  auto subst = build_substitution(*struct_def.scheme, key.type_args);

  typed::TypedStructDecl result;
  result.span = struct_def.scheme->body->span;
  result.name = struct_instances[key];

  // Substitute and mangle the type
  result.ty = substitute_and_mangle_type(struct_def.scheme->body, subst);

  // Monomorphize fields
  for (auto &[name, ty] : struct_def.fields) {
    // First substitute to get concrete types (before mangling)
    auto substituted = substitute_type(ty, subst);
    // Collect any new instantiations from the substituted type
    collect_from_type(substituted);
    // Then mangle
    auto field_ty = substitute_and_mangle_type(ty, subst);
    result.fields.push_back({ty->span, name, field_ty});
  }

  // Methods are handled separately in monomorphize_struct_methods

  return result;
}

typed::TypedUnionDecl
Monomorphizer::monomorphize_union(const InstantiationKey &key) {
  // Handle built-in Option type
  if (key.base_name == "Option" && !typechecker.unions.contains("Option")) {
    // Create the monomorphized Option type
    typed::TypedUnionDecl result;
    result.span = Span{}; // Built-in has no source location
    result.name = union_instances[key];

    auto inner_ty = key.type_args.empty() ? nullptr : key.type_args[0];

    // Create the type
    auto result_ty = std::make_shared<Type>();
    result_ty->ty = TyNamed{result.name, NamedTyKind::Union, {}};
    result.ty = result_ty;

    // Variant: some(T)
    typed::TypedUnionVariant some_variant;
    some_variant.span = Span{};
    some_variant.name = "some";
    if (inner_ty) {
      some_variant.ty = substitute_and_mangle_type(inner_ty, {});
    }
    result.variants.push_back(std::move(some_variant));

    // Variant: none
    typed::TypedUnionVariant none_variant;
    none_variant.span = Span{};
    none_variant.name = "none";
    // none has no payload (unit type)
    result.variants.push_back(std::move(none_variant));

    return result;
  }

  if (!typechecker.unions.contains(key.base_name)) {
    throw MonoError("Unknown union: " + key.base_name);
  }

  auto &union_def = typechecker.unions.at(key.base_name);
  if (!union_def.scheme) {
    throw MonoError("Union is not generic: " + key.base_name);
  }

  auto subst = build_substitution(*union_def.scheme, key.type_args);

  typed::TypedUnionDecl result;
  result.span = union_def.scheme->body->span;
  result.name = union_instances[key];
  result.ty = substitute_and_mangle_type(union_def.scheme->body, subst);

  // Monomorphize variants
  for (auto &[name, payload] : union_def.variants) {
    typed::TypedUnionVariant variant;
    variant.span = union_def.scheme->body->span;
    variant.name = name;
    if (!payload.empty()) {
      // First substitute to get concrete types (before mangling)
      auto substituted = substitute_type(payload[0], subst);
      // Collect any new instantiations from the substituted type
      collect_from_type(substituted);
      // Then mangle
      variant.ty = substitute_and_mangle_type(payload[0], subst);
    }
    result.variants.push_back(std::move(variant));
  }

  return result;
}

std::vector<typed::TypedFnDecl> Monomorphizer::monomorphize_struct_methods(
    const InstantiationKey &struct_key,
    const std::map<uint32_t, TypeRef> &subst) {

  std::vector<typed::TypedFnDecl> result;

  auto it = struct_methods.find(struct_key.base_name);
  if (it == struct_methods.end())
    return result;

  std::string mangled_type = struct_instances[struct_key];

  for (auto &method : it->second) {
    typed::TypedFnDecl mono_method;
    mono_method.span = method.span;

    // Mangle method name: TypeName_method
    mono_method.name = mangled_type + "_" + method.name;

    // Substitute in type
    mono_method.ty = substitute_and_mangle_type(method.ty, subst);

    // Substitute in parameters
    for (auto &param : method.params) {
      mono_method.params.push_back(
          {param.span, param.name,
           substitute_and_mangle_type(param.ty, subst)});
    }

    // Substitute return type
    mono_method.return_type =
        substitute_and_mangle_type(method.return_type, subst);

    // Substitute body
    mono_method.body = substitute_block(method.body, subst);

    result.push_back(std::move(mono_method));
  }

  return result;
}

std::vector<typed::TypedFnDecl> Monomorphizer::monomorphize_union_methods(
    const InstantiationKey &union_key,
    const std::map<uint32_t, TypeRef> &subst) {

  std::vector<typed::TypedFnDecl> result;

  auto it = union_methods.find(union_key.base_name);
  if (it == union_methods.end())
    return result;

  std::string mangled_type = union_instances[union_key];

  for (auto &method : it->second) {
    typed::TypedFnDecl mono_method;
    mono_method.span = method.span;
    mono_method.name = mangled_type + "_" + method.name;
    mono_method.ty = substitute_and_mangle_type(method.ty, subst);

    for (auto &param : method.params) {
      mono_method.params.push_back(
          {param.span, param.name,
           substitute_and_mangle_type(param.ty, subst)});
    }

    mono_method.return_type =
        substitute_and_mangle_type(method.return_type, subst);
    mono_method.body = substitute_block(method.body, subst);

    result.push_back(std::move(mono_method));
  }

  return result;
}

typed::TypedFnDecl Monomorphizer::monomorphize_fn(const InstantiationKey &key) {
  std::string base_name = key.base_name;

  if (original_fns.find(base_name) == original_fns.end()) {
    throw MonoError("Cannot find original function: " + base_name);
  }

  const typed::TypedFnDecl *orig = original_fns.at(base_name);
  auto *forall = std::get_if<ForAll>(&orig->ty->ty);
  if (!forall) {
    throw MonoError("Function is not generic: " + base_name);
  }

  auto subst = build_substitution(*forall, key.type_args);

  typed::TypedFnDecl result;
  result.span = orig->span;
  result.name = function_instances[key];

  // Get the function type from forall body
  auto *orig_fn_ty = std::get_if<FnTy>(&forall->body->ty);
  if (!orig_fn_ty) {
    throw MonoError("Expected FnTy in forall body");
  }

  // Substitute and mangle function type
  result.ty = substitute_and_mangle_type(forall->body, subst);

  // Substitute in parameters
  for (auto &param : orig->params) {
    result.params.push_back(
        {param.span, param.name, substitute_and_mangle_type(param.ty, subst)});
  }

  // Substitute return type
  result.return_type = substitute_and_mangle_type(orig->return_type, subst);

  // Substitute body
  result.body = substitute_block(orig->body, subst);

  return result;
}

TypeRef
Monomorphizer::substitute_type(const TypeRef &ty,
                               const std::map<uint32_t, TypeRef> &subst) {
  if (!ty)
    return nullptr;

  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, TyVar>) {
          auto it = subst.find(t.id);
          if (it != subst.end())
            return clone_type(it->second);
          return clone_type(ty);
        } else if constexpr (std::is_same_v<T, ETVar>) {
          // First check the ForAll substitution map
          auto it = subst.find(t.id);
          if (it != subst.end())
            return clone_type(it->second);
          // Then check the global ETVar resolutions
          auto etvar_it = etvar_resolutions.find(t.id);
          if (etvar_it != etvar_resolutions.end())
            return clone_type(etvar_it->second);
          return clone_type(ty);
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          std::vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(substitute_type(arg, subst));
          }
          auto result = std::make_shared<Type>();
          result->ty = TyNamed{t.name, t.kind, new_args};
          result->span = ty->span;
          return result;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          std::vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(substitute_type(arg, subst));
          }
          auto result = std::make_shared<Type>();
          result->ty = FnTy{new_args, substitute_type(t.return_type, subst)};
          result->span = ty->span;
          return result;
        } else if constexpr (std::is_same_v<T, ForAll>) {
          // Don't substitute inside ForAll. It defines its own type vars
          return clone_type(ty);
        } else if constexpr (std::is_same_v<T, TyArray>) {
          auto result = std::make_shared<Type>();
          result->ty = TyArray{substitute_type(t.inner, subst), t.size};
          result->span = ty->span;
          return result;
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          auto result = std::make_shared<Type>();
          result->ty = TyInterfaceObj{
              t.interfaces,
              t.data_ty ? substitute_type(t.data_ty, subst) : nullptr};
          result->span = ty->span;
          return result;
        }
        return clone_type(ty);
      },
      ty->ty);
}

TypeRef Monomorphizer::substitute_and_mangle_type(
    const TypeRef &ty, const std::map<uint32_t, TypeRef> &subst) {
  // First substitute type variables
  TypeRef substituted = substitute_type(ty, subst);

  if (!substituted)
    return nullptr;

  // Then mangle generic type names
  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, TyNamed>) {
          // For generic structs/unions, replace with mangled name
          if ((t.kind == NamedTyKind::Struct || t.kind == NamedTyKind::Union) &&
              !t.args.empty()) {
            // Recursively mangle args
            std::vector<TypeRef> mangled_args;
            for (auto &arg : t.args) {
              mangled_args.push_back(substitute_and_mangle_type(arg, subst));
            }

            // Check if this is a known instantiation
            InstantiationKey key{t.name, mangled_args};
            std::string mangled_name;

            if (t.kind == NamedTyKind::Struct) {
              auto it = struct_instances.find(key);
              mangled_name = (it != struct_instances.end())
                                 ? it->second
                                 : mangle_name(t.name, mangled_args);
            } else {
              auto it = union_instances.find(key);
              mangled_name = (it != union_instances.end())
                                 ? it->second
                                 : mangle_name(t.name, mangled_args);
            }

            // Return type with mangled name and EMPTY args (they're embedded in
            // the name now)
            auto result = std::make_shared<Type>();
            result->ty = TyNamed{mangled_name, t.kind, {}}; // Empty args!
            result->span = substituted->span;
            return result;
          }

          // For pointers, slices, etc. mangle the inner type args
          if (!t.args.empty()) {
            bool changed = false;
            std::vector<TypeRef> mangled_args;
            for (auto &arg : t.args) {
              auto mangled = substitute_and_mangle_type(arg, subst);
              if (mangled != arg)
                changed = true;
              mangled_args.push_back(mangled);
            }
            if (changed) {
              auto result = std::make_shared<Type>();
              result->ty = TyNamed{t.name, t.kind, mangled_args};
              result->span = substituted->span;
              return result;
            }
          }
          return substituted;
        } else if constexpr (std::is_same_v<T, FnTy>) {
          std::vector<TypeRef> mangled_args;
          for (auto &arg : t.args) {
            mangled_args.push_back(substitute_and_mangle_type(arg, subst));
          }
          auto result = std::make_shared<Type>();
          result->ty = FnTy{mangled_args,
                            substitute_and_mangle_type(t.return_type, subst)};
          result->span = substituted->span;
          return result;
        } else if constexpr (std::is_same_v<T, TyArray>) {
          auto result = std::make_shared<Type>();
          result->ty =
              TyArray{substitute_and_mangle_type(t.inner, subst), t.size};
          result->span = substituted->span;
          return result;
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          auto result = std::make_shared<Type>();
          result->ty = TyInterfaceObj{
              t.interfaces, t.data_ty
                                ? substitute_and_mangle_type(t.data_ty, subst)
                                : nullptr};
          result->span = substituted->span;
          return result;
        }
        return substituted;
      },
      substituted->ty);
}

TypeRef Monomorphizer::clone_type(const TypeRef &ty) {
  if (!ty)
    return nullptr;

  return std::visit(
      [&](auto &&t) -> TypeRef {
        using T = std::decay_t<decltype(t)>;
        auto result = std::make_shared<Type>();
        result->span = ty->span;

        if constexpr (std::is_same_v<T, TyVar>) {
          result->ty = TyVar{t.id, t.name};
        } else if constexpr (std::is_same_v<T, ETVar>) {
          // Apply ETVar resolution if available (with cycle detection in
          // apply_etvar_resolutions)
          auto it = etvar_resolutions.find(t.id);
          if (it != etvar_resolutions.end()) {
            // Use apply_etvar_resolutions which has cycle detection
            return apply_etvar_resolutions(it->second);
          }
          result->ty = ETVar{t.id, t.name};
        } else if constexpr (std::is_same_v<T, TyNamed>) {
          std::vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(clone_type(arg));
          }
          result->ty = TyNamed{t.name, t.kind, new_args};
        } else if constexpr (std::is_same_v<T, FnTy>) {
          std::vector<TypeRef> new_args;
          for (auto &arg : t.args) {
            new_args.push_back(clone_type(arg));
          }
          result->ty = FnTy{new_args, clone_type(t.return_type)};
        } else if constexpr (std::is_same_v<T, ForAll>) {
          result->ty = ForAll{t.vars, clone_type(t.body)};
        } else if constexpr (std::is_same_v<T, TyArray>) {
          result->ty = TyArray{clone_type(t.inner), t.size};
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          result->ty = TyInterfaceObj{t.interfaces, clone_type(t.data_ty)};
        }
        return result;
      },
      ty->ty);
}

typed::TypedExpr Monomorphizer::clone_expr(const typed::TypedExpr &expr) {
  typed::TypedExpr result;
  result.span = expr.span;
  result.ty = clone_type(expr.ty);

  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral>) {
          result.value = typed::IntLiteral{e.span, e.value};
        } else if constexpr (std::is_same_v<T, typed::FloatLiteral>) {
          result.value = typed::FloatLiteral{e.span, e.value};
        } else if constexpr (std::is_same_v<T, typed::BoolLiteral>) {
          result.value = typed::BoolLiteral{e.span, e.value};
        } else if constexpr (std::is_same_v<T, typed::StringLiteral>) {
          result.value = typed::StringLiteral{e.span, e.value};
        } else if constexpr (std::is_same_v<T, typed::NullLiteral>) {
          result.value = typed::NullLiteral{e.span};
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          result.value =
              typed::IdentifierExpr{e.span, e.name, clone_type(e.ty)};
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          std::vector<std::pair<std::string, std::unique_ptr<typed::TypedExpr>>>
              fields;
          for (auto &[name, val] : e.fields) {
            fields.push_back(
                {name, std::make_unique<typed::TypedExpr>(clone_expr(*val))});
          }
          result.value = typed::StructInit{e.span, e.name, std::move(fields)};
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          std::vector<std::unique_ptr<typed::TypedExpr>> payload;
          for (auto &p : e.payload) {
            payload.push_back(
                std::make_unique<typed::TypedExpr>(clone_expr(*p)));
          }
          result.value =
              typed::ScopeAccess{e.span, e.scope, e.member, std::move(payload)};
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          std::vector<std::unique_ptr<typed::TypedExpr>> payload;
          for (auto &p : e.payload) {
            payload.push_back(
                std::make_unique<typed::TypedExpr>(clone_expr(*p)));
          }
          result.value = typed::UnionVariantInit{e.span, e.union_name,
                                                 e.variant, std::move(payload)};
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          result.value = typed::FieldAccess{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.object)),
              e.field};
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          std::vector<std::unique_ptr<typed::TypedExpr>> args;
          for (auto &a : e.args) {
            args.push_back(std::make_unique<typed::TypedExpr>(clone_expr(*a)));
          }
          result.value = typed::MethodCall{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.object)),
              e.method, std::move(args)};
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          result.value = typed::IndexAccess{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.object)),
              std::make_unique<typed::TypedExpr>(clone_expr(*e.index))};
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          std::vector<std::unique_ptr<typed::TypedExpr>> args;
          for (auto &a : e.args) {
            args.push_back(std::make_unique<typed::TypedExpr>(clone_expr(*a)));
          }
          result.value = typed::Call{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.callee)),
              std::move(args)};
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          result.value = typed::UnaryExpr{
              e.span, e.op,
              std::make_unique<typed::TypedExpr>(clone_expr(*e.operand))};
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          result.value = typed::BinaryExpr{
              e.span, e.op,
              std::make_unique<typed::TypedExpr>(clone_expr(*e.left)),
              std::make_unique<typed::TypedExpr>(clone_expr(*e.right))};
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          result.value = typed::Assignment{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.target)),
              std::make_unique<typed::TypedExpr>(clone_expr(*e.value))};
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          std::vector<typed::IfBranch> branches;
          for (auto &b : e.branches) {
            branches.push_back(typed::IfBranch{
                b.span,
                std::make_unique<typed::TypedExpr>(clone_expr(*b.condition)),
                clone_block(b.body)});
          }
          std::optional<typed::TypedBlock> else_branch;
          if (e.else_branch) {
            else_branch = clone_block(*e.else_branch);
          }
          result.value = typed::IfExpr{e.span, std::move(branches),
                                       std::move(else_branch)};
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          std::vector<typed::MatchArm> arms;
          for (auto &arm : e.arms) {
            arms.push_back(typed::MatchArm{arm.span, clone_pattern(arm.pattern),
                                           clone_block(arm.body)});
          }
          result.value = typed::MatchExpr{
              e.span,
              std::make_unique<typed::TypedExpr>(clone_expr(*e.subject)),
              std::move(arms)};
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          std::optional<std::unique_ptr<typed::TypedExpr>> val;
          if (e.value) {
            val = std::make_unique<typed::TypedExpr>(clone_expr(**e.value));
          }
          result.value = typed::Break{e.span, e.label, std::move(val)};
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          result.value = typed::Continue{e.span, e.label};
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          std::vector<TypeRef> type_args;
          for (auto &ta : e.type_args) {
            type_args.push_back(clone_type(ta));
          }
          std::vector<std::unique_ptr<typed::TypedExpr>> args;
          for (auto &a : e.args) {
            args.push_back(std::make_unique<typed::TypedExpr>(clone_expr(*a)));
          }
          result.value = typed::BuiltinCall{
              e.span, e.name, std::move(type_args), std::move(args)};
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          result.value = typed::RangeExpr{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.start)),
              std::make_unique<typed::TypedExpr>(clone_expr(*e.end))};
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          std::vector<std::unique_ptr<typed::TypedExpr>> fields;
          for (auto &f : e.fields) {
            fields.push_back(
                std::make_unique<typed::TypedExpr>(clone_expr(*f)));
          }
          result.value =
              typed::TypeInit{e.span, clone_type(e.ty), std::move(fields)};
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          result.value = typed::AsExpr{
              e.span, std::make_unique<typed::TypedExpr>(clone_expr(*e.expr)),
              clone_type(e.target_ty)};
        }
      },
      expr.value);

  return result;
}

typed::TypedStmt Monomorphizer::clone_stmt(const typed::TypedStmt &stmt) {
  typed::TypedStmt result;
  result.span = stmt.span;

  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          result.value = typed::LetStmt{
              s.span, s.name, clone_type(s.ty),
              std::make_unique<typed::TypedExpr>(clone_expr(*s.init))};
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          std::optional<std::unique_ptr<typed::TypedExpr>> val;
          if (s.value) {
            val = std::make_unique<typed::TypedExpr>(clone_expr(**s.value));
          }
          result.value = typed::ReturnStmt{s.span, std::move(val)};
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          result.value = typed::DeferStmt{
              s.span, std::make_unique<typed::TypedStmt>(clone_stmt(*s.stmt))};
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          result.value = typed::LoopStmt{s.span, s.label, clone_block(s.body)};
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          result.value = typed::WhileStmt{
              s.span, s.label,
              std::make_unique<typed::TypedExpr>(clone_expr(*s.condition)),
              clone_block(s.body)};
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          result.value = typed::ForStmt{
              s.span,
              s.label,
              s.var,
              clone_type(s.var_ty),
              std::make_unique<typed::TypedExpr>(clone_expr(*s.iterable)),
              clone_block(s.body)};
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          result.value = std::make_unique<typed::TypedExpr>(clone_expr(*s));
        }
      },
      stmt.value);

  return result;
}

typed::TypedBlock Monomorphizer::clone_block(const typed::TypedBlock &block) {
  typed::TypedBlock result;
  result.span = block.span;
  result.ty = clone_type(block.ty);

  for (auto &stmt : block.stmts) {
    result.stmts.push_back(
        std::make_unique<typed::TypedStmt>(clone_stmt(*stmt)));
  }

  return result;
}

typed::TypedPattern
Monomorphizer::clone_pattern(const typed::TypedPattern &pat) {
  typed::TypedPattern result;
  result.span = pat.span;
  result.ty = clone_type(pat.ty);
  result.value = pat.value;
  return result;
}

typed::TypedFnDecl Monomorphizer::clone_fn(const typed::TypedFnDecl &fn) {
  typed::TypedFnDecl result;
  result.span = fn.span;
  result.name = fn.name;
  result.ty = clone_type(fn.ty);

  for (auto &param : fn.params) {
    result.params.push_back({param.span, param.name, clone_type(param.ty)});
  }

  result.return_type = clone_type(fn.return_type);
  result.body = clone_block(fn.body);

  return result;
}

typed::TypedStructDecl
Monomorphizer::clone_struct(const typed::TypedStructDecl &s) {
  typed::TypedStructDecl result;
  result.span = s.span;
  result.name = s.name;
  result.ty = clone_type(s.ty);
  for (auto &f : s.fields) {
    result.fields.push_back({f.span, f.name, clone_type(f.ty)});
  }
  for (auto &m : s.methods) {
    result.methods.push_back(clone_fn(m));
  }
  return result;
}

typed::TypedUnionDecl
Monomorphizer::clone_union(const typed::TypedUnionDecl &u) {
  typed::TypedUnionDecl result;
  result.span = u.span;
  result.name = u.name;
  result.ty = clone_type(u.ty);
  for (auto &v : u.variants) {
    typed::TypedUnionVariant variant;
    variant.span = v.span;
    variant.name = v.name;
    if (v.ty) {
      variant.ty = clone_type(*v.ty);
    }
    result.variants.push_back(std::move(variant));
  }
  for (auto &m : u.methods) {
    result.methods.push_back(clone_fn(m));
  }
  return result;
}

typed::TypedExpr
Monomorphizer::substitute_expr(const typed::TypedExpr &expr,
                               const std::map<uint32_t, TypeRef> &subst) {
  typed::TypedExpr result = clone_expr(expr);

  // Substitute and mangle the expression type
  result.ty = substitute_and_mangle_type(result.ty, subst);

  // Recursively process expressions and update types
  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                      std::is_same_v<T, typed::FloatLiteral> ||
                      std::is_same_v<T, typed::BoolLiteral> ||
                      std::is_same_v<T, typed::StringLiteral> ||
                      std::is_same_v<T, typed::NullLiteral>) {
          // Literals. No nested expressions
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          e.ty = result.ty; // Use the substituted type
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          if (auto *named = std::get_if<TyNamed>(&result.ty->ty)) {
            e.name = named->name;
          }
          for (auto &[name, val] : e.fields) {
            *val = substitute_expr(*val, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          for (auto &p : e.payload) {
            *p = substitute_expr(*p, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          if (auto *named = std::get_if<TyNamed>(&result.ty->ty)) {
            e.union_name = named->name;
          }
          for (auto &p : e.payload) {
            *p = substitute_expr(*p, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          *e.object = substitute_expr(*e.object, subst);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          *e.object = substitute_expr(*e.object, subst);
          for (auto &a : e.args) {
            *a = substitute_expr(*a, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          *e.object = substitute_expr(*e.object, subst);
          *e.index = substitute_expr(*e.index, subst);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          *e.callee = substitute_expr(*e.callee, subst);
          for (auto &a : e.args) {
            *a = substitute_expr(*a, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          *e.operand = substitute_expr(*e.operand, subst);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          *e.left = substitute_expr(*e.left, subst);
          *e.right = substitute_expr(*e.right, subst);
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          *e.target = substitute_expr(*e.target, subst);
          *e.value = substitute_expr(*e.value, subst);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            *branch.condition = substitute_expr(*branch.condition, subst);
            branch.body = substitute_block(branch.body, subst);
          }
          if (e.else_branch) {
            e.else_branch = substitute_block(*e.else_branch, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          *e.subject = substitute_expr(*e.subject, subst);
          for (auto &arm : e.arms) {
            arm.body = substitute_block(arm.body, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value) {
            **e.value = substitute_expr(**e.value, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          // No nested expressions
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &ta : e.type_args) {
            ta = substitute_and_mangle_type(ta, subst);
          }
          for (auto &a : e.args) {
            *a = substitute_expr(*a, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          *e.start = substitute_expr(*e.start, subst);
          *e.end = substitute_expr(*e.end, subst);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          for (auto &f : e.fields) {
            *f = substitute_expr(*f, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          *e.expr = substitute_expr(*e.expr, subst);
          e.target_ty = substitute_and_mangle_type(e.target_ty, subst);
        }
      },
      result.value);

  return result;
}

typed::TypedStmt
Monomorphizer::substitute_stmt(const typed::TypedStmt &stmt,
                               const std::map<uint32_t, TypeRef> &subst) {
  typed::TypedStmt result = clone_stmt(stmt);

  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          s.ty = substitute_and_mangle_type(s.ty, subst);
          *s.init = substitute_expr(*s.init, subst);
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value) {
            **s.value = substitute_expr(**s.value, subst);
          }
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          *s.stmt = substitute_stmt(*s.stmt, subst);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          s.body = substitute_block(s.body, subst);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          *s.condition = substitute_expr(*s.condition, subst);
          s.body = substitute_block(s.body, subst);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          s.var_ty = substitute_and_mangle_type(s.var_ty, subst);
          *s.iterable = substitute_expr(*s.iterable, subst);
          s.body = substitute_block(s.body, subst);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          *s = substitute_expr(*s, subst);
        }
      },
      result.value);

  return result;
}

typed::TypedBlock
Monomorphizer::substitute_block(const typed::TypedBlock &block,
                                const std::map<uint32_t, TypeRef> &subst) {
  typed::TypedBlock result = clone_block(block);
  result.ty = substitute_and_mangle_type(result.ty, subst);

  for (auto &stmt : result.stmts) {
    *stmt = substitute_stmt(*stmt, subst);
  }

  return result;
}

typed::TypedPattern
Monomorphizer::substitute_pattern(const typed::TypedPattern &pat,
                                  const std::map<uint32_t, TypeRef> &subst) {
  typed::TypedPattern result = clone_pattern(pat);
  result.ty = substitute_and_mangle_type(result.ty, subst);
  return result;
}

void Monomorphizer::replace_references(typed::TypedProgram &program) {
  for (auto &decl : program.declarations) {
    std::visit(
        [&](auto &&d) {
          using T = std::decay_t<decltype(d)>;
          if constexpr (std::is_same_v<T, typed::TypedFnDecl>) {
            replace_fn_refs(d);
          }
        },
        decl.value);
  }
}

void Monomorphizer::replace_fn_refs(typed::TypedFnDecl &fn) {
  replace_type_refs(fn.ty);
  replace_type_refs(fn.return_type);
  for (auto &param : fn.params) {
    replace_type_refs(param.ty);
  }
  replace_block_refs(fn.body);
}

void Monomorphizer::replace_block_refs(typed::TypedBlock &block) {
  replace_type_refs(block.ty);
  for (auto &stmt : block.stmts) {
    replace_stmt_refs(*stmt);
  }
}

void Monomorphizer::replace_stmt_refs(typed::TypedStmt &stmt) {
  std::visit(
      [&](auto &&s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, typed::LetStmt>) {
          replace_type_refs(s.ty);
          replace_expr_refs(*s.init);
        } else if constexpr (std::is_same_v<T, typed::ReturnStmt>) {
          if (s.value)
            replace_expr_refs(**s.value);
        } else if constexpr (std::is_same_v<T, typed::DeferStmt>) {
          replace_stmt_refs(*s.stmt);
        } else if constexpr (std::is_same_v<T, typed::LoopStmt>) {
          replace_block_refs(s.body);
        } else if constexpr (std::is_same_v<T, typed::WhileStmt>) {
          replace_expr_refs(*s.condition);
          replace_block_refs(s.body);
        } else if constexpr (std::is_same_v<T, typed::ForStmt>) {
          replace_type_refs(s.var_ty);
          replace_expr_refs(*s.iterable);
          replace_block_refs(s.body);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<typed::TypedExpr>>) {
          replace_expr_refs(*s);
        }
      },
      stmt.value);
}

void Monomorphizer::replace_type_refs(TypeRef &ty) {
  if (!ty)
    return;

  std::visit(
      [&](auto &&t) {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, TyNamed>) {
          // Replace generic struct/union names with mangled versions
          if ((t.kind == NamedTyKind::Struct || t.kind == NamedTyKind::Union) &&
              !t.args.empty()) {
            auto mangled = get_mangled_type_name(ty);
            if (mangled) {
              t.name = *mangled;
              t.args.clear(); // Clear args since they're now embedded in name
            }
          }
          for (auto &arg : t.args) {
            replace_type_refs(arg);
          }
        } else if constexpr (std::is_same_v<T, FnTy>) {
          for (auto &arg : t.args) {
            replace_type_refs(arg);
          }
          replace_type_refs(t.return_type);
        } else if constexpr (std::is_same_v<T, TyArray>) {
          replace_type_refs(t.inner);
        } else if constexpr (std::is_same_v<T, TyInterfaceObj>) {
          if (t.data_ty)
            replace_type_refs(t.data_ty);
        }
      },
      ty->ty);
}

void Monomorphizer::replace_expr_refs(typed::TypedExpr &expr) {
  replace_type_refs(expr.ty);

  std::visit(
      [&](auto &&e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, typed::IntLiteral> ||
                      std::is_same_v<T, typed::FloatLiteral> ||
                      std::is_same_v<T, typed::BoolLiteral> ||
                      std::is_same_v<T, typed::StringLiteral> ||
                      std::is_same_v<T, typed::NullLiteral>) {
          // No refs to replace
        } else if constexpr (std::is_same_v<T, typed::IdentifierExpr>) {
          // Check if this is a reference to a generic function
          if (typechecker.functions.contains(e.name)) {
            auto &fn_def = typechecker.functions.at(e.name);
            if (auto *forall = std::get_if<ForAll>(&fn_def.ty->ty)) {
              auto type_args = extract_type_args(*forall, expr.ty);
              if (!type_args.empty()) {
                InstantiationKey key{e.name, type_args};
                auto it = function_instances.find(key);
                if (it != function_instances.end()) {
                  e.name = it->second;
                }
              }
            }
          }
        } else if constexpr (std::is_same_v<T, typed::StructInit>) {
          // Replace struct name
          if (auto *named = std::get_if<TyNamed>(&expr.ty->ty)) {
            if (!named->args.empty()) {
              auto mangled = get_mangled_type_name(expr.ty);
              if (mangled) {
                e.name = *mangled;
              }
            }
          }
          for (auto &[name, val] : e.fields) {
            replace_expr_refs(*val);
          }
        } else if constexpr (std::is_same_v<T, typed::ScopeAccess>) {
          // Replace union/struct name
          if (typechecker.unions.contains(e.scope) ||
              typechecker.structs.contains(e.scope)) {
            if (auto *named = std::get_if<TyNamed>(&expr.ty->ty)) {
              if (!named->args.empty()) {
                auto mangled = get_mangled_type_name(expr.ty);
                if (mangled) {
                  e.scope = *mangled;
                }
              }
            }
          }
          for (auto &p : e.payload) {
            replace_expr_refs(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::UnionVariantInit>) {
          if (auto *named = std::get_if<TyNamed>(&expr.ty->ty)) {
            if (!named->args.empty()) {
              auto mangled = get_mangled_type_name(expr.ty);
              if (mangled) {
                e.union_name = *mangled;
              }
            }
          }
          for (auto &p : e.payload) {
            replace_expr_refs(*p);
          }
        } else if constexpr (std::is_same_v<T, typed::FieldAccess>) {
          replace_expr_refs(*e.object);
        } else if constexpr (std::is_same_v<T, typed::MethodCall>) {
          replace_expr_refs(*e.object);
          for (auto &a : e.args) {
            replace_expr_refs(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::IndexAccess>) {
          replace_expr_refs(*e.object);
          replace_expr_refs(*e.index);
        } else if constexpr (std::is_same_v<T, typed::Call>) {
          replace_expr_refs(*e.callee);
          for (auto &a : e.args) {
            replace_expr_refs(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::UnaryExpr>) {
          replace_expr_refs(*e.operand);
        } else if constexpr (std::is_same_v<T, typed::BinaryExpr>) {
          replace_expr_refs(*e.left);
          replace_expr_refs(*e.right);
        } else if constexpr (std::is_same_v<T, typed::Assignment>) {
          replace_expr_refs(*e.target);
          replace_expr_refs(*e.value);
        } else if constexpr (std::is_same_v<T, typed::IfExpr>) {
          for (auto &branch : e.branches) {
            replace_expr_refs(*branch.condition);
            replace_block_refs(branch.body);
          }
          if (e.else_branch) {
            replace_block_refs(*e.else_branch);
          }
        } else if constexpr (std::is_same_v<T, typed::MatchExpr>) {
          replace_expr_refs(*e.subject);
          for (auto &arm : e.arms) {
            replace_type_refs(arm.pattern.ty);
            replace_block_refs(arm.body);
          }
        } else if constexpr (std::is_same_v<T, typed::Break>) {
          if (e.value)
            replace_expr_refs(**e.value);
        } else if constexpr (std::is_same_v<T, typed::Continue>) {
          // Nothing
        } else if constexpr (std::is_same_v<T, typed::BuiltinCall>) {
          for (auto &ta : e.type_args) {
            replace_type_refs(ta);
          }
          for (auto &a : e.args) {
            replace_expr_refs(*a);
          }
        } else if constexpr (std::is_same_v<T, typed::RangeExpr>) {
          replace_expr_refs(*e.start);
          replace_expr_refs(*e.end);
        } else if constexpr (std::is_same_v<T, typed::TypeInit>) {
          replace_type_refs(e.ty);
          for (auto &f : e.fields) {
            replace_expr_refs(*f);
          }
        } else if constexpr (std::is_same_v<T, typed::AsExpr>) {
          replace_expr_refs(*e.expr);
          replace_type_refs(e.target_ty);
        }
      },
      expr.value);
}

} // namespace shikimori
