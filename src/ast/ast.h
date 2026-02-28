#pragma once

// Main AST header - includes all AST components
// For faster compiles, include only what you need:
//   - ast_fwd.hpp   - forward declarations only
//   - ast_types.hpp - type annotations
//   - ast_expr.hpp  - expressions (+ types)
//   - ast_stmt.hpp  - statements (+ expressions)
//   - ast_decl.hpp  - declarations (+ statements)

#include "ast/ast_decl.hpp"
#include "ast/ast_expr.hpp"
#include "ast/ast_stmt.hpp"
#include "ast/ast_types.hpp"
