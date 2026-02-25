#pragma once

#include "ast.h"
#include <iostream>
#include <string>

namespace shikimori {

void dump_ast(const ast::Program &program, std::ostream &os = std::cout);

} // namespace shikimori
