#pragma once

#include <iostream>

namespace shikimori {
namespace ast {
struct Program;
}

void dump_ast(const ast::Program &program, std::ostream &os = std::cout);

} // namespace shikimori
