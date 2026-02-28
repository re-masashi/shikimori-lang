#pragma once

#include <iostream>

namespace shikimori {
namespace typed {
struct TypedProgram;
}

void dump_typed_ast(const typed::TypedProgram &program,
                    std::ostream &os = std::cout);

} // namespace shikimori
