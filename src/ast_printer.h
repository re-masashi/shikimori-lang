#pragma once

#include "ast.h"
#include <iostream>

using namespace std;

namespace shikimori {

void dump_ast(const ast::Program &program, ostream &os = cout);

} // namespace shikimori
