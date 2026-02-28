#pragma once
#include <filesystem>
#include <fstream>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "ast/ast.h"
#include "parser/parser.h"
#include "parser/tokenizer.hpp"

using namespace std;

namespace shikimori {

class ImportResolver {
public:
  vector<string> resolved_paths;
  map<string, unique_ptr<ast::Program>> programs;
  map<string, ast::UseDecl> import_map; // path -> UseDecl that imported it

  void collect(const ast::Program &program);
  optional<string> resolve_use(const ast::UseDecl &use, string file_path);
  string resolve_path(const string &path, const string &current_file);
  optional<ast::Program> parse_file(const string &path);

private:
  set<string> visited_paths;
  vector<string> import_stack; // Track current import chain for cycle detection
  void collect_recursive(const ast::Program &program);
  void collect_from_file(const string &path);
};

} // namespace shikimori
