#include "import_resolver.h"
#include <cstdlib>
#include <filesystem>
#include <iostream>

namespace shikimori {

void ImportResolver::collect(const ast::Program &program) {
  visited_paths.clear();
  resolved_paths.clear();
  collect_recursive(program);
}

void ImportResolver::collect_recursive(const ast::Program &program) {
  for (auto &declaration : program.declarations) {
    if (auto *use = std::get_if<ast::UseDecl>(&declaration.value)) {
      resolve_use(*use, program.span.file);
    }
  }
}

void ImportResolver::collect_from_file(const string &path) {
  for (const auto &visited : import_stack) {
    if (visited == path) {
      std::cerr << "Warning: circular import detected" << std::endl;
      std::cerr << "Import chain: ";
      for (const auto &p : import_stack) {
        std::cerr << p << " -> ";
      }
      std::cerr << path << std::endl;
      return;
    }
  }

  if (visited_paths.contains(path)) {
    return;
  }
  visited_paths.insert(path);
  import_stack.push_back(path);

  auto program = parse_file(path);
  if (!program) {
    import_stack.pop_back();
    return;
  }

  resolved_paths.push_back(path);
  programs.emplace(path, make_unique<ast::Program>(std::move(*program)));
  collect_recursive(*program);

  import_stack.pop_back();
}

optional<string> ImportResolver::resolve_use(const ast::UseDecl &use,
                                             string file_path) {
  auto resolved = resolve_path(use.path, file_path);
  if (resolved.empty()) {
    return nullopt;
  }

  import_map[resolved] = use;

  if (!visited_paths.contains(resolved)) {
    collect_from_file(resolved);
  }

  return resolved;
}

optional<ast::Program> ImportResolver::parse_file(const string &path) {
  ifstream file(path);
  if (!file) {
    return nullopt;
  }

  string source((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

  Tokenizer tokenizer(source);
  vector<Token> tokens = tokenizer.tokenize();

  Parser parser(tokens, source, path);
  return parser.parse();
}

string ImportResolver::resolve_path(const string &path,
                                    const string &current_file) {
  namespace fs = std::filesystem;

  const char *home_env = getenv("HOME");
  string home = home_env ? home_env : "/root";

  fs::path shikimori_home = fs::path(home) / ".shikimori";

  if (path.starts_with("std/")) {
    return (shikimori_home / path / ".shiki").string();
  }

  if (path.starts_with("core/")) {
    return (shikimori_home / path / ".shiki").string();
  }

  fs::path current_dir = fs::path(current_file).parent_path();
  fs::path import_path = path;

  if (import_path.is_relative() && !path.starts_with("./") &&
      !path.starts_with("../") && !path.starts_with("..")) {
    return (shikimori_home / "packages" / import_path / ".shiki").string();
  }

  fs::path full_path;
  if (path.starts_with("./")) {
    full_path = current_dir / path.substr(2);
  } else if (path.starts_with("../")) {
    size_t dots = 0;
    size_t i = 0;
    while (i < path.size() && path[i] == '.') {
      dots++;
      i++;
    }
    fs::path temp = current_dir;
    for (size_t j = 0; j < dots - 1; j++) {
      temp = temp.parent_path();
    }
    full_path = temp / path.substr(dots) / ".shiki";
  } else {
    full_path = current_dir / path / ".shiki";
  }

  return full_path.string();
}

} // namespace shikimori
