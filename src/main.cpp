#include "tokenizer.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string_view>

using namespace shikimori;

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <source file>\n";
    return 1;
  }

  std::string_view source_file = argv[1];

  std::ifstream file(source_file.data());
  if (!file) {
    std::cerr << "Error: Cannot open file '" << source_file << "'\n";
    return 1;
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string source = buffer.str();

  Tokenizer tokenizer(source);
  std::vector<Token> tokens = tokenizer.tokenize();

  for (const auto &token : tokens) {
    token.dump();
  }

  return 0;
}
