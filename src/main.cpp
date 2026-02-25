#include "ast_printer.h"
#include "parser.h"
#include "tokenizer.hpp"
#include <expected>
#include <filesystem>
#include <fstream>
#include <print>
#include <string>
#include <string_view>
#include <vector>

using namespace shikimori;

[[nodiscard]] std::expected<std::string, std::string>
read_source(std::string_view path) {
  std::ifstream file(std::filesystem::path(path),
                     std::ios::binary | std::ios::ate);

  if (!file) {
    return std::unexpected(std::format("Error: Cannot open file '{}'", path));
  }

  const auto size = file.tellg();
  file.seekg(0, std::ios::beg);

  std::string content;
  content.resize(static_cast<size_t>(size));

  if (!file.read(content.data(), size)) {
    return std::unexpected("Error: Failed reading file content");
  }

  return content;
}

int main(int argc, char *argv[]) {
  const std::vector<std::string_view> args(argv, argv + argc);

  if (args.size() < 2) {
    std::print(stderr, "Usage: {} <source file>\n", args[0]);
    return 1;
  }

  const std::string_view source_file = args[1];
  auto filepath = std::filesystem::path(source_file);

  return read_source(source_file)
      .and_then(
          [&filepath](std::string source) -> std::expected<int, std::string> {
            Tokenizer tokenizer(source);
            std::vector<Token> tokens = tokenizer.tokenize();

            Parser parser(tokens, source, filepath);
            auto program = parser.parse();

            if (!program) {
              for (const auto &err : parser.get_errors()) {
                std::print(stderr, "{}\n", err);
              }
              return 1;
            }

            dump_ast(*program);
            return 0;
          })
      .or_else([](const std::string &error) -> std::expected<int, std::string> {
        std::print(stderr, "{}\n", error);
        return 1;
      })
      .value();
}
