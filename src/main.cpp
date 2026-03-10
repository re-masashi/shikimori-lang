// #include "ast/ast_printer.h"
#include "color.hpp"
#include "parser/parser.h"
#include "parser/tokenizer.hpp"
#include "typechecker/typechecker.h"
#include "ast/typed_ast_printer.h"
#include "monomorphization/monomorphizer.h"
#include "error_reporter.h"
#include "utils.h"
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <print>
#include <stdio.h>
#include <string>
#include <string_view>
#include <vector>

using namespace shikimori;
using namespace std;

[[nodiscard]] expected<string, string> read_source(string_view path) {
  TRACE_SCOPE();
  ifstream file(filesystem::path(path), ios::binary | ios::ate);

  if (!file) {
    return unexpected(format("Error: Cannot open file '{}'", path));
  }

  const auto size = file.tellg();
  file.seekg(0, ios::beg);

  string content;
  content.resize(static_cast<size_t>(size));

  if (!file.read(content.data(), size)) {
    return unexpected("Error: Failed reading file content");
  }

  return content;
}

int main(int argc, char *argv[]) {
  TRACE_SCOPE();
  const vector<string_view> args(argv, argv + argc);

  if (args.size() < 2) {
    print(stderr, "Usage: {} <source file>\n", args[0]);
    return 1;
  }

  const string_view source_file = args[1];

  auto filepath = filesystem::path(source_file);

  return read_source(source_file)
      .and_then([&filepath](string source) -> expected<int, string> {
        TRACE_SCOPE();
        Tokenizer tokenizer(source);
        vector<Token> tokens = tokenizer.tokenize();

        Parser parser(tokens, source, filepath);
        auto program = parser.parse();

        if (!program) {
          for (const auto &err : parser.get_errors()) {
            report_error(source, err.span, "parse error", err.message);
          }
          return 1;
        }

        // dump_ast(*program);
        try {
          TRACE_SCOPE();
          Typechecker tc;
          auto typed_program = tc.run(*program);

          Monomorphizer mono(tc);
          auto mono_program = mono.run(typed_program);
          dump_typed_ast(mono_program);
        } catch (const TypeError &e) {
          if (!e.source.empty()) {
            if (e.import_span.start != 0 || e.import_span.end != 0) {
              report_error(source, e.import_span, "type error", "error in imported file");
            } else {
              cerr << Color::BOLD << Color::RED << "error" << Color::RESET << Color::BOLD
                   << ": " << "type error" << Color::RESET << ": " << "error in imported file" << "\n";
              if (!e.span.file.empty()) {
                cerr << "  --> " << e.span.file.string() << "\n";
              }
              cerr << "\n";
            }
            report_error(e.source, e.span, "type error", e.what());
          } else {
            report_error(source, e.span, "type error", e.what());
          }
          return 1;
        }
        return 0;
      })
      .or_else([](const string &error) -> expected<int, string> {
        print(stderr, "{}\n", error);
        return 1;
      })
      .value();
}
