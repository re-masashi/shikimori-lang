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

  // Print tokenization for debugging
  Tokenizer tokenizer(source);
  std::vector<Token> tokens = tokenizer.tokenize();

  for (const auto &token : tokens) {
    std::cout << "Token: ";
    switch (token.type) {
    case TokenType::INT_LIT:
      std::cout << "INT_LIT(" << token.lexeme << ")";
      break;
    case TokenType::FLOAT_LIT:
      std::cout << "FLOAT_LIT(" << token.lexeme << ")";
      break;
    case TokenType::STRING_LIT:
      std::cout << "STRING_LIT(\"" << token.lexeme << "\")";
      break;
    case TokenType::BOOL_LIT:
      std::cout << "BOOL_LIT(" << token.lexeme << ")";
      break;
    case TokenType::IDENT:
      std::cout << "IDENT(" << token.lexeme << ")";
      break;
    case TokenType::KW_FN:
      std::cout << "KW_FN";
      break;
    case TokenType::KW_STRUCT:
      std::cout << "KW_STRUCT";
      break;
    case TokenType::KW_UNION:
      std::cout << "KW_UNION";
      break;
    case TokenType::KW_INTERFACE:
      std::cout << "KW_INTERFACE";
      break;
    case TokenType::KW_EXTERN:
      std::cout << "KW_EXTERN";
      break;
    case TokenType::KW_USE:
      std::cout << "KW_USE";
      break;
    case TokenType::KW_WHERE:
      std::cout << "KW_WHERE";
      break;

    case TokenType::KW_MACRO:
      std::cout << "KW_MACRO";
      break;
    case TokenType::KW_LET:
      std::cout << "KW_LET";
      break;
    case TokenType::KW_RETURN:
      std::cout << "KW_RETURN";
      break;
    case TokenType::KW_DEFER:
      std::cout << "KW_DEFER";
      break;
    case TokenType::KW_IF:
      std::cout << "KW_IF";
      break;
    case TokenType::KW_ELSE:
      std::cout << "KW_ELSE";
      break;
    case TokenType::KW_MATCH:
      std::cout << "KW_MATCH";
      break;
    case TokenType::KW_LOOP:
      std::cout << "KW_LOOP";
      break;
    case TokenType::KW_WHILE:
      std::cout << "KW_WHILE";
      break;
    case TokenType::KW_FOR:
      std::cout << "KW_FOR";
      break;
    case TokenType::KW_IN:
      std::cout << "KW_IN";
      break;
    case TokenType::KW_COMPTIME:
      std::cout << "KW_COMPTIME";
      break;
    case TokenType::KW_AS:
      std::cout << "KW_AS";
      break;
    case TokenType::KW_BREAK:
      std::cout << "KW_BREAK";
      break;
    case TokenType::KW_CONTINUE:
      std::cout << "KW_CONTINUE";
      break;
    case TokenType::KW_NULL:
      std::cout << "KW_NULL";
      break;
    case TokenType::KW_TRUE:
      std::cout << "KW_TRUE";
      break;
    case TokenType::KW_FALSE:
      std::cout << "KW_FALSE";
      break;
    case TokenType::KW_IMPL:
      std::cout << "KW_IMPL";
      break;
    case TokenType::KW_I8:
      std::cout << "KW_I8";
      break;
    case TokenType::KW_I16:
      std::cout << "KW_I16";
      break;
    case TokenType::KW_I32:
      std::cout << "KW_I32";
      break;
    case TokenType::KW_I64:
      std::cout << "KW_I64";
      break;
    case TokenType::KW_U8:
      std::cout << "KW_U8";
      break;
    case TokenType::KW_U16:
      std::cout << "KW_U16";
      break;
    case TokenType::KW_U32:
      std::cout << "KW_U32";
      break;
    case TokenType::KW_U64:
      std::cout << "KW_U64";
      break;
    case TokenType::KW_F32:
      std::cout << "KW_F32";
      break;
    case TokenType::KW_F64:
      std::cout << "KW_F64";
      break;
    case TokenType::KW_BOOL:
      std::cout << "KW_BOOL";
      break;
    case TokenType::KW_USIZE:
      std::cout << "KW_USIZE";
      break;
    case TokenType::KW_STRING:
      std::cout << "KW_STRING";
      break;
    case TokenType::LPAREN:
      std::cout << "LPAREN";
      break;
    case TokenType::RPAREN:
      std::cout << "RPAREN";
      break;
    case TokenType::LBRACE:
      std::cout << "LBRACE";
      break;
    case TokenType::RBRACE:
      std::cout << "RBRACE";
      break;
    case TokenType::LBRACKET:
      std::cout << "LBRACKET";
      break;
    case TokenType::RBRACKET:
      std::cout << "RBRACKET";
      break;
    case TokenType::COMMA:
      std::cout << "COMMA";
      break;
    case TokenType::SEMICOLON:
      std::cout << "SEMICOLON";
      break;
    case TokenType::COLON:
      std::cout << "COLON";
      break;
    case TokenType::DOT:
      std::cout << "DOT";
      break;
    case TokenType::QUESTION:
      std::cout << "QUESTION";
      break;
    case TokenType::TILDE:
      std::cout << "TILDE";
      break;
    case TokenType::AT:
      std::cout << "AT";
      break;
    case TokenType::ARROW:
      std::cout << "ARROW";
      break;
    case TokenType::DOUBLE_COLON:
      std::cout << "DOUBLE_COLON";
      break;
    case TokenType::HASH_BRACK:
      std::cout << "HASH_BRACK";
      break;
    case TokenType::PLUS:
      std::cout << "PLUS";
      break;
    case TokenType::MINUS:
      std::cout << "MINUS";
      break;
    case TokenType::STAR:
      std::cout << "STAR";
      break;
    case TokenType::SLASH:
      std::cout << "SLASH";
      break;
    case TokenType::PERCENT:
      std::cout << "PERCENT";
      break;
    case TokenType::EQ:
      std::cout << "EQ";
      break;
    case TokenType::NEQ:
      std::cout << "NEQ";
      break;
    case TokenType::LT:
      std::cout << "LT";
      break;
    case TokenType::GT:
      std::cout << "GT";
      break;
    case TokenType::LTE:
      std::cout << "LTE";
      break;
    case TokenType::GTE:
      std::cout << "GTE";
      break;
    case TokenType::AND:
      std::cout << "AND";
      break;
    case TokenType::OR:
      std::cout << "OR";
      break;
    case TokenType::NOT:
      std::cout << "NOT";
      break;
    case TokenType::BIT_AND:
      std::cout << "BIT_AND";
      break;
    case TokenType::BIT_OR:
      std::cout << "BIT_OR";
      break;
    case TokenType::BIT_XOR:
      std::cout << "BIT_XOR";
      break;
    case TokenType::LSHIFT:
      std::cout << "LSHIFT";
      break;
    case TokenType::RSHIFT:
      std::cout << "RSHIFT";
      break;
    case TokenType::ASSIGN:
      std::cout << "ASSIGN";
      break;
    case TokenType::UNDERSCORE:
      std::cout << "UNDERSCORE";
      break;
    case TokenType::EOF_TOKEN:
      std::cout << "EOF";
      break;
    case TokenType::ERROR:
      std::cout << "ERROR(" << token.lexeme << ")";
      break;
    default:
      std::cout << "UNKNOWN";
    }
    std::cout << " SPAN " << token.start << ".." << token.end << "\n";
  }

  return 0;
}
