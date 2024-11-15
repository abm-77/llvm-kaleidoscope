
#include "lexer.h"
#include "mem.h"
#include "parser.h"
#include <cstdio>

int main(void) {
  auto arena = Arena::init(MB(2));
  auto tokens = Lexer::lex(&arena, "programs/test.hex");

  for (auto it = tokens.make_iter(); !it.done(); it.next()) {
    const Token *token = it.value();
    char buf[64];
    memcpy(buf, token->lexeme.ptr, token->lexeme.len);
    buf[token->lexeme.len] = 0;
    printf("%s\n", buf);
  }

  auto ast_exprs = Parser::parse(&arena, tokens);

  arena.deinit();
  return 0;
}
