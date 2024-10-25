
#include "lexer.h"
#include "mem.h"
#include <cstdio>

int main(void) {
  auto arena = Arena::init(MB(2));
  auto tokens = Lexer::lex(&arena, "programs/test.hex");

  for (auto it = tokens.make_iter(); !it.done(); it.next()) {
    const Token *token = it.value();

    char buf[64];
    memcpy(buf, token->lexeme.buffer + token->lexeme.start, token->lexeme.len);
    buf[token->lexeme.len] = 0;
    printf("%s\n", buf);
  }

  printf("%lu\n", sizeof(Token));

  arena.deinit();
  return 0;
}
