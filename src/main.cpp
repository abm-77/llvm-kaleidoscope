
#include "collections.h"
#include "lexer.h"
#include "mem.h"

int main(void) {
  auto arena = Arena::init(4096);
  Lexer::lex(&arena, "programs/test.hex");
  arena.deinit();
  return 0;
}
