
#include "collections.h"
#include "mem.h"

int main(void) {
  auto arena = Arena::init(4096);
  // Lexer::lex(&arena, "programs/test.hex");

  const u32 chunk_size = 8;
  DynArray<i32> arr(&arena, chunk_size);

  for (i32 i = 0; i < (chunk_size * 2) - 4; i++) {
    arr.push(i + 1);
  }

  for (auto i = arr.make_iter(); !i.done(); i.next()) {
    printf("%d\n", *i.value());
  }

  arena.deinit();
  return 0;
}
