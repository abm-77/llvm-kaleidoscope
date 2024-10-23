
#include "collections.h"
#include "mem.h"

int main(void) {
  StaticStringMap<int, 5> test({
      {"who", 1},
      {"what", 2},
      {"when", 3},
      {"when", 4},
      {"why", 5},
  });

  auto arena = Arena::init(4096);

  arena.deinit();

  return 0;
}
