#pragma once

#include <cstdio>
#include <sys/stat.h>

#include "common.h"
#include "mem.h"

struct FileBuffer {
  u8 *content;
  u32 len;
};

static FileBuffer read_entire_file(Arena *arena, const char *filename) {
  FILE *f = fopen(filename, "rb");
  fseek(f, 0, SEEK_END);
  u32 fsize = ftell(f);
  fseek(f, 0, SEEK_SET);
  u8 *content = arena->alloc(fsize + 1);
  fread(content, fsize, 1, f);
  content[fsize] = 0;

  return FileBuffer{
      .content = content,
      .len = fsize,
  };
}
