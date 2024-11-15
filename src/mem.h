#pragma once

#include <assert.h>
#include <cstdlib>
#include <string.h>

#include "common.h"

#define KB(x) x * 1024
#define MB(x) KB(x) * 1024
#define GB(x) GB(x) * 1024

class Arena {
public:
  class TempArena {
  public:
    TempArena(Arena *arena) : arena(arena), start_offset(arena->ptr) {}
    void reset() { arena->ptr = start_offset; }

  private:
    Arena *arena;
    u32 start_offset;
  };

public:
  static Arena init(u32 cap) { return Arena(cap); }

  u8 *alloc(u32 alloc_size) {
    u32 aligned_size = clp2(alloc_size);
    assert(ptr + aligned_size < capacity);
    u8 *res = data + ptr;
    ptr += aligned_size;
    memset(res, 0, aligned_size);
    return res;
  }

  template <typename T> T *alloc() {
    return reinterpret_cast<T *>(alloc(sizeof(T)));
  }

  template <typename T> T *alloc(u32 count) {
    return reinterpret_cast<T *>(alloc(sizeof(T) * count));
  }

  TempArena begin_temp() { return TempArena(this); }

  void deinit() {
    free(data);
    capacity = 0;
    ptr = 0;
  }

private:
  Arena(u32 cap) : capacity(cap), ptr(0) {
    data = static_cast<u8 *>(malloc(cap));
    memset(data, 0, cap);
  }

  u32 clp2(u32 v) {
    v -= 1;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    return v + 1;
  }

private:
  u8 *data;
  u32 capacity;
  u32 ptr;
};

struct Slice {
  u8 *ptr;
  u32 len;
};

static u8 *materialize_slice(Arena *arena, Slice *slice) {
  u8 *buff = arena->alloc(slice->len);
  memcpy(buff, slice->ptr, slice->len);
  return buff;
}
