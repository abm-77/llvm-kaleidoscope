#pragma once

#include <assert.h>
#include <cstdint>
#include <cstring>
#include <initializer_list>
#include <stdio.h>

#include "common.h"
#include "mem.h"

template <typename T, u32 N> class StaticArray {
public:
  StaticArray() : capacity(N) {}

  void set(u32 index, T value) {
    assert_inbounds(index);
    items[index] = value;
  }

  T *get(u32 index) {
    assert_inbounds(index);
    return &items[index];
  }

  u32 cap() { return capacity; }

private:
  inline void assert_inbounds(u32 index) {
    assert(index >= 0 && index < capacity);
  }

private:
  const u32 capacity;
  T items[N];
};

// StaticStringMap maps static strings to values in an underlying
// statically allocated array.
template <typename DataType, u32 N> class StaticStringMap {
private:
  struct KV {
    const char *key;
    DataType value;
  };
  struct Slot {
    KV kv;
    bool present;
  };

public:
  StaticStringMap(std::initializer_list<KV> kvs) {
    for (KV kv : kvs) {
      u32 h = hash(kv.key) % N;
      while (slots[h].present)
        h = (h + 1) % N;
      slots[h] = Slot{kv, true};
    }
  }

  const DataType *get(const char *key) const {
    u32 count = 0;
    u32 h = hash(key) % N;
    while (strcmp(slots[h].kv.key, key) != 0 && count++ < N)
      h = (h + 1) % N;
    return count < N ? &slots[h].kv.value : nullptr;
  }

private:
  // djb2 hash
  u32 hash(const char *key) const {
    u64 hash = 5381;
    i32 c;
    while ((c = *key++) != 0)
      hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
  }

private:
  Slot slots[N];
};

template <typename DataType> class DynArray {
private:
  struct ChunkHeader {
    ChunkHeader *next;
  };

public:
  class Iterator {
  public:
    void next() {
      idx += 1;

      if (done())
        return;

      if (idx % chunk_size == 0) {
        curr_chunk = curr_chunk->next;
      }

      i32 i = idx % chunk_size;

      DataType *items = get_chunk(curr_chunk);
      v = &items[i];
    }

    bool done() { return idx >= len; }

    DataType *value() { return v; }

    Iterator(ChunkHeader *start, u32 chunk_size, u32 len)
        : idx(0), len(len), chunk_size(chunk_size), curr_chunk(start) {
      v = &get_chunk(curr_chunk)[0];
    }

    Iterator()
        : idx(0), len(0), chunk_size(0), v(nullptr), curr_chunk(nullptr) {}

  private:
    i32 idx;
    u32 len;
    u32 chunk_size;
    DataType *v;
    ChunkHeader *curr_chunk;
  };

public:
  DynArray(Arena *arena, u32 base_capacity)
      : arena(arena), chunk_size(base_capacity), length(0), cap(base_capacity) {
    head = alloc_chunk();
    head->next = nullptr;
    tail = head;
  }

  void push(DataType v) {
    DataType *items = get_chunk(tail);

    u32 idx = length % chunk_size;
    items[idx] = v;
    length += 1;

    if (idx == chunk_size - 1) {
      tail->next = alloc_chunk();
      tail = tail->next;
      cap += chunk_size;
    }
  }

  DataType get(u32 i) {
    u32 chunk_idx = i / chunk_size;
    ChunkHeader *chunk = head;

    for (i32 i = 0; i < chunk_idx; i++)
      chunk = chunk->next;

    u32 idx = i % chunk_size;
    return get_chunk(chunk)[idx];
  }

  u32 len() { return length; }

  Iterator make_iter() { return Iterator(head, chunk_size, length); }

private:
  static DataType *get_chunk(ChunkHeader *chunk) {
    return reinterpret_cast<DataType *>(reinterpret_cast<uintptr_t>(chunk) +
                                        sizeof(ChunkHeader));
  }
  ChunkHeader *alloc_chunk() {
    return reinterpret_cast<ChunkHeader *>(
        arena->alloc(sizeof(ChunkHeader) + sizeof(DataType) * chunk_size));
  }

private:
  Arena *arena;
  ChunkHeader *head;
  ChunkHeader *tail;
  u32 chunk_size;
  u32 length;
  u32 cap;
};
