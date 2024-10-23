#pragma once

#include <assert.h>
#include <cstring>
#include <initializer_list>
#include <stdio.h>

#include "common.h"

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
    while (strcmp(slots[h].kv.key, key) != 0 && count < N)
      h = (h + 1) % N;
    return count < N ? &slots[h].kv.value : nullptr;
  }

private:
  // djb2 hash
  u32 hash(const char *key) {
    u64 hash = 5381;
    i32 c;
    while ((c = *key++) != 0)
      hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
  }

private:
  Slot slots[N];
};
