#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

typedef struct list {
  int* vals;
  int size;
} IntList;

IntList* emptyList(int size) {
  IntList* xs = malloc(sizeof(IntList));
  assert(xs != NULL);

  xs->vals = malloc(sizeof(int) * size);
  assert(xs->vals != NULL);

  for (size_t i = 0; i < size; i++) {
    xs->vals[i] = 0;
  }

  xs->size = size;
  return xs;
}

IntList* map(IntList* xs, (int f)(int x)) {
  IntList* ys = malloc(sizeof(IntList));
  assert(ys != NULL);
  ys->next    = malloc(sizeof(int) * xs->size);

  assert(ys->vals != NULL);
  ys->size    = xs->size;

  for (size_t i = 0; i < xs->size; i++) {
    ys->vals[i] = f(xs->vals[i]);
  }
  return ys;
}

int main(void) {
  IntList* xs = emptyList(10);
  
  printf("hi");
}
