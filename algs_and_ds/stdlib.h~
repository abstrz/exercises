#include <stddef.h>
#include <assert.h>
#include <unistd.h>

#define META_SIZE sizeof(block_meta);

extern void *global_base = NULL;

typedef struct block_meta {
  size_t size;
  struct block_meta *next;
  int free;
  int debug;
} block meta;


void *malloc(size_t size);
void free(void *ptr);
