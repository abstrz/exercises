#include "stdlib.h"

block_meta *
find_free_block(block_meta **last, size_t size)
{
  block_meta *current = global_base;
  while (current && !(current->free && current->size >= size)){
    *last = current;
    current = current->next;
  }
  return current;
}

block_meta *
request_space(block_meta *last, size_t size)
{
  block_meta *block = sbrk(0);
  void *request = sbrk(size + META_SIZE); //increments pointer by size + META_SIZE, then returns old ptr value

  assert((void *) block == request); //not thread safe

  if(request == (void *) -1) 
    return NULL; //sbrk failed

  if(last) //null on first request
    last->next = block;

  block->size = size;
  block->next = NULL;
  block->free = 0;
  block->debug = 0x12345678;

  return block;
}

block_meta *get_block_ptr(void *ptr)
{
  return (block_meta *)ptr - 1;
}

void *
malloc(size_t size)
{
  block_meta *block;
  // TODO: align size?

  if(size <= 0)
    return NULL;

  if(!global_base) { //First call
    block = request_space(NULL, size);
    if (!block)
      return NULL;
  global_base = block;
  }
  else{
    block_meta *last = global_base;
    block = find_free_block(&last, size);
    if (!block) //Failed to find free block
      return NULL;
    else{
      // TODO: consider splitting block here
      block->free = 0;
      block->debug = 0x77777777;
    }
  }

  return block+1; //we want to return a pointer to the region after block_meta. Remember, we sbrk allocates META_SIZE + size space, and we used up that first META_SIZE on the heap for block_meta.
                    //Since block is of type block_meta, + 1 increments the address by sizeof(block_meta).
}
      
}

void free(void* ptr)
{
  if (!ptr){
    return;
  }

  // TODO: consider merging blocks once splitting blocks is implemented
  block_meta *block_ptr = get_block_ptr(ptr);
  assert(block_ptr->free == 0);
  assert(block_ptr->magic == 0x77777777 || block_ptr->magic == 0x12345678);
  block_ptr->free = 1;
  block_ptr->magic = 0x55555555;
}
