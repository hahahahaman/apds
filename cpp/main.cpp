/*
  main.cpp
  Code that uses these data structures and algorithms.
 */

#include <iostream>
#include <cstdio>

#include "algorithms.h"
#include "data.h"

using namespace std;

// heap and stack

// heap:
// new operator allocates memory on heap
// lots of space to allocate
// allocating memory on the heap is relatively slow
// memory leaks can occur if it is not deallocated
// dereferencing a pointer is slower than accessing a variable

// stack:
// functions are pushed and popped off of the stack
// allocating memory on stack is faster
// memory allocated on stack is deleted when out of scope

// stack is small, should not create large function nesting or data
// allocation

/*
  1. function call
  2. stack frame constructed and pushed onto stack
     // return address (address of instruction after function call)
     // function arguments
     // local variables
     // saved copies of registers modified by the function, that are then
     restored
  3. cpu jumps to function's start point
  4. instructions inside of function begin executing
*/

int main() {
  int * ptr = new int; // ptr assigns 4 bytes, or 32 bits in the heap

  int *array = new int[10]; // array is assigned 40 bytes in heap

  delete ptr;
  delete[] array;

  singly_list slist;
  int snodes = 4;
  singly_node sn[snodes];

  for (int i = 0; i < snodes; i++){
    sn[i].data = i;
    if (i+1 < snodes)
      sn[i].next = &sn[i+1];
    else
      sn[i].next = nullptr;
  }
  slist.head = &sn[0];

  // list_traverse(slist);

  int search[] = {0, 1, 2, 3, 4};

  cout << recursive_binary_search(4, search, 0, 4) << endl;

  cout << binary_search(0, search, 5) << endl;
  // cout << "heloo" << endl;
  return 0;
}
