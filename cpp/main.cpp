/*
  main.cpp
  Code that uses these data structures and algorithms.
 */

#include <iostream>
#include <cstdio>
#include <climits>
#include <string>
#include <memory>
#include <stack>
#include <vector>
#include <utility>
#include <ctime>

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

void print_array(int * array, unsigned length){
  for(int i =0 ; i < length; i++)
    printf("%d\n", array[i]);
}

unsigned coin_problem(unsigned sum, unsigned *values, unsigned num_values){
  unsigned min[sum+1];

  min[0] = 0;
  for(unsigned i = 1;i < sum+1; i++){
    min[i] = UINT_MAX;
  }

  for (unsigned i = 1; i < sum+1; i++){
    for(int j = 0; j < num_values; j++){
      unsigned value = values[j];
      if(value <= i && min[i-value]+1 < min[i]){
        min[i] = min[i-value]+1;
      }
    }
  }

  return min[sum];
}

vector<int> preorder_keys, inorder_keys;

int main() {
  srand(time(NULL));

  // int * ptr = new int; // ptr assigns 4 bytes, or 32 bits in the heap

  // int *array = new int[10]; // array is assigned 40 bytes in heap

  // delete ptr;
  // delete[] array;

  // singly_list slist;
  // int snodes = 4;
  // singly_node sn[snodes];

  // for (int i = 0; i < snodes; i++){
  //   sn[i].data = i;
  //   if (i+1 < snodes)
  //     sn[i].next = &sn[i+1];
  //   else
  //     sn[i].next = nullptr;
  // }
  // slist.head = &sn[0];

  // list_traverse(slist);

  // int search[] = {2, 1, 4, 3, 0};

  // cout << recursive_binary_search(4, search, 0, 4) << endl;

  // cout << binary_search(0, search, 5) << endl;

  // merge_sort(search, 0, 5);
  // print_array(search, 5);

  // unsigned coin_values[] = {1, 3, 5};
  // cout << coin_problem(11, coin_values, 3) << endl;

  // list_queue* q = new list_queue;

  // for(int i =0; i <10; i++) q->enqueue(i);

  // for (int i = 0; i < 10; i++) cout << q->dequeue() << endl;

  // delete q;

  // string str1("1231414");
  // string str2(str1.rbegin(), str1.rend());

  // cout << str2 <<endl;

  // cout << reverse_64bit(1) << endl;

  // shared_ptr<int> spi = make_shared<int>(10);
  // cout << *spi << endl;

  shared_ptr<binary_tree<int>> tree = generate_binary_tree(10, 0);

  auto add_inorder =
    [=](shared_ptr<binary_tree<int>> n)->void
    {
      inorder_keys.push_back(n->data);
    };

  auto add_preorder =
    [=](shared_ptr<binary_tree<int>> n)->void
    {
      preorder_keys.push_back(n->data);
    };

  inorder_traversal<int>(tree,add_inorder);

  for (int i : inorder_keys) printf("%d ", i);
  printf("\n");

  preorder_traversal<int>(tree, add_preorder);
  for (int i : preorder_keys) printf("%d ", i);
  printf("\n");

  int start_index = 0;
  auto reconstructed_tree =
    inorder_preorder_reconstruction<int>(inorder_keys, preorder_keys,
                                         0, inorder_keys.size(), start_index);

  inorder_keys.clear();
  preorder_keys.clear();

  inorder_traversal<int>(reconstructed_tree,add_inorder);

  for (int i : inorder_keys) printf("%d ", i);
  printf("\n");

  preorder_traversal<int>(reconstructed_tree, add_preorder);
  for (int i : preorder_keys) printf("%d ", i);
  printf("\n");

  return 0;
}
