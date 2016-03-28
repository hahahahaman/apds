#ifndef _ALGORITHMS_H_
#define _ALGORITHMS_H_

#include <memory>
#include <queue>
#include <stdexcept>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <functional>
#include "data.h"

/*
  STUFF:
  - searching
  - sorting
  - tree traversal
  - graph traversal
  - prefix tree searches
*/

int linear_search (int val, int * array, unsigned length);

int binary_search (int val, int * array, unsigned length);

int recursive_binary_search (int val, int * array, int min, int max);

/* assuming min is inclusive, max is exclusive*/

int* bubble_sort(int * array, int min, int max);

int* insertion_sort(int * array, int min, int max);

void merge (int *array, const unsigned &min, const unsigned &mid, const unsigned &max);

int* merge_sort(int * array, int min, int max);

int heap_parent(int i);

int heap_left(int i);

int heap_right(int i);

void sift_down(std::vector<int> &array, int start, int end);

void heapify(std::vector<int> &array);

int* heap_sort(std::vector<int> &array);

int partition(int * array, int min, int max)

int* quick_sort(int * array, int min, int max);

/* int* radix_sort(int * array, int min, int max); */

unsigned long long reverse_64bit(unsigned long long input);


template <typename T>
void preorder_traversal(std::shared_ptr<binary_tree<T>> n,
                        std::function<void(std::shared_ptr<binary_tree<T>>)> visit){
  visit(n);
  if (n->left) preorder_traversal(n->left, visit);
  if (n->right) preorder_traversal(n->right, visit);
}

template <typename T>
void inorder_traversal(std::shared_ptr<binary_tree<T>> n,
                       std::function<void(std::shared_ptr<binary_tree<T>>)> visit){
  if(n->left) inorder_traversal<T>(n->left, visit);
  visit(n);
  if(n->right) inorder_traversal<T>(n->right, visit);
}

/* template <typename T> */
/* void morris_inorder_traversal(){ */
/* } */

template <typename T>
void postorder_traversal(std::shared_ptr<binary_tree<T>> n,
                         std::function<void(std::shared_ptr<binary_tree<T>>)> visit){
  if(n->left) postorder_traversal<T>(n->left, visit);
  if(n->right) postorder_traversal<T>(n->right,visit);
  visit(n);
}

template <typename T>
void levelorder_traversal(std::shared_ptr<binary_tree<T>> n,
                          std::function<void(std::shared_ptr<binary_tree<T>>)> visit){
  std::queue<std::shared_ptr<binary_tree<T>>> nodes;
  nodes.push(n);
  while(!nodes.empty()){
    n = nodes.top();
    nodes.pop();

    visit(n);

    if (n->left) nodes.push(n->left);
    if (n->right) nodes.push (n->left);
  }
}

template <typename T>
bool is_binary_tree_symmetric_helper(std::shared_ptr<binary_tree<T>> l,
                                    std::shared_ptr<binary_tree<T>> r){
  if (!l && !r) return true;

  if (l && r)
    return l->data == r->data &&
      is_binary_tree_symmetric_helper<T>(l->left, r->right) &&
      is_binary_tree_symmetric_helper<T>(l->right, r->left);

  return false;
}

template <typename T>
bool is_binary_tree_symmetric(std::shared_ptr<binary_tree<T>> n){
  if (!n) return true;
  return is_binary_tree_symmetric_helper<T>(n->left, n->right);
}

template <typename T>
std::shared_ptr<binary_tree<T>>
inorder_preorder_reconstruction(const std::vector<T> &inorder_keys,
                                const std::vector<T> &preorder_keys,
                                const int &inorder_lo,
                                const int &inorder_hi,
                                int &preorder_index){

  // no nodes left
  if (inorder_hi <= inorder_lo || preorder_index == preorder_keys.size())
    return nullptr;

  T root_key = preorder_keys[preorder_index];

  int root_pos = inorder_lo;
  for(; root_pos < inorder_hi; root_pos++){
    if (inorder_keys[root_pos] == root_key) break;
  }

  /* std::cout << root_key << " "; */
  /* printf("%d %d %d %d %d\n", root_pos, inorder_lo,
     inorder_hi, preorder_lo, preorder_hi); */

  if (root_pos == inorder_hi)
    throw std::invalid_argument("inorder and preorder not of the same tree");

  int num_left = root_pos - inorder_lo;
  int num_right = inorder_hi - (root_pos+1);

  std::shared_ptr<binary_tree<T>> new_root(new binary_tree<T>());

  new_root->data = root_key;
  preorder_index++;

  new_root->left = inorder_preorder_reconstruction<T>(inorder_keys, preorder_keys,
                                                      root_pos - num_left,
                                                      root_pos,
                                                      preorder_index);

  new_root->right = inorder_preorder_reconstruction<T>(inorder_keys, preorder_keys,
                                                       root_pos+1,
                                                       root_pos+1+num_right,
                                                       preorder_index);

  return new_root;
}

std::shared_ptr<binary_tree<int>>
generate_binary_tree(unsigned num_nodes, int key);

#endif
