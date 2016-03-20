#include "algorithms.h"
#include <cmath>
#include <cstdio>
#include <stack>
#include <queue>

using namespace std;

int linear_search (int val, int * array, unsigned length){
  for (unsigned i = 0; i < length; i++){
    if (array[i] == val)
      return i;
  }
  return -1;
}

int binary_search (int val, int * array, unsigned length){
  int start = 0, end = length;

  int mid = start + floor ((end - start) / 2.0);
  while (start < end){
    if (array[mid] == val)
      return mid;
    else if (val > array[mid]){
      start = mid+1;
      mid = start + floor ((end - start) / 2.0);
    } else {
      end = mid;
      mid = start + floor ((end - start) / 2.0);
    }
  }

  return -1;
}

int recursive_binary_search(int val, int * array, int min, int max){
  if (min>=max)
    return -1;

  int mid = min + (max-min)/2;
  // printf("%d\n", mid);

  if (array[mid] == val)
    return mid;
  else if (val > array[mid]){
    return recursive_binary_search (val, array, mid+1, max);
  } else {
    return recursive_binary_search (val, array, min, mid);
  }
}

int* bubble_sort(int* array, int min, int max){
  for(int i = min; i < max; i++){
    for(int j = max; j > min; j--){
      if (array[j] < array[j-1]){
        int temp = array[j];
        array[j] = array[j-1];
        array[j-1] = temp;
      }
    }
  }

  return array;
}

int* insertion_sort(int* array, int min, int max){
  for(int i = 1; i <= max; i++){
    int key = array[i];
    int j = i-1;
    while (j >= 0 && array[j] > key){
      array[j+1] = array[j];
      j--;
    }
    array[j+1] = key;
  }
  return array;
}

void merge (int *array, const unsigned &min, const unsigned &mid, const unsigned &max){
  unsigned left = min, right = mid, i = 0;
  int temp[max-min];

  while (left < mid || right < max){
    if (left >= mid){
      temp[i] = array[right];
      right++;
    } else if (right >=  max){
      temp[i] = array[left];
      left++;
    } else if (array[left] <= array[right]){
      temp[i] = array[left];
      left++;
    } else if (array[right] < array[left]){
      temp[i] = array[right];
      right++;
    }

    i++;
  }

  for (unsigned j = min; j < max; j++){
    array[j] = temp[j-min];
  }
}

int* merge_sort(int* array, int min, int max){
  if (max-min < 2) // subarray size == 1
    return array;

  int mid = min + (max - min)/2;
  merge_sort (array, min, mid);
  merge_sort (array, mid, max);

  int left = min, right = mid;

  int scratch[max-min];

  for (int i = 0; i < max-min; i++){
    if (left < mid && (array[left] < array[right] || right == max)) {
      scratch[i] = array[left];
      left++;
    } else {
      scratch[i] = array[right];
      right++;
    }
  }

  for(int i = min; i < max; i++){
    array[i] = scratch[i-min];
  }

  return array;
}

// int* heap_sort (int *array, int min, int max){
//   // heapify (a, min, max)
// }

unsigned long long reverse_64bit(unsigned long long input){
  unsigned long long out = 0;

  for (int i = 0; i < 64; i++){
    out |= (input & 1) << (63 - i);
    input >>= 1;
  }
  return out;
}

std::shared_ptr<binary_tree<int>>
generate_binary_tree(unsigned num_nodes, int key){

  if (num_nodes == 0) return nullptr;

  auto root = std::make_shared<binary_tree<int>>();

  root->data = key;
  num_nodes--;
  key++;

  queue<shared_ptr<binary_tree<int>>> next;

  next.push(root);

  while (num_nodes || !next.empty()){
    auto current = next.front();
    next.pop();

    unsigned direct_children = num_nodes < 2 ? num_nodes: 2;

    unsigned side = rand() % 2;

    for(unsigned i = 0; i < direct_children; i++){
      auto child = std::make_shared<binary_tree<int>>();

      child->data = key;
      num_nodes--;
      key++;

      if(side == 0){
        side++;
        current->left = child;
      } else {
        side--;
        current->right = child;
      }
      next.push(child);
    }
  }
  return root;
}
