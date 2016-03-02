#include "algorithms.h"
#include <cmath>

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
