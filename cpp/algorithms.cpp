#include "algorithms.h"
#include <cmath>
#include <cstdio>

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
        array[j-1] = temp
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

int* merge_sort(int* array, int min, int max){
  if (max-min < 2) // subarray size == 1
    return array;

  int mid = min + (max - min)/2;
  merge_sort (a, min, mid, scratch);
  merge_sort (a, mid, max, scratch);

  int left = min, right = mid;

  int scratch[max-min];

  for (int i = 0; i < max-min; i++){
    if (left < mid && (array[left] < array[right] || right == end)) {
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
