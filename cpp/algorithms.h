#ifndef _ALGORITHMS_H_
#define _ALGORITHMS_H_

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

int* merge_sort(int * array, int min, int max);

/* int* heap_sort(int * array, int min, int max); */

/* int* quick_sort(int * array, int min, int max); */

/* int* radix_sort(int * array, int min, int max); */

#endif
