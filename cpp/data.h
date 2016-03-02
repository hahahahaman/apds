#ifndef _DATA_H_
#define _DATA_H_

/*
  Things to think about:
  - arrays
*/

struct singly_node {
  int data;
  singly_node* next;
};

struct singly_list {
  singly_node* head;
};

#endif
