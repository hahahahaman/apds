#ifndef _DATA_H_
#define _DATA_H_

/*
  Things to think about:
  - arrays
*/

/* linked lists */

struct singly_node {
  int data;
  singly_node* next;
};

struct singly_list {
  singly_node* head;
};

struct doubly_node {
  int data;
  doubly_node *prev = nullptr, *next = nullptr;
};

struct doubly_list {
  doubly_node *head, *tail;
};

void list_traverse (const singly_list &list);

singly_node *list_search (int val, const singly_list &list);

void insert_after (singly_node *node, singly_node *new_node);

void remove_after (singly_node *node);

void list_insert_beginning (singly_list &list, singly_node *new_node);

void list_remove_beginning (singly_list &list);

/* dynamic array */

/* class dynamic_array { */
/*  private: */
/*   unsigned capacity; */
/* }; */

/* stack */

class bounded_stack {
 public:
  bounded_stack(unsigned bound);
  ~bounded_stack();

  int push (int n);
  int pop ();

  unsigned get_bound() const;
 private:
  unsigned max_size;
  unsigned top;
  int * array;
};

/* queue */

class list_queue {
 public:
  list_queue();
  ~list_queue();

  void enqueue(int n);

  int dequeue();

 private:
  doubly_list list;
};

#endif
