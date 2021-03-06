#include "data.h"
#include <cstdio>

void list_traverse (const singly_list &list){
  singly_node* current = list.head;

  while (current != nullptr){
    printf("%d\n", current->data);
    current = current->next;
  }
}

singly_node *list_search (int val, const singly_list &list){
  singly_node *current = list.head;

  while (current != nullptr){
    if (current->data == val)
      return current;

    current = current->next;
  }

  return nullptr;
}

void insert_after(singly_node * node, singly_node * new_node){
  node->next = new_node;
}

void remove_after (singly_node * node){
  node->next = nullptr;
}

void list_insert_beginning (singly_list &list, singly_node *new_node){
  new_node->next = list.head;
  list.head = new_node;
}

void list_remove_beginning (singly_list &list){
  if (list.head != nullptr) list.head = list.head->next;
}

/* stack */

bounded_stack::bounded_stack (unsigned bound){
  max_size = bound;
  top = 0;
  array = new int [bound];
}

bounded_stack::~bounded_stack (){
  if (array) delete [] array;
}

int bounded_stack::push(int n){
  if (top + 1 >= max_size)
    return -1;

  array[top] = n;
  top++;

  return top;
}

int bounded_stack::pop(){
  if (top > 0){
    top--;
    return array[top+1];
  }

  printf("stack empty\n");
  return -1;
}

unsigned bounded_stack::get_bound() const {
  return max_size;
}

/* queue */

list_queue::list_queue(){
  list.head = nullptr;
  list.tail = nullptr;
}

list_queue::~list_queue(){
  while(list.head != nullptr){
    this->dequeue();
  }
}

void list_queue::enqueue(int n){
  doubly_node* fresh = new doubly_node;
  fresh->data = n;

  if (list.head == nullptr){
    list.head = fresh;
    list.tail = fresh;
  } else {
    list.tail->next = fresh;
    fresh->prev = list.tail;
    list.tail = fresh;
  }
}

int list_queue::dequeue(){
  int result = -1;
  if(list.head != nullptr){
    result = list.head->data;

    doubly_node *to_delete = list.head;
    if (list.head->next != nullptr) {
      list.head->next->prev = nullptr;
      list.head = list.head->next;
    } else {
      list.head = nullptr;
      list.tail = nullptr;
    }
    delete to_delete;
  }

  return result;
}
