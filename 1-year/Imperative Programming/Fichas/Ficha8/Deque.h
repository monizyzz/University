#include "Listas.h"

typedef struct {
     DList back, front;
} Deque;

void initDeque (Deque *q);
int DisEmpty (Deque q);
int pushBack (Deque *q, int x);
int pushFront (Deque *q, int x);
int popBack (Deque *q, int *x);
int popFront (Deque *q, int *x);
int popMax (Deque *q, int *x);
int back (Deque q, int *x);
int front (Deque q, int *x);
