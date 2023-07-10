#include "Listas.h"

typedef struct {
    LInt inicio,fim;
  } Queue;

void initQueue (Queue *q);
int QisEmpty (Queue q);
int enqueue (Queue *q, int x);
int dequeue (Queue *q, int *x);
int frontQ (Queue q, int *x);

typedef LInt QueueC;

void initQueueC (QueueC *q);
int QisEmptyC (QueueC q);
int enqueueC (QueueC *q, int x);
int dequeueC (QueueC *q, int *x);
int frontC (QueueC q, int *x);