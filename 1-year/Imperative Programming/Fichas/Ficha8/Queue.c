#include "Listas.h"
#include "Queue.h"
#include <stdio.h>
#include <stdlib.h>

void initQueue(Queue *q) {
    q->inicio = NULL;
    q->fim = NULL;
}
int QisEmpty(Queue q) { 
    return q.inicio == NULL;
}
int enqueue(Queue *q, int x) { 
    LInt c = newLInt(x, NULL);
    if (c == NULL) {
        return 1;
    }

    if(QisEmpty(*q)) {
        q->fim = c;
    } else {
        q->fim->prox = c;
        q->fim = c;
    }
    return 0;
}
int dequeue(Queue *q, int *x) { 
    if(QisEmpty(*q)) {
        return 1;
    }

    *x = q->inicio->valor;
    LInt tmp = q->inicio;
    q->inicio = q->inicio->prox;

    free(tmp);

    return 0;
}
int frontQ(Queue q, int *x) { 
    if(QisEmpty(q)) {
        return 1;
    }

    *x = (q.inicio)->valor;

    return 0; 
}

typedef LInt QueueC;

void initQueueC(QueueC *q) {

    
};
int QisEmptyC(QueueC q) { return -1; }
int enqueueC(QueueC *q, int x) { return -1; }
int dequeueC(QueueC *q, int *x) { return -1; }
int frontC(QueueC q, int *x) { return -1; }