#include "Queue.h"
#include "Stack.h"
#include <stdio.h>

int main() {
    int i;
    struct staticStack s1;
    SStack S1 = &s1;
    struct dinStack d1;
    DStack D1 = &d1;

    struct staticQueue q1;
    SQueue Q1 = &q1;
    struct dinQueue r1;
    DQueue R1 = &r1;

    printf("Testing Stacks .... \n");
    SinitStack(S1);
    DinitStack(D1);
    for (i = 0; i < 15; i++) {
        if (Spush(S1, i) != 0)
            printf("ERROR pushing %d\n", i);
        if (Dpush(D1, i) != 0)
            printf("ERROR pushing %d\n", i);
    }
    ShowSStack(S1);
    ShowDStack(D1);

    printf("Testing Queues .... \n");
    SinitQueue(Q1);
    DinitQueue(R1);
    for (i = 0; i < 15; i++) {
        if (Senqueue(Q1, i) != 0)
            printf("ERROR enqueueing %d\n", i);
        if (Denqueue(R1, i) != 0)
            printf("ERROR enqueueing %d\n", i);
    }
    ShowSQueue(Q1);
    ShowDQueue(R1);

    return 0;
}