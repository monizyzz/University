#include "Deque.h"
#include "Queue.h"
#include "Stack.h"
#include <stdio.h>

int main() {
    int i, a, b;
    Stack s;
    Queue q;
    Deque d;
    printf("_______________ Testes _______________\n\n");
    printf("Stack:\n");
    initStack(&s);
    push(&s, 1);
    push(&s, 1);
    // altere este código de forma a que a stack tenha no final
    // do ciclo a sequencia com os numeros de fibonacci
    for (i = 0; i < 10; i++) {
        pop(&s, &a);
        top(s, &b);
        push(&s, b);
        push(&s, a + b);
    }
    while (!SisEmpty(s)) {
        pop(&s, &a);
        printf("%d ", a);
    }

    printf("\nQueue:\n");
    initQueue(&q);
    enqueue(&q, 1);
    enqueue(&q, 1);
    for (i = 0; i < 10; i++) {
        dequeue(&q, &a);
        frontQ(q, &b);
        enqueue(&q, a);
        enqueue(&q, a + b);
    }
    while (!QisEmpty(q)) {
        dequeue(&q, &a);
        printf("%d ", a);
    }

    printf("\nDeque:\n");
    initDeque(&d);
    pushFront(&d, 1);
    pushBack(&d, 1);
    for (i = 0; i < 10; i++) {
        popBack(&d, &a);
        back(d, &b);
        pushBack(&d, a);
        pushFront(&d, a + b);
    }

    popMax(&d, &a);
    printf("Max: %d \n", a);

    while (!DisEmpty(d)) {
        popBack(&d, &a);
        printf("%d ", a);
    }

    printf("\n\n___________ Fim dos Testes ___________\n\n");
    return 0;
}
