#include "Listas.h"
#include "Stack.h"
#include <stdio.h>
#include <stdlib.h>

void initStack(Stack *s) {
    *s = NULL;
}

int SisEmpty(Stack s) { 
    return s == NULL;
}

int push(Stack *s, int x) { 
    LInt c = newLInt(x, *s);
    if (c == NULL){
        return 1;
    }
    *s = c;
    return 0;
}

int pop(Stack *s, int *x) {
    if (SisEmpty(*s)) {
        return 1;
    }
    *x = (*s)->valor;
    LInt tmp = *s; 
    (*s) = (*s)->prox;
    free(tmp);
    return 0;
}
int top(Stack s, int *x) {
    if (SisEmpty(s)) {
        return 1;
    }

    *x = s->valor;
}