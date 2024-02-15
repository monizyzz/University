#include "Listas.h"

typedef LInt Stack;

void initStack (Stack *s);
int SisEmpty (Stack s);
int push (Stack *s, int x);
int pop (Stack *s, int *x);
int top (Stack s, int *x);