#ifndef _LISTAS_
#define _LISTAS_
typedef struct slist {
    int valor;
    struct slist * prox;
} * LInt;

LInt newLInt (int x, LInt xs);

typedef struct dlist {
      int valor;
      struct dlist *ant, *prox;
} *DList;

DList newDList (int x, DList xs);
#endif
