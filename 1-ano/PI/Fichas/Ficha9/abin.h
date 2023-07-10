#include <stdio.h>
#include <stdlib.h>

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} * ABin;


ABin newABin (int r, ABin e, ABin d) {
    ABin a = malloc (sizeof(struct nodo));
    if (a!=NULL) {
        a->valor = r; a->esq = e; a->dir = d;
    }
    return a;
}
ABin RandArvFromArray (int v[], int N);
void dumpABin (ABin a, int N);
void freeABin (ABin a);

// 1. 
// a.
int altura (ABin a) {
    int esq, dir, alt = 0; 

    if (a != NULL) {
        esq = 1 + altura (a->esq);
        dir = 1 + altura (a->dir);
        if(esq < dir) alt = dir;
        else alt = esq;
    }
    return alt;
}

// b. 
int nFolhas (ABin a) {
    if (a == NULL) return 0;

    if (a->esq == NULL && a->dir == NULL) {
        return 1;
    }
    return nFolhas (a->esq) + nFolhas (a->dir);
}

// c.
ABin maisEsquerda (ABin a) {
    if (a == NULL || a->esq == NULL) {
        return a;
    }
    return maisEsquerda(a->esq);
}

// d.
void imprimeNivel (ABin a, int l) {
    if (l == 0) {
        if (a != NULL) {
            printf("-> %d\n", a->valor);
        } else {
            imprimeNivel(a->esq, l - 1);
            imprimeNivel(a->dir, l - 1);
        }
    }
}

// e.
int procuraE (ABin a, int x) {
    if (a == NULL) return 0;

    if (a->valor == x) return 1;

    return procuraE(a->esq, x) || procuraE(a->dir, x);
}

struct nodo *procura (ABin a, int x);
int nivel (ABin a, int x);
void imprimeAte (ABin a, int x);