#include "abin.h"



ABin newABin (int r, ABin e, ABin d) {
   ABin a = malloc (sizeof(struct nodo));
   if (a!=NULL) {
      a->valor = r; a->esq = e; a->dir = d;
   }
   return a;
}

ABin RandArvFromArray (int v[], int N) {
   ABin a = NULL;
    int m;
    if (N > 0){
    	m = rand() % N;
    	a = newABin (v[m], RandArvFromArray (v,m), RandArvFromArray (v+m+1,N-m-1));
    }
    return a;	
}

int altura (ABin a){
    return (-1);
}

int nFolhas (ABin a){
    return (-1);
}

ABin maisEsquerda (ABin a){
    return NULL;
}

void imprimeNivel (ABin a, int l){
    
}

int procuraE (ABin a, int x){
    return (-1);
}

struct nodo *procura (ABin a, int x){

    return NULL;
}

int nivel (ABin a, int x){
    int count = 0;

    while (a != NULL) {
        if(a->valor == x) return count;
        else if (a->valor < x) {
            a = a->esq;
        } else {
            a = a->dir;
        }
        count++;
    }
    return (-1);
}

void imprimeAte (ABin a, int x){
    if (a == NULL) {
        return;
    }
    
    imprimeAte(a->esq, x);

    if (a->valor < x) {
        printf ("%d ->", a->valor);
        imprimeAte(a->dir, x);
    }
}