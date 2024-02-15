#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct lligada {
    int valor;
    struct lligada *prox;
} *LInt;

LInt newLInt (int v, LInt t){
    LInt new = (LInt) malloc (sizeof (struct lligada));
    
    if (new!=NULL) {
        new->valor = v;
        new->prox  = t;
    }
    return new;
}

// 1.
int length (LInt l) {
    int count = 0;

    while (l != NULL) {
        count++;
        l = l->prox;
    }

    return count;
}

// 2.
void freeL (LInt l) {
    LInt list;

    while(l != NULL) {
        list = l->prox;
        free(l);
        l = list;
    }
}

// 3.
void imprimeL (LInt l) {
    while (l != NULL) {
        printf("%d\n", l->valor);
        l = l->prox;
    }
}

// 4.
LInt reverseL (LInt l) {
    LInt front, back = NULL;

    while (l) {
        front = l->prox;
        l->prox = back;
        back = l;
        l = front;
    }
    return back;
}

// 5.
void insertOrd (LInt *l, int n) {
    LInt nova;
    nova = malloc(sizeof(struct lligada));
    nova->valor = n;
    nova->prox = NULL;

    while (*l != NULL && (*l)->valor < n) {
            l = &( (*l)->prox);
    }

    nova->prox = (*l);
    *l = nova; 
}

// 6.
int removeOneOrd (LInt *l, int n) {
    int r = 1;
    
    for (; *l && (*l)->valor < n; l = &(*l)->prox);
    
    if (*l && (*l)->valor == n) {
        r = 0;
        LInt temp = *l;
        *l = (*l)->prox;
        free(temp);
    }
    
    return r;
}

// 7. +/-
void merge (LInt *r, LInt a, LInt b) {
    while (a != NULL || b != NULL) {
        if ((a != NULL && b != NULL && a->valor < b->valor) || b == NULL) {
            *r = a;
            a = a->prox;
        } else {
            *r = b;
            b = b->prox;
        }
        r = &( (*r)->prox);
    }
    *r = NULL;
}

// 8. 
void splitQS (LInt l, int x, LInt *mx, LInt *Mx) {
    while (l != NULL) {
        if (l->valor < x) {
            *mx = l;
            mx = &( (*mx)->prox);
        } else {
            *Mx = l;
            Mx = &( (*Mx)->prox) ;
        }
        l = l->prox;
    }
    *mx = NULL;
    *Mx = NULL;
}

// 9. +/-
LInt parteAmeio (LInt *l) {
    int meio = length(*l)/2;

    if (meio == 0) return NULL;

    LInt y = (*l);
    LInt prev = NULL;

    for (int i = 0; i < meio; i++) {
        prev = (*l);
        (*l) = (*l)->prox;
    }
    prev->prox = NULL;
    return y;
}

// 10. -- 
int removeAll(LInt *l, int x) {
    int count = 0;
    LInt prev = NULL;

    while (*l) {
        if ((*l)->valor == x) {
            if (!prev) (*l) = (*l)->prox;
            else prev->prox = (*l)->prox;
            count++;
        } else {
            prev = (*l);
            l = &((*l)->prox);
        }
    }
    return count;
}

// 11. --
int removeDups (LInt *l) {
    int count = 0;

    for (; *l ; l = &((*l)->prox)) {
        LInt prevAux = (*l);
        LInt aux = (*l)->prox;

        for (; aux ; aux = prevAux->prox) {
            if (aux->valor == (*l)->valor) {
                prevAux->prox = aux->prox;
                count++;
                free(aux);
            }
            else prevAux = aux;
        }
    }
    return count;
}

// 12.
int removeMaiorL (LInt *l) {
    int max = (*l)->valor;
    LInt prev = NULL;
    LInt maxPrev = NULL;
    LInt maxL = (*l);
    LInt list = (*l);
    for (;list; list = list->prox) {
        if(list->valor > max) {
            maxPrev = prev;
            maxL = list;
            max = list->valor;
        }
        prev = list;
    }
    if (!maxPrev) (*l) = (*l)->prox;
    else maxPrev->prox = maxL->prox;
    return max;    
}


// 13. 
void init (LInt *l) {
    LInt prev = NULL;
    for(;(*l)->prox;prev = *l, l = &((*l)->prox));
    if(!prev) {
        free(*l);
        *l = NULL;
    }
    else {
        prev->prox = NULL;
        free(*l);
    }    
}

// 14.
void appendL(LInt *l, int x){
	while (*l)
		l = &(*l)->prox;
    
    *l = malloc(sizeof(struct lligada));
	(*l)->valor = x;
	(*l)->prox = NULL;
}

// 15.
void concatL (LInt *a, LInt b) {
    while (*a)
        a = &(*a)->prox;
    
    (*a) = b;
}

// 16.
LInt cloneL (LInt l) {
    if (!l) return NULL; 
    LInt new = malloc(sizeof(struct lligada));
    new->valor = l->valor;
    new->prox = cloneL(l->prox);
    return new;
}

// 17.
LInt cloneRev (LInt l) {
    LInt new = NULL, list = NULL;

    for (; l; l = l->prox) {
        new = malloc(sizeof(struct lligada));
        new->valor = l->valor;
        new->prox = list;
        list = new;
    }

    return list;
}

// 18.
int maximo (LInt l) {
	int maior = l->valor;

	while (l) {
		if (l->valor > maior)
			maior = l->valor;
		l = l->prox;
	}
	return maior;
}

// 19.
int take (int n, LInt *l){
	int i = 0;
	while ( i < n && *l ) {
		l = &(*l)->prox;
		i++;
	}

	if (!(*l))
		return i;
	
	while (*l) {
		LInt temp = (*l)->prox;
		free(*l);
	    *l = temp;
	}

	return n;
}

// 20.
int drop (int n, LInt *l) {
    int i;
    for (i = 0; i < n && (*l); i++) {
        LInt temp = (*l);
        (*l) = (*l)->prox;
        free(temp);
    }
    return i;
}

// 21.
LInt NForward (LInt l, int N) {
	LInt aux = l;
	int i = 0;

	while (i != N) {
		aux = aux->prox;
		i++;
	}
	return aux;
}

// 22. 
int listToArray (LInt l, int v[], int N) {
	int i;
	for( i = 0; i < N && l; i += 1, l = l->prox )
		v[i] = l->valor;


	return i;
}

// 23.
LInt arrayToList (int v[], int N) {
    if (N == 0) return NULL;
    LInt new = malloc(sizeof(struct lligada));
    new->valor = (*v);
    new->prox = arrayToList(v + 1, N - 1);
    return new;
}

// 24.
LInt somasAcL (LInt l) {
	LInt head;
	LInt *r = &head;

	int acc = 0;

	for(; l ; r = &(*r)->prox ){
		acc += l->valor;
		*r = malloc(sizeof(struct lligada));
		(*r)->valor = acc;
		l = l->prox;
	}
	*r = NULL;

	return head;
}

// 25.
void remreps(LInt l) {
    if (l) {
        while (l->prox) {
            if (l->prox->valor == l->valor) {
                LInt temp = l->prox;
                l->prox = temp->prox;
                free(temp);
            }
            else l = l->prox;
        }
    }
}

// 26.
LInt rotateL(LInt l){
	
	if (!l || !(l->prox))
		return l;
	
	LInt aux = l;
	LInt head = l->prox;

	while (aux->prox)
		aux = aux->prox;

	aux->prox = l;
	l->prox = NULL;

	return head;
}

// 27.
LInt parte(LInt l) {
    if (!l || !l->prox) return NULL;
    LInt newL = l->prox;
    l->prox = l->prox->prox;
    newL->prox = parte(l->prox);
    return newL;
}

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} *ABin;

ABin newABin (int r, ABin e, ABin d){
	ABin new = (ABin) malloc (sizeof (struct nodo));

	if (new!=NULL){
		new->valor = r;
		new->esq   = e;
		new->dir   = d;
	}
	return new;
}

// 28. 
int altura (ABin a) {
    int e, d, r = 0;
    while (a != NULL) {
        e = 1 + altura(a->esq);
        d = 1 + altura(a->dir);
        r = d > e ? d : e;       
    }
    return r;
}

// 29.
ABin cloneAB (ABin a) {
    ABin c = NULL;

    if (a != NULL) {
        c = malloc(sizeof(struct nodo));
        c->valor = a->valor;
        c->esq = cloneAB(a->esq);
        c->dir = cloneAB(a->dir);
    }
    return c;
}

// 30.
void mirror (ABin *a) {
    if (*a) {
        ABin temp = (*a)->esq;
        (*a)->esq = (*a)->dir;
        (*a)->dir = temp;
        mirror(&(*a)->esq);
        mirror(&(*a)->dir);
    }
}

// 31.
void inorder (ABin a, LInt *l) {
	if(!a)
		*l = NULL;
	else {
		
		inorder(a->esq, l);
		
		while(*l)
			l = &(*l)->prox;

		*l = malloc(sizeof(struct nodo));
		(*l)->valor = a->valor;
		inorder(a->dir, &(*l)->prox);
	}
}

// 32.
void preorder (ABin a, LInt *l) {
    if(!a)
		*l = NULL;
    else {
        *l = malloc(sizeof(struct nodo));
        (*l)->valor = a->valor;
        preorder(a->esq, &(*l)->prox);
        while(*l)
            l = &(*l)->prox;
        preorder(a->dir, &(*l)->prox);
    }
}

// 33.
posorder (ABin a, LInt *l) {
    if (a) {
        posorder(a->esq, l);
        while (*l)
            l = &(*l)->prox;

        posorder(a->dir, l);
        while (*l)
            l = &(*l)->prox;

        *l = malloc(sizeof(struct nodo));
		(*l)->valor = a->valor;

		(*l)->prox = NULL;
    } else 
        *l = NULL;
}

// 34.
int min (int a, int b){
	int r = a;

	if (a < b)
		r = a;
	else 
		r = b;

	return r;
}

int depth(ABin a, int x) {
	int e, d;

	if (a == NULL)
		return -1;
	
	if (a->valor == x)
		return 1;

	e = depth(a->esq, x);
	d = depth(a->dir, x);

	if (e == -1 && d == -1)
		return -1;

	if (e == -1)
		return d + 1;

	if (d == -1)
		return e + 1;

	return (1 + min(e,d));
}

// 35.
int freeAB (ABin a) {
    int r = 0;

    if (a != NULL) {
        r += 1 + freeAB(a->esq) + freeAB(a->dir);
        free(a);
    }
    return r;
}

// 36.
int pruneAB (ABin *a, int l) {
    int r = 0;

    if(!(*a))
        return r;
    
    else {
        if (l == 0) {
            r += 1 + pruneAB (&(*a)->esq, l) + pruneAB (&(*a)->dir, l);
            free(*a);
            *a = NULL;
        } else {
            r += pruneAB (&(*a)->esq, l - 1);  
            r += pruneAB (&(*a)->dir, l - 1);  
        }
    } 
    return r;
}

// 37.
int iguaisAB (ABin a, ABin b) {
    int r = 0;

    if (a != NULL && b != NULL) {
        r = (a->valor == b->valor) && iguaisAB(a->dir, b->dir) &&
        iguaisAB(a->esq, b->esq);
    } else if (a == b)
        r = 1;

    return r;
}

// 38. 
LInt nivelL (ABin a, int n) {
LInt l = NULL, e, d;

  if (a != NULL) {
    if (n == 1) {
      l = malloc(sizeof(struct lligada));
      l->valor = a->valor;
      l->prox = NULL;
    } else if (n > 1) {
      e = nivelL(a->esq, n - 1);
      d = nivelL(a->dir, n - 1);
      if (e != NULL) {
        l = e;
        while (e->prox != NULL) e = e->prox;
        e->prox = d;
      } else
        l = d;
    }
  }

  return l;
}

// 39.
int nivelV (ABin a, int n, int v[]) {
    int r = 0;

    if (a != NULL) {
        if (n == 1) {
            v[0] = a->valor;
            r = 1;
        } else if (n > 1) {
            r += nivelV(a->esq, n - 1, v);
            r += nivelV(a->dir, n - 1, v + r);
        }
    }

    return r;
}

// 40.
int dumpAbin (ABin a, int v[], int N) {
    int r = 0;

    if (a != NULL && r < N) {
        r = dumpAbin(a->esq, v, N);
        if (r < N) {
            v[r++] = a->valor;
            r += dumpAbin(a->dir, v + r, N - r);
        }
    }

    return r;
}

// 41. 
ABin somasAcA (ABin a) {
    ABin r = NULL;

    if (a != NULL) {
        r = malloc(sizeof(struct nodo));
        r->valor = a->valor;

        r->esq = somasAcA(a->esq);
        if (r->esq != NULL) r->valor += r->esq->valor;

        r->dir = somasAcA(a->dir);
        if (r->dir != NULL) r->valor += r->dir->valor;
    }

    return r;
}

// 42.
int contaFolhas (ABin a) {
    int count = 0;

    if (a != NULL) {
        if(a->esq == NULL && a->dir == NULL) {
            count = 1;
        } else 
            count = contaFolhas(a->esq) + contaFolhas(a->dir);
    }
    return count;
}

// 43.
ABin cloneMirror (ABin a) {
    ABin clone = NULL;

    if (a != NULL) {
        clone = malloc(sizeof(struct nodo));
        clone->valor = a->valor;
        clone->esq = cloneMirror (a->dir);
        clone->dir = cloneMirror (a->esq);
    }

    return clone;
}

// 44.
int addOrd (ABin *a, int x) {
    int r = 0;

    while (*a != NULL && r == 0)
        if ((*a)->valor < x)
            a = &((*a)->dir);
        else if ((*a)->valor > x)
            a = &((*a)->esq);
        else
            r = 1;

    if (r == 0) {
        *a = malloc(sizeof(struct nodo));
        (*a)->valor = x;
        (*a)->esq = NULL;
        (*a)->dir = NULL;
    }

    return r;
}

// 45.
int lookupAB (ABin a, int x) {
    int r = 0;

    while (a != NULL && r == 0) 
        if (a->valor > x)
            a = a->esq;
        else if (a->valor < x)
            a = a->dir;
        else 
            r = 1;

    return r;
}

// 46.
int depthOrd (ABin a, int x) {
    int r = 1;

    while (a) {
        if (a->valor == x)
            return r;             
        else if (a->valor < x) {
            r++;
            a = a->dir;
        } else {
            r++;
            a = a->esq;
        }
    } 
    return r;
}

// 47.
int maiorAB (ABin a) {
        while (a->dir != NULL){
            a = a->dir;
        }

    return a->valor;
}

// 48. 
void removeMaiorA (ABin *a) {
    if ( (*a)->dir == NULL) {
        ABin temp = (*a);
        free(*a);
        (*a) = temp->esq;
    } else {
        removeMaiorA( &(*a)->dir );
    }
}

// 49.
int quantosMaiores (ABin a, int x) {
    int r = 0;

    if (a != NULL) {
        if (a->valor < x) 
            r = 1 + quantosMaiores(a->esq, x) + quantosMaiores(a->dir, x);
        else 
            r = quantosMaiores(a->esq, x);
    }
    return r;
}

// 50.
void listToBTree(LInt l, ABin *a) {
    LInt menores = parteAmeio(&l);
    *a = NULL;

    if (l != NULL) {
        *a = malloc(sizeof(struct nodo));
        (*a)->valor = l->valor;
        l = l->prox;
        listToBTree(menores, &((*a)->esq));
        listToBTree(l, &((*a)->dir));
    }
} 

// 51.
int bigger(ABin a, int x) {
    int r = 1;

    if (a != NULL) {
        if (a->valor > x) {
            r = bigger(a->esq, x) && bigger(a->dir, x);
    } else
        r = 0;
    }

    return r;
}

int smaller(ABin a, int x) {
    int r = 1;

    if (a != NULL) {
        if (a->valor <= x) {
            r = smaller(a->esq, x) && smaller(a->dir, x);
    } else
        r = 0;
    }

    return r;
}

int deProcura(ABin a) {
    int r = 1;

    if (a != NULL) {
        r = smaller(a->esq, a->valor) && bigger(a->dir, a->valor);
    if (r == 1) {
        r = deProcura(a->esq) && deProcura(a->dir);
    } else
        r = 0;
    }

     return r;
}