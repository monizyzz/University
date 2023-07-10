typedef struct LInt_nodo {
	int valor;
	struct LInt_nodo *prox;
} *LInt;

LInt newLInt (int x, LInt xs);

LInt LfromArray(int v[], int N);

void dumpL (LInt l);

void freeLInt (LInt l);

typedef struct ABin_nodo {
	int valor;
	struct ABin_nodo *esq, *dir;
} *ABin;

ABin AfromArray(int v[], int N);

void freeABin (ABin a);

int nesimo(int a[], int N, int i);

LInt removeMaiores (LInt l, int x);

LInt caminho (ABin a, int x);

void inc(char s[]) ;

int sacos (int p[], int N, int C);

int nesimo_sol(int a[], int N, int i);

LInt removeMaiores_sol (LInt l, int x);

LInt caminho_sol (ABin a, int x);

void inc_sol (char s[]) ;

int sacos_sol (int p[], int N, int C);