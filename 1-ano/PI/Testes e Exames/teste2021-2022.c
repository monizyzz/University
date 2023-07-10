#include <stdio.h>

#include "tipos.h"

/*
Questao 1:
Implemente a função int nesimo(int a[], int N, int i) 
que dado um array de tamanho N > 0 e um inteiro 
0 < i <= N devolve o i-ésimo menor elemento do array. 
Por exemplo, se i == 1 a função deve retornar o menor 
elemento do array.
*/

int nesimo(int a[], int N, int i) {
    for (int b = 0; b < N; b++) {
        for (int j = b + 1; j < N; j++) {
            if (a[b] > a[j]) {
                int temp = a[j];
                a[j] = a[b];
                a[b] = temp;
            }
        }
    }
    return a[i - 1];
}

/*
Questão 2:
Implemente a função LInt removeMaiores(LInt l, int x) 
que remove de uma lista ordenada l todos os elementos 
maiores que x, devolvendo a lista resultante. 
Considere a definição usual do tipo LInt.
*/

LInt removeMaiores(LInt l, int x) {
    if (l == NULL)
        return NULL;

    // Remover elementos no início da lista
    while (l != NULL && l->valor > x) {
        LInt aux = l;
        l = l->prox;
        free(aux);
    }

    // Remover elementos no meio ou no final da lista
    LInt current = l;
    while (current != NULL && current->prox != NULL) {
        if (current->prox->valor > x) {
            LInt aux = current->prox;
            current->prox = aux->prox;
            free(aux);
        } else {
            current = current->prox;
        }
    }

    return l;
}

/*
Questão 3:
Implemente a função LInt caminho(ABin a, int x) que, 
dada uma árvore binária de procura a e um valor x, 
devolve uma lista com todos os valores desde a raiz 
até x (inclusivé). 
Se x não existir na árvore, deve devolver NULL. 
*/

LInt caminho(ABin a, int x){
    LInt l = NULL;
    if(a == NULL){
        return NULL;
    }
    if(a->valor == x){
        l = malloc(sizeof(struct LInt_nodo));
        l->valor = x;
        l->prox = NULL;
        return l;
    }
    if(a->valor > x){
        l = caminho(a->esq, x);
    }
    else{
        l = caminho(a->dir, x);
    }
    if(l != NULL){
        LInt aux = malloc(sizeof(struct LInt_nodo));
        aux->valor = a->valor;
        aux->prox = l;
        l = aux;
    }
    return l;
}

/*
Questão 4:
Implemente a função void inc(char s[]) que, dada uma 
string s com um número em decimal, incrementa esse 
número numa unidade. 
Assuma que a string tem espaço suficiente para 
armazenar o número resultante. 
Por exemplo, se a string for "123" deverá ser 
modificada para "124". 
Se for "199" deverá ser modificada para "200".
*/
void inc(char s[]) {
    int i = 0;
    int carry = 1; // carry é inicializado como 1 para incrementar o último dígito

    // Percorre a string da direita para a esquerda
    for (i = strlen(s) - 1; i >= 0; i--) {
        int digit = s[i] - '0'; // Converte o caractere para um dígito inteiro
        int sum = digit + carry;

        // Verifica se há carry
        if (sum >= 10) {
            carry = 1;
            sum -= 10;
        } else {
            carry = 0;
        }

        // Atualiza o dígito na string
        s[i] = sum + '0';
    }

    // Se ainda houver carry após o loop, adiciona um dígito '1' no início da string
    if (carry == 1) {
        memmove(s + 1, s, strlen(s) + 1); // Move os caracteres para a direita
        s[0] = '1';
    }
}

/*
Questão 5:
Implemente a função 
int sacos(int p[], int N, int C) que, 
dado um array com os pesos de N produtos 
que se pretende comprar num supermercado, 
e a capacidade C dos sacos desse supermercado,  
determine o número mínimo de sacos necessários 
para transportar todos os produtos. 
Por exemplo, se os pesos dos produtos forem 
{3,6,2,1,5,7,2,4,1} e C == 10, então bastam 4 sacos. 
Se os pesos forem {3,3,3,3,5,5,11} e C == 11, então 
bastam 3 sacos. 
Em geral, para descobrir este mínimo teria que 
testar todas as possíveis maneiras de ensacar os 
produtos. Se não conseguir implementar essa estratégia 
óptima, implemente outra que devolva uma aproximação 
razoável.
*/

int sacos(int p[], int N, int C) {
    int soma = 0;
    int sacos = 0;
    for (int i = 0; i < N; i++) {
        for (int j = i + 1; j < N; j++) {
            if (p[i] > p[j]) {
                int temp = p[i];
                p[i] = p[j];
                p[j] = temp;
            }
        }
    }

    for(int i = 0; i < N; i++) {
        soma += p[i] + p[i + 1];
        if(soma <= C) {
            p[i] = 0;
            p[i + 1] = 0;
        }
        else{
            soma = 0;
            i--;
            sacos++;
        }
    }

    return sacos;

}

int main (){

	//testes para a questão 1
	printf ("\nQuestao 1\n----------\n");

	int q1 [20] = {11,12,3,17,19,15,18,2,7,1,20,8,14,13,16,6,5,9,10,4};

	printf ("i=1: %d\n", nesimo_sol(q1,20,1));
	printf ("i=10: %d\n", nesimo_sol(q1,20,10));
	printf ("i=20: %d\n", nesimo_sol(q1,20,20));

	// testes para a questão 2: nesta altura q1 está ordenado!
	printf ("\nQuestao 2\n----------\n");

	LInt q2 = LfromArray (q1,20);

	dumpL (q2);
	q2 = removeMaiores_sol (q2,15);
	printf ("removeMaiores 15: "); dumpL (q2);
	q2 = removeMaiores_sol (q2,5);
	printf ("removeMaiores 5: "); dumpL (q2);
	q2 = removeMaiores_sol (q2,-5);
	printf ("removeMaiores -5: "); dumpL (q2);

    // testes para a questão 3: q1 ainda está ordenado, q2 está a NULL
	printf ("\nQuestao 3\n----------\n");

    ABin q3 = AfromArray (q1,20);

    q2 = caminho_sol (q3,10); 
    printf ("caminho até 10: "); dumpL (q2);
    freeLInt (q2);
    q2 = caminho_sol (q3,100); 
    printf ("caminho até 100: "); dumpL (q2);
    freeLInt (q2);

    // testes para a questão 4
	printf ("\nQuestao 4\n----------\n");

	char s1[10] = "12345678",
	     s2[10] = "12345999",
	     s3[10] = "99999999";

	printf ("%s %s %s\n", s1, s2, s3);
	inc_sol (s1); inc_sol (s2); inc_sol (s3);
	printf ("%s %s %s\n", s1, s2, s3);


    // testes para a questão 5
	printf ("\nQuestao 5\n----------\n");

	int ex1 [ 9] = {3,6,2,1,5,7,2,4,1},   N1 =  9, C1 = 10,
	    ex2 [ 7] = {3,3,3,3,5,5,11},      N2 =  7, C2 = 11,
	    ex3 [10] = {6,6,6,6,6,6,6,6,6,6}, N3 = 10, C3 = 10;

	printf ("ex1 = %d\n", sacos_sol (ex1,N1,C1));
	printf ("ex2 = %d\n", sacos_sol (ex2,N2,C2));
	printf ("ex3 = %d\n", sacos_sol (ex3,N3,C3));


	return 0;
}