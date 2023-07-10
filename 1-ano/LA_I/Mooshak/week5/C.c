#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PESSOAS 40
#define MAX_LINHA 1000
#define MAX_NOME 20

int main() {
    int linhas = 0;
    if (scanf("%d", &linhas) != 1) return 1;
    getchar();

    char nomes[MAX_PESSOAS][MAX_NOME] = {0};
    int acc[MAX_PESSOAS] = {0};
    int n_pessoas = 0;

    for (int i = 0; i < linhas; i++) {
        char linha[MAX_LINHA];
        if (fgets(linha, MAX_LINHA, stdin) == NULL) {
            return 1;
        }

        char *token = strtok(linha, " \n"); // strtok(), divide a string em tokens, retorna um ponteiro para o próximo token na string, ou NULL se não houver mais tokens
        while (token != NULL) {
            int encontrado = 0;

            for (int j = 0; j < n_pessoas; j++) {
                if (strcmp(token, nomes[j]) == 0) { // comparar strings
                    acc[j]++;
                    encontrado = 1;
                    break;
                }
            }

            if (!encontrado && n_pessoas < MAX_PESSOAS) { // 
                strcpy(nomes[n_pessoas], token); // adicionar a string token ao vetor nomes
                acc[n_pessoas] = 1;
                n_pessoas++;
            }

            token = strtok(NULL, " \n");
        }
    }

    int menor_idx = 0;
    for (int i = 1; i < n_pessoas; i++) { // encontra a pessoa com o menor número de ocorrências
        if (acc[i] < acc[menor_idx]) {
            menor_idx = i;
        }
    }

    printf("%s\n", nomes[menor_idx]);

    return 0;
}