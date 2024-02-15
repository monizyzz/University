#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX_CANDIDATOS 100
#define MAX_NOME 100

int main() {
    int N;
    if(scanf("%d", &N) != 1) return 1;

    while (N--) {
        int n_candidatos;
        char vilao[MAX_NOME + 1];
        if(scanf("%d\n", &n_candidatos) != 1) return 1;
        if(fgets(vilao, MAX_NOME + 1, stdin) == NULL) return 1;

        // Remove o caracter de nova linha do nome do vilão
        vilao[strcspn(vilao, "\n")] = '\0';

        int indices[MAX_CANDIDATOS], n_indices = 0;

        for (int i = 1; i <= n_candidatos; i++) {
            char candidato[MAX_NOME + 1];
            if(fgets(candidato, MAX_NOME + 1, stdin) == NULL) return 1;

            // Remove o caractere de nova linha do nome do candidato
            candidato[strcspn(candidato, "\n")] = '\0';

            int tam_vilao = strlen(vilao), tam_candidato = strlen(candidato);
            int freq_vilao[26] = {0}, freq_candidato[26] = {0};

            // Conta as frequências de cada letra no nome do vilão
            for (int j = 0; j < tam_vilao; j++) {
                freq_vilao[tolower(vilao[j]) - 'a']++;
            }

            // Conta as frequências de cada letra no nome do candidato
            for (int j = 0; j < tam_candidato; j++) {
                freq_candidato[tolower(candidato[j]) - 'a']++;
            }

            // Verifica se o nome do candidato é válido
            int valido = 1;
            for (int j = 0; j < 26 && valido; j++) {
                if (freq_vilao[j] > freq_candidato[j]) {
                    valido = 0;
                }
            }

            if (valido) {
                indices[n_indices++] = i;
            }
        }

        if (n_indices == 0) {
            printf("-1\n");
        } else {
            printf("%d", indices[0]);
            for (int i = 1; i < n_indices; i++) {
                printf(" %d", indices[i]);
            }
            printf("\n");
        }
    }

    return 0;
}