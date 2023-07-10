#include <stdio.h>
#include <ctype.h>
#include <string.h>

// retorna 1 se é vogal e 0 se é consoante
int eVogal(char letra) {
    if (letra == '\n') {
        return 2;
    }
    char vogais[] = "AEIOUY";
    for(int j = 0; vogais[j] != '\0'; j++) {
        if (letra == vogais[j]) {
            return 1;
        }
    }
    return 0;
}

int main() {
    int N;
    if (scanf("%d", &N) != 1) return 1;
    getchar(); // tirar o '\n' do buffer

    int resultado[N];
    for(int i = 0; i < N; i++) {
        char texto[1001];

        if (fgets(texto, 1001, stdin) == NULL){ // lê até encontrar '\n'
            return 1;
        }
        int acc = 0;
        char *word = strtok(texto, " "); // quebra a linha de texto em palavras (separadas por espaços) usando a função strtok() e retorna um ponteiro
        while (word != NULL) { 
            int len = strlen(word); 
            int anterior = eVogal(word[0]);
            int atual;
            int alternada = 1;

            for(int j = 1; j < len; j++) {
                atual = eVogal(word[j]);
                if(anterior == atual) {
                    alternada = 0;
                    break;
                }
                anterior = atual;
            }
            if (alternada == 1) {
                acc++;
            }
            word = strtok(NULL, " "); // obtém a próxima palavra na linha de texto
        }
        resultado[i] = acc;
    }
    for (int i = 0; i < N; i++) {
        printf("%d\n", resultado[i]);
    }
    return 0;
}