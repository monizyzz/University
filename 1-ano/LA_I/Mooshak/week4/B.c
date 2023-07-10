#include <stdio.h>
#include <string.h>

int main () {
    int N; 
    char texto[1001];
    
    if(scanf("%d", &N) != 1)return 1;
    getchar(); // tirar o '\n' do buffer
    char final[N+1];

    for(int i = 0; i < N; i++) {
        if(fgets(texto, 1001, stdin) == 0)return 1;

        int len = strlen(texto);

        for (int k = 0; k < len; k++) {
            for (int m = k + 1; m < len; m++)
                if (texto[k] == texto[m]) {
                    final[i] = texto[k];
                }
        }
    }

    final[N] = 0; // adiciona um caracter nulo ('\0') à última posição
    printf ("%s\n", final);

    return 0;
}