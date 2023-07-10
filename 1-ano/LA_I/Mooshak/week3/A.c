#include <stdio.h>
#include <string.h>

int main () {
    char n[9];
    char msg[1001];

    if (scanf("%s", n) != 1) return 1;
    if (scanf("%s", msg) != 1) return 1;

    int com = strlen(n);
    int size = strlen(msg);
  
    for (int i = 0; i < com; i++) {
        int pos;
        pos = n[i] - '1'; // Obter a posição "pos" ao subtrair o caractere atual "n" pelo caractere '1', tem haver com a tabela ASCII

        for (int l = 0; l + pos < size; l += com) {
            printf("%c", msg[l + pos]);
        }
    }
    printf("\n");
    return 0;
}