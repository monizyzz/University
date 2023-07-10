#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main () {
    char comando[21];
    int N;
    int r = 5;
    
    if(scanf("%d",&N) != 1)return 1;

    int final[N];
    /*
    7 8 9
    4 5 6
    1 2 3
    */
    for (int i = 0; i < N; i++) {
        if(scanf("%s", comando) != 1) return 1;

        for (size_t j = 0; j < strlen(comando); j++) {
            switch (comando[j]) {
                case 'C':
                    if (r != 7 && r != 8 && r != 9) r += 3;
                    break;
                case 'B':
                    if (r != 1 && r != 2 && r != 3) r -= 3;
                    break;
                case 'D':
                    if (r != 3 && r != 6 && r != 9) r += 1;
                    break;
                case 'E':
                    if (r != 1 && r != 4 && r != 7) r -= 1;
                    break;
                default:
                    break;
            }
        }
        final[i] = r;
    }
    for (int i = 0; i < N; i++) {
        printf("%d", final[i]);
    }
    printf("\n");
    return 0;
}