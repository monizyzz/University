#include <stdio.h>
#include <stdlib.h>

int main () {
    int N, resto, pr, max, v;

    if(scanf("%d",&N) != 1)return 1;
    int final [N];

    for (int i = 0; i < N; i++) {
        if(scanf("%d", &pr) != 1)return 1;

        max = 0;
        v = 0;

        for (int l = 0; l < pr; l++){
            if(scanf("%d", &resto) != 1)return 1;

            if (resto > max) {
                v++;
                max = resto;
            }
        }

        final[i] = v;

    }
    for (int i = 0; i < N; i++) {
        printf("%d\n", final[i]);
    }
    return 0;
}