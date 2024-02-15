#include <stdio.h>

int main() {
    int N;
    if(scanf("%d", &N) != 1) return 1;

    while (N--) {
        int K, I;
        int prox[101];

        if(scanf("%d", &K) != 1) return 1;
        if(scanf("%d", &I )!= 1) return 1;
        for (int j = 1; j <= K; j++) {
            if(scanf("%d", &prox[j]) != 1)return 1;
        }

        int vistos[101] = {0};
        printf("%d", I);
        vistos[I] = 1;
        I = prox[I];

        while (I != 0) {
            if (vistos[I]) {
                printf(" CICLO INFERNAL\n");
                break;
            }
            printf(" %d", I);
            vistos[I] = 1;
            I = prox[I];
        }

        if (I == 0) {
            printf("\n");
        }
    }

    return 0;
}