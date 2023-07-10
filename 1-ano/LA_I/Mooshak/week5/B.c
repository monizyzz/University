#include <stdio.h>

int main() {
    int A, N, i, j, k, P, C;
    if(scanf("%d", &A) != 1) return 1;
    if(scanf("%d", &N) != 1) return 1;

    int presente[24][A+1]; // matriz que armazena presença dos agentes em cada hora
    for (i = 0; i < 24; i++)
        for (j = 0; j <= A; j++)
            presente[i][j] = 0;

    for (i = 1; i <= N; i++) {
        if(scanf("%d %d %d", &k, &P, &C) != 3) return 1;
        for (j = P; j <= C; j++)
            presente[j][k] = 1; // marca a presença na hora j do agente k 
    }

    for (i = 0; i < 24; i++) { // percorre as horas do dia
        int count = 0;
        for (j = 1; j <= A; j++)
            if (presente[i][j]) 
                count++; // conta o número de agentes presentes na hora i

        if (count > 1) { // se há mais de um agente presente na hora i
            printf("%d", i);
            for (j = 1; j <= A; j++)
                if (presente[i][j])
                    printf(" %d", j); // imprime os códigos dos agentes presentes na hora i
            printf("\n");
        }
    }

    return 0;
}