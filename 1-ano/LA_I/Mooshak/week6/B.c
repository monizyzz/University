#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_COZINHEIROS 100

struct cozinheiro {
    char alcunha[50];
    int peso;
    int altura;
};

// Função para comparar dois cozinheiros com base no peso ideal do empregador, peso real do cozinheiro, altura e alcunha em caso de empate
int comparar(const void *a, const void *b) {
    struct cozinheiro *coz1 = (struct cozinheiro*)a;
    struct cozinheiro *coz2 = (struct cozinheiro*)b;

    int peso_ideal = 90;

    // Calcula a diferença entre o peso real do cozinheiro e o peso ideal do empregador
    int dif_peso1 = abs(coz1->peso - peso_ideal);
    int dif_peso2 = abs(coz2->peso - peso_ideal);

    // Se as diferenças nos pesos forem diferentes, classifique pelo peso mais próximo do ideal
    if (dif_peso1 != dif_peso2) {
        return dif_peso1 - dif_peso2;
    }
    // Se os pesos forem iguais, classifique pela altura
    else if (coz1->altura != coz2->altura) {
        return coz2->altura - coz1->altura;
    }
    // Caso contrário, classifique por ordem alfabética
    else {
        return strcmp(coz1->alcunha, coz2->alcunha);
    }
}

int main() {
    int num_cozinheiros;
    if(scanf("%d", &num_cozinheiros) != 1) return 1;

    // Define um array de estruturas de tamanho fixo
    struct cozinheiro cozinheiros[MAX_COZINHEIROS];

    // Lê os dados dos cozinheiros
    for (int i = 0; i < num_cozinheiros; i++) {
        if(scanf("%s %d %d", cozinheiros[i].alcunha, &cozinheiros[i].peso, &cozinheiros[i].altura) != 3) return 1;
    }

    // Ordena os cozinheiros com base em peso e altura
    qsort(cozinheiros, num_cozinheiros, sizeof(struct cozinheiro), comparar);

    // Imprime os cozinheiros ordenados
    for (int i = 0; i < num_cozinheiros; i++) {
        printf("%s %d %d\n", cozinheiros[i].alcunha, cozinheiros[i].peso, cozinheiros[i].altura);
    }

    return 0;
}