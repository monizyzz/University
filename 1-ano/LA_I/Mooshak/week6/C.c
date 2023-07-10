#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PEDIDOS 100

struct pedido {
    int numero;
    char dificuldade;
    int preco;
};

// Função para comparar dois pedidos com base em dificuldade e preço
int comparar(const void *a, const void *b) {
    struct pedido *ped1 = (struct pedido*)a;
    struct pedido *ped2 = (struct pedido*)b;

    // Se os preços forem diferentes, classifique por preço (ordem decrescente)
    if (ped1->preco != ped2->preco) {
        return ped2->preco - ped1->preco;
    }
    // Se as dificuldades forem diferentes, classifique por dificuldade (A é o mais fácil e E o mais difícil)
    else if (ped1->dificuldade != ped2->dificuldade) {
        return ped1->dificuldade - ped2->dificuldade;
    }
    // Caso contrário, classifique por número de pedido (ordem crescente)
    else {
        return ped1->numero - ped2->numero;
    }
}

int main() {
    int num_pedidos;
    if(scanf("%d", &num_pedidos) != 1) return 1;

    // Define um array de estruturas de tamanho fixo
    struct pedido pedidos[MAX_PEDIDOS];

    // Lê os dados dos pedidos
    for (int i = 0; i < num_pedidos; i++) {
        if(scanf("%d %c %d", &pedidos[i].numero, &pedidos[i].dificuldade, &pedidos[i].preco) != 3) return 1;
    }

    // Ordena os pedidos com base em dificuldade e preço
    qsort(pedidos, num_pedidos, sizeof(struct pedido), comparar);

    // Imprime os pedidos ordenados
    for (int i = 0; i < num_pedidos; i++) {
        printf("%d %c %d\n", pedidos[i].numero, pedidos[i].dificuldade, pedidos[i].preco);
    }

    return 0;
}