#include <stdio.h>
#include <math.h>
// 3.
// 3.1.

void quadrado () {
    int y; 
    int z = 0;
    int altura = 0;

    printf ("Coloque a dimensão do quadrado: ");
    scanf ("%d",&y);
    // y = 5
    while (z<=y) {
        printf("#");
        z += 1;
        if (z==y) {
            printf("\n");
            z = 0;
            altura += 1;
        }
        if (altura==y){
            break;
        }
    } 
}

// 3.2.

void xadrez1 () {
    int i;
    int z = 0; 
    int altura = 0;

    printf("Coloque a dimensão do tabuleiro: ");
    scanf("%d",&i);
    // y = 5
    while (z<=i){
        if (i%2==0){
            putchar('#');
            }
            else putchar ('_');
            z += 1;
        if (z==i){
            printf("\n");
            z = 0;
            altura += 1;
        }
        if (altura==i){
            break;
        }
    }
}

void xadrez2 () {
int par;
int size;
int linhas = 0; 
int colunas = 0;

printf("Coloque a dimensão do tabuleiro: ");
scanf("%d", &size);

while (linhas<size){
    colunas = 0;
    while (colunas < size){
            par = linhas + colunas;
            if (par%2==0) {
                putchar ('#');
            }
            else putchar ('_');
            colunas += 1;
            }
    printf ("\n");
    linhas += 1;
    }
}

// 3.3.

void triangulo1 () {
int altura = 0;
int comp = 0;
int i;

printf("Insire a altura da pirâmide: ");
scanf("%d",&i);

    while (altura<=i-1) {
        comp = 0;
        while (comp <(altura + 1)) {
            putchar ('#'); 
            ++comp; 
        }
        printf("\n");
        ++altura;
    }
    while (altura>=0) {
        comp = 1;
        while (comp <(altura)) {
            putchar ('#'); 
            ++comp; 
        }
        printf("\n");
        --altura;
    }
}
// and 

void triangulo2 () {
int altura;

printf("Insere a altura do triangulo: ");
scanf("%d",&altura);

    for (int i = 0; i < altura; i++) {
        for (int vanessa = 0; vanessa < altura - i - 1; vanessa++) {
            putchar(' ');
        }
        for(int j = 0; j < 2 * i + 1 ; j++){
            putchar ('#');
        }
        printf("\n");
    }
}

void circulo (int r) {
    int i,j, contador = 0, diametro = 2*r + 1;
    // matrix lines
    for (i = 0; i < 2*r + 1 ; i++) {
        contador++;
        if (contador == r + 1) {
            for (j = 0; j < 2 * r + 1;j++) {
                putchar('#');
            }
        } else {
            for (j = 0; j<= 2 * r + 1;j++) {
                double distance = sqrt((double)(i-r)*(i-r) + (j-r)*(j-r));
                if (distance>r-0.5 && distance<r+0.5) {
                    printf("#");
                } else { 
                    printf(" ");
                }
            }
        }
        putchar('\n');
    }
}

int main(){
    //quadrado();
    //xadrez1();
    //xadrez2();
    //triangulo1();
    //triangulo2();
    circulo(4);
    return 0;
}