#include <stdio.h>
#include <stdlib.h>

int main () {
    int altura1, altura2, altura3;

    if(scanf("%d",&altura1) != 1) abort();
    if(scanf("%d",&altura2) != 1) abort();
    if(scanf("%d",&altura3) != 1) abort();

    if (altura1<=altura2 && altura2<=altura3) {
        printf("OK\n");
    } else if (altura1 >= altura2 && altura2>=altura3)
        printf("OK\n");
        else
        printf("NAO\n");
    return 0;
}