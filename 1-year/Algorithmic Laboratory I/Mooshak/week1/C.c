#include <stdio.h>
#include <stdlib.h>

int main () {
    int altura1, altura2, altura3;

    if(scanf("%d",&altura1) != 1) abort();
    if(scanf("%d",&altura2) != 1) abort();
    if(scanf("%d",&altura3) != 1) abort();

    if (altura1 <= altura2 && altura1 <= altura3) {
        printf("%d ", altura1);

        if (altura2 <= altura3){
            printf("%d %d\n",altura2, altura3);
        } else {
            printf("%d %d\n",altura3, altura2);
        }
    } else if (altura2 < altura1 && altura2 < altura3){
        printf("%d ",altura2);

        if (altura1 <= altura3) {
            printf("%d %d\n",altura1, altura3);
        } else {
            printf("%d %d\n",altura3, altura1);
        }
    } else {
        printf("%d ", altura3);
        if (altura1 < altura2) {
            printf("%d %d\n", altura1, altura2);
        } else {
            printf("%d %d\n", altura2, altura1);
        }
    }
    return 0;
}