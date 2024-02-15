#include <stdio.h>
#include <stdlib.h>

int main () {
    int C, A, N, b;

    if(scanf("%d",&C) != 1) return 1;
    if(scanf("%d",&A) != 1) return 1;
    if(scanf("%d",&N) != 1) return 1;

    for(int i = 0; i < N; i++){
        if(scanf("%d",&b) != 1) {
            return 1;
        }
        if ((C<A && C>0) || (C==A && b==-1) || (C==0 && b==1)){
            switch (b){
            case 1:
                C = C + 1;
                break;
            case (-1):
                C = C - 1;
                break;
            default:
                break;
            }
        }
    }
    printf("%d\n", C);
    return 0;
}