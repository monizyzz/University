#include <stdio.h>
#include <stdlib.h>

int main () {
    int n;
    int carta;
    int x = 0;
    int y = 0;

    if(scanf("%d\n",&n) != 1){ 
        return 1;
    }

    for (int i = 0; i < n; i++) {
        if (scanf("%d",&carta) != 1 && carta <= 78 && carta > 0) {
            return 1;
        }
    
        switch (carta%4) {
        case 0:
            x = x + 1;
            break;

        case 1:
            y = y - 1;
            break;

        case 2:
            y = y + 1;
            break;

        case 3:
            x = x - 1;
            break;
        default:
            break;
        }
    }
    printf("%d %d\n", x, y);
    return 0;
}