#include <stdio.h>
#include <stdlib.h>

int main () {
    int N;
    int vi = 0;
    int vc = 0;
    int e = 0;
    char a, b;

    if(scanf("%d",&N) != 1) return 1;

    for(int i = 0; i < N; i++){
        if(scanf("\n%c",&a) != 1) {
            return 1;
        }
        if(scanf("%c",&b) != 1) {
            return 1;
        }
        switch (a) {
            case '@':
                switch (b) {
                    case '*':
                        e++;
                        break;
                    case '-':
                        vc++;
                        break;
                    case '+':
                        vi++;
                        break;
                }   
                break;
            case '|':
                switch (b) {
                    case '*':
                        vi++;
                        break;
                    case '-':
                        e++;
                        break;
                    case '+':
                        vc++;
                        break;
                }
                break;
            case 'X':
                switch(b) {
                    case '*':
                        vc++;
                        break;
                    case '-':
                        vi++;
                        break;
                    case '+':
                        e++;
                        break;
                }
                break;
        }
    }
    printf("%d %d %d\n", vi, vc, e);
    return 0;
}