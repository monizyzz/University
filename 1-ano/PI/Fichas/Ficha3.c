#include <stdio.h>

// 2.
void swapM (int *a, int *b) {
    int tmp = (*b);
    (*b) = (*a);
    (*a) = tmp;
}

// 3. 
void swap (int v[], int i, int j) {
    int tmp = v[j];
    v[j] = v[i];
    v[i] = tmp;
}

// 4. 
int soma (int v[], int N) {
    int r = 0;

    for(int i = 0; i < N; i++){
        r = r + v[i];
    }
    return r;
}

// 5. 
void inverteArray (int v[], int N) {
    for (int i = 0; i < N; i++){
        swap (v, i, N - 1);
            N--;
    }
}
void inverteArrayM (int v[], int N) {
    int stop = N / 2;
    int i;

/*   for (int i = 0; i < N; i++){
        swapM (v, i, N - 1){
           
        }
    }
    */
}

// 6. 
int maximum (int v[], int N, int *m) {



}




int main () {
//int x = 3, y = 5;
//swapM (&x, &y);
//printf ("%d %d\n", x, y);

//int N = 5;
//int v[] = {1,2,3,4,5};
//int r = soma (v,N);
//printf("%d\n", r);


}