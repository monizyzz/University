#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// 1. 
float multInt1 (int n, float m) {

    int i;
    float s1 = 0;

    for (i = 0; i < n; i++)
        s1 = s1 + m;
    return s1;
} 

// 2.
float multInt2 (int n, float m) {
    
    int op = 0;
    float s2 = 0;
    
    while(n != 1) {

        if (n % 2 == 1) {
            op += m;
        }
        
        n /= 2;
        m *= 2;
    }
    return m + s2;
}

// 3. e 4. 
int mdc1 (int a, int b) {

while (a != 0 && b != 0)
    if (a > b) a = a - b;
    else b = b - a;

    return (a+b);
}

// 5.
int mdc2 (int a, int b){
    int x, r;

    if (a<b) x = a;else x = b;

    while (x > 0){
        if (a % x == 0 && b % x == 0)r = x;
        x--;
    }
    return r;
} 

// 6. 
// (a)
int fib1 (int n){
    int r;

    if (n > 2){
        r = fib1(n - 1) + fib1(n - 2);
    } else {
        r = 1;
        return r;
    }
    return r;
}

// (b)
int fib2 (int n){
    if (n == 0) return n;

    int acc1, acc2;
    acc1 = acc2 = 1;
   
    int i;
    for(i = 3; i <= n; i++){
        acc2 += acc1;
        acc1 = acc2 - acc1;
    } 
    return acc2;
}


int main () {
    //float s1;
    //s1 = multInt1(14,2.3);
    ////("%f\n",s1);

    //float s2,
    //s2 = multInt1(81,423.0);
    //printf("%f\n",s2);

    //int s3e4;
    //s3e4 = mdc1 (28,63);
    //printf ("%d\n",s3e4);

    //int s5;
    //s5 = mdc2 (28,63);
    //printf ("%d\n",s5);
    
    //int s6a;
    //s6a = fib1(5);
    //printf("%d\n", s6a);
    
    //int s6b;
    //s6b = fb2(5);
    //printf("%d\n", s6b);
    //assert(5 == fib2(5));
    
    return 0;
}