#include <stdio.h>
#include <string.h>
#include <ctype.h>

// 1 Funcoes sobre strings
// 1.
int evogal (char c) {
    switch (tolower(c)) {
        case 'a':
        case 'e':
        case 'i':
        case 'o':
        case 'u':
            return 1;
        default:
        return 0;   
    }
}

int contaVogais (char *s) {
    int nVogais = 0; 
    int pos = 0; 

    while (s[pos] != '\0') {
        if (evogal(s[pos])){
            nVogais++;
        }
    pos++;
    }  
    return nVogais;
}

// 2.
int retiraVogaisRep1 (char *s) {
    int count = 0; 
    int comprimento = strlen(s);
    char aux[comprimento + 1];
    aux[comprimento] = '\0';
    int pos = 1;
    int posAux = 1;
    
    aux[0] = s[0];

    while (s[pos] != '\0') {
        if (!evogal(s[pos]) || s[pos] != s[pos-1]){
            aux[posAux] = s[pos];
            posAux++;
        } else {
            count++;
        }
        pos++;
    }
    int i = 0; 
    while (aux[i] != '\0'){
        s[i]= aux[i];
        i++;
    }
    s[i]= '\0';

    return count;
}

int retiraVogaisRep2 (char *s) {
    char *n = s;
    char *p = n + 1;

    while (*p != ' \0') {
        if (!(evogal(*n) && evogal(*p) && *p == * n)){
            n++;
            *n = *p;
        }
        p++;
    }
    *(n+1) = '\0';

    return p - n - 1;
}

int main () {

    
}