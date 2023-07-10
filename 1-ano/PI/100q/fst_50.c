#include <stdio.h>
#include <string.h>
#include <ctype.h>

// 1. 
int ex1 () {
    int n = 1, maior = 0;

    printf("Indique os números que quiser: \n");
    while(n > 0) {
        scanf("%d", &n);
        if(maior <= n)maior = n;
    }
    return maior;
}

// 2.
int ex2 () {
    int n = 1, i = 0;
    float media, acc = 0;

    printf("Indique os números que quiser: \n"); 
    while(n > 0){   
        scanf("%d", &n);

        if (n == 0)
            break;

        acc += n;    
        i++;
    }
    media = acc / i;

    return media;
}

// 3.
int ex3 () {
    int n = 1, fst = 0, snd = 0;

    printf("Indique os números que quiser: \n");
    while (n > 0){
        scanf("%d", &n);

        if (n > fst) {
            snd = fst;
            fst = n; 
        } else if (n > snd) 
            snd = n;
            
    }
    printf("Segundo maior: %d\n", snd);
}

// 4. 
int bistUm (unsigned int n) {
    int r = 0;

    printf("Insere um número: \n");
    scanf("%d", &n);
    while(n > 0){
        if(n % 2 == 1)
            r++;
        
        n /= 2;
    }
    printf("Números de 1's: %d\n", r);
}

// 5.
int trailingZ (unsigned int n) {
    int r = 0; 

    printf("Insere um número: \n");
    scanf("%d", &n);

    while(n > 0) {
        if(n % 2 == 0)
            r++;

        n /= 2;
    }
    printf("Números de 0's: %d\n", r);
}

// 6. 
int qDig (unsigned int n) {
    int r = 0;

    printf("Insere um número: \n");
    scanf("%d",&n);

    while(n > 0) {
        r++;
        n /= 10;
    }
    printf("Número de dígitos: %d\n", r);
}

// 7. 
char *mystrcat (char s1[], char s2[]){
    char strgcat[strlen(s1) + strlen(s2)];
    int i, j;

    for(i = 0; i < strlen(s1); i++) {
        strgcat[i] =  s1[i];
    }

    for(j = 0; j < strlen(s2); j++){
        strgcat[i + j] = s2[j];
    }

    strgcat[strlen(s1) + strlen(s2)] = '\0';

    printf("%s\n", strgcat);
    return 0;
}

// 8. 
char *mystrcpy (char *dest, char source[]){
    int i;

    for (i = 0; source[i] != '\0'; i++) {
        dest[i] = source[i];
    }
    dest[i] = '\0';

    return dest;
}

// 9.
int mystrcmp (char s1[], char s2[]) {
    int i = 0, x, y; 
    
    while (s1[i] == s2[i] && s1[i])
        i++;

    x = s1[i];
    y = s2[i];

    return x - y;
}

// 10. ver
char *mystrstr (char s1[], char s2[]) { 
    if (*s1 == '\0' && *s2 == '\0') return s1;
    
    if (*s1 == '\n' && *s2 == '\0') return s1;
    
	for ( ; *s1 ; s1++ ) {
		if (*s1 == *s2) {
			char *r = s1;
			char *p = s2;

			while (*s1 == *p && *s1) {
				p++;
				s1++;
			}

			if (*p == '\0') return r;
				
		}
	}

    return NULL;
}

// 11.
void strrev (char s[]) {
    int i = strlen(s) - 1, j;
    char s2[strlen(s)];

    for(j = 0; j < strlen(s); j++){
        s2[j] = s[i];
        i--;
    }
    s2[j] = '\0';
    printf("%s\n", s2);
}

// 12.
int vogal(char s[], int size) {
  char vogal[] = "aeiouAEIOU";
  int acc = 0;

  for (int i = 0; i < size; i++) {
    for (int j = 0; j < 10; j++) {
      if (s[i] == vogal[j]) acc++;
    }
  }
  return acc;
}

void mystrnoV (char s[]) {
    char vogais[] = "aeiouAEIOU";
    int size = strlen(s);
    int sizenV = size - vogal(s, size);
    char s2[sizenV];
    int index = 0;

    for (int i = 0; i < size; i++) {
        int isVowel = 0;

        for (int j = 0; j < 10; j++) {
            if (s[i] == vogais[j]) isVowel = 1;
        }
        if (isVowel == 0) {
            s2[index] = s[i];
            index++;
        }
    }
    s2[sizenV] = '\0';
    printf("%s", s2);
}

// 13.
void truncW (char t[], int N) {
    int i, j = 0, n = 0;

    for (i = 0; t[i]; i++) {
        if (t[i] == ' ') {
            n = 0;
            t[j] = t[i];
            j++;
        } else {
            if (n < N){
                t[j] = t[i];
                j++;
                n++;
            }
        }
    }
    t[j] = '\0';
}

// 14.
char charMaisfreq (char s[]) {
    int count = 0, maior = 0;
    char maisFreq = s[0];

    for (int i = 0; s[i]; i++){
        for (int j = 0; s[j]; j++){
            if (s[i] == s[j]) count++;
        }
        if (count > maior) {
            maior = count; 
            maisFreq = s[i];
        }
    }
    return maisFreq;
}

// 15.
int iguaisConsecutivos (char s[]) {
    int count = 1, maior = 0, i = 0; 
    
    while(s[i]){
        count = 1;
        while (s[i] == s[i + 1]) {
            count++;
            i++;
        }
        if (count >= maior) maior = count;
        
        i++;
    }
    return maior;
}

// 16.
int difConsecutivos (char s[]) {
    int count = 1, maior = 0, i = 0; 
    
    while(s[i]){
        count = 1;
        while (s[i] != s[i + 1]) {
            count++;
            i++;
        }
        if (count >= maior) maior = count;
        
        i++;
    }
    return maior;
}

// 17. 
int maiorPrefixo (char s1 [], char s2 []) {
    int count = 0;

    for (int i = 0; s1[i]; i++) {
        if (s1[i] == s2[i]) {
            count++;
        } else {
        return count;
        }
    }
    return count;
}

// 18.
int maiorSufixo (char s1 [], char s2 []) {
    int size1 = strlen(s1) - 1;
    int size2 = strlen(s2) - 1;
    int count = 0;

    for (int i = 0; s1[i]; i++) {
        if (s1[size1] == s2[size2]) {
            count++;
            size1--;
            size2--;
        } else {
        return count;
        }
    }
    return count;
}

// 19. ver mais tarde
int sufPref (char s1[], char s2[]) {
    int count = 0, j = 0;

    for(int i = 0; s1[i]; i++){
        if(s1[i] == s2[j]){
            count++;
            j++;
        } else {
            count = 0;
            j = 0;
        }
    }
    return count;
}

// 20. 
int contaPal (char s[]) {
    int count = 1;
    int size = strlen(s);

    for (int i = 0; i < size; i++) {
        if (isspace(s[i])) count++;
    }
    return count;
}

// 21.
int contaVogais (char s[]) {
    int count = 0;
    char vogais[] = "aeiouAEIOU";
    int size = strlen(s);

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < 10; j++) {
            if (s[i] == vogais[j]) count++;
        }
    }
    return count;
}

// 22. ver este, penso que está mal
int contida (char a[], char b[]) {
    int sizeA = strlen(a);
    int sizeB = strlen(b);
    int count = 0;

    for (int i = 0; i < sizeA; i++){
        for (int j = 0; j < sizeA; j++){ // sizeB
            if (a[i] == b[j]) count++;
        }
        if (count == (sizeB - 1)) return 1; // sizeA
    }
    return 0;
}

// 23. 
int palindrome (char s[]) {
    int i, j = strlen(s) - 1, result = 1;

    for (i = 0; i < (strlen(s) / 2) ; i++) {
        if (s[j] == s[i]) j--;
        else {
            result = 0;
            break;
        }
    }
    return result;
}

// 24. 
int remRep (char x[]) {
    int i; 
    for (i = 0; x[i]; i++){
        if (x[i] == x[i + 1]){
            for (int j = i + 1; x[j]; j++)
                x[j] = x[j + 1];
            i--;
        }
    }
    return i;
}

// 25. 
int limpaEspacos (char t[]) {
    int i, j;

    for (i = 0; t[i]; i++) {
        if (t[i] == ' ' && t[i + 1] == ' ') {
            for (j = i + 1; t[j]; j++)
                t[j] = t[j + 1];
            i--;
        }
    }
    return i;
}

// 26. ver, não percebi um crl
void insere (int v[], int N, int x) {
     int i, j;

    for (i = 0; v[i]; i++) {
        if (v[i] > x){
            for (j = N; j > i; j--) {
                v[j] = v[j - 1];
            }
            v[i] = x;
            break;
        }
        if (i == N - 1) {
            v[N] = x;
        }
    }
}

// 27.
void merge (int r [], int a[], int b[], int na, int nb) {
    int i = 0, j = 0, k;

    for(k = 0; i < na && j < nb; k++){
        if(a[i] < b[j]){
            r[k] = a[i];
            i++;
        } else {
            r[k] = b[j];
            j++;
        }
    }
            
    while (i < na) {
        r[k] = a[i];
        k++;
        i++;
    }

    while (j < nb) {
        r[k] = b[j];
        k++;
        j++;
    }
}

// 28. 
int crescente (int a[], int i, int j) {
    int res = 1;

    while (i != j){
        if(a[i] > a[i + 1])
            res = 0;
        i++;
    }
    return res;
}

// 29.
int retiraNeg(int v[], int N) {
    int i = 0, j;

    while (i < N) {
        if (v[i] < 0) {
            for(j = i; j < N - 1; j++)
                v[j] = v[j + 1];
            N--;
        } else 
            i++;
    }
    return N;
}

// 30. 
int contaRepetidos(int numero, int v[], int N){
    int i = 0, count = 0;

    while(i < N){
        if(numero == v[i])
            count++;
        i++;
    }
    return count;
}

// 31.
int menosFreq (int v[], int N){
    int i = 0, resultado = v[i];
    int menor = contaRepetidos(v[i], v, N);
    while(i < N){
        int x = contaRepetidos(v[i], v, N);
        if(x<menor){
            menor = x;
            resultado = v[i];
        }
        i+=x;
    }
    return resultado;
}

// 32.
int maxCresc (int v[], int N) {
    int i, maior = 1, r = 1;

    for (i = 0; i < N; i++){
        if(v[i + 1] >= v[i])
            r++;
        else {
            if(r > maior)
                maior = r;
            r = 1;
        }
    }
    return maior;
}

// 33. 
int elimRep (int v[], int n) {
    int i = 1;
    while (i < n) {
        int belongs = 0;

        for (int j = 0; j < i; j++) {
            if(v[i] == v[j]) belongs = 1;
        }
        if (belongs) {
            for (int j = i; j < n; j++) {
                v[j] = v[j + 1];
            }
            n--;
        } else i++;
    }
    return n;
}

// 34. 
int elimRepOrd (int v[], int n) {
    int i = 0, j;

    while (i < n - 1) {
        if (v[i] == v[i + 1]) {
            for (j = i; j < n; j++)
                v[j] = v[j + 1];
            n--;
        } else
            i++;
    }
    return n;
}

// 35. 
int comunsOrd (int a[], int na, int b[], int nb) {
    int i = 0, j = 0, count = 0;

    while(i < na && j < nb) {
        if (a[i] == b[j]) {
            count++;
            i++; 
            j++;
        } else {
            if (a[i] < b[j])
                i++;
            else
                j++;
        }
    }
    return count;
}

// 36.
int comuns (int a[], int na, int b[], int nb) {
    int count = 0;

    for (int i = 0; i < na; i++) {
        int belongs = 0;

        for (int j = 0; j < nb; j++) 
            if (a[i] == b[j]) belongs = 1;

        if (belongs) count++;
    }
    return count;
}

// 37.
int minInd (int v[], int n) {
    int idx_menor = 0;
    for (int i = 1; i < n; i++) {
        if (v[i] < v[idx_menor]) idx_menor = i;
    }
    return idx_menor;
}

// 38.
void somasAc (int v[], int Ac [], int N) {
    for (int i = 0; i < N; i++) {
        Ac[i] = 0;
        for(int j = 0; j <= i; j++) {
            Ac[i] += v[j];
        }
    }
}

// 39.
int triSup (int N, float m [N][N]) {
    for (int i = 1; i < N; i++) {
        for (int j = 0; j < i; j++) {
            if (m[i][j] != 0) return 0;
        }
    }
    return 1;
}

// 40.
void transposta (int N, float m [N][N]) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < i; j++) {
            float temp = m[i][j];
            m[i][j] = m[j][i];
            m[j][i] = temp;
        }
    }
}

// 41. 
void addTo (int N, int M, int a [N][M], int b[N][M]) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            a[i][j] += b[i][j];
        }
    }
}

// 42.
int unionSet (int N, int v1[N], int v2[N], int r[N]) {
    int i;
    for (i = 0; i < N; i++)
        r[i] = v1[i] || v2[i];
    return i;
} 

// 43.
int intersectSet (int N, int v1[N], int v2[N], int r[N]) {
    int i;
    for (i = 0; i < N; i++)
        r[i] = v1[i] && v2[i];
    return i;
}

// 44.
int intersectMSet (int N, int v1[N], int v2[N], int r[N]) {
    int count = 0;
    for (int i = 0; i < N; i++) {
        r[i] = v1[i] < v2[i] ? v1[i] : v2[i];
        count += r[i]; 
    }
    return count;
}

// 45.
int unionMSet (int N, int v1[N], int v2[N], int r[N]) {
    int count = 0;
    for (int i = 0; i < N; i++) {
        r[i] = v1[i] + v2[i];
        count += r[i]; 
    }
    return count;
}

// 46. 
int cardinalMSet (int N, int v[N]) {
    int count = 0;
    for(int i = 0; i < N; i++) count += v[i];
    return count;
}

int main () {
    //int v[]= {1, 2, 3, 2, 1, 4, 2, 4, 5, 4};
    //int idx_menor = minInd (v,10);
    //printf("%d\n", idx_menor);
}