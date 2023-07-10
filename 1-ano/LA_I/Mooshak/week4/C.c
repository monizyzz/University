#include <stdio.h>
#include <string.h>

// Função que verifica se há letras repetidas em uma substring, retorna 1 se houver repetição e 0 caso contrário.
int repetido(char word[], int esp){
    for(int i = 0; i < esp ; i++){
        for(int j = i + 1; j < esp; j++) {
            if(word[i] == word[j]){ 
                return 1;
            }
        }
    }
    return 0;
}
// Função que encontra a posição da primeira substring sem letras repetidas na string "word", retorna -1 se não houver tal substring.
int sem_repetidos(char word[], int esp){
    int len = strlen(word);
    for(int i = 0; i <= (len - esp); i++){
        if(!repetido (word + i, esp)) return i;
    }
    return -1;
}

int main() {
    int N;
    int esp;
    char word[1024];

    if(scanf("%d", &N) != 1) return 1;
    int acc[N];

    for(int i = 0; i < N; i++) {
        if(scanf("%d" , &esp) != 1) return 1;
        if(scanf("%s", word) != 1) return 1;
        int pos = sem_repetidos (word,esp);
        acc[i] = pos;
    }

    for(int i = 0; i < N; i++) {
        printf("%d\n",acc[i]);
    }

    return 0;
}