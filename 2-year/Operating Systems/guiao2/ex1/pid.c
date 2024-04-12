#include <stdio.h>
#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <sys/wait.h> /* chamadas wait*() e macros relacionadas */

int main() {
    pid_t pid;
    int status;

    pid = fork();

    if (pid == 0) { // fork() retorna 0 ao processo-filho
        printf("Eu sou o filho, com PID %d\n", getpid());
        _exit(0); // _exit() termina o processo atual com código passado por argumento
    } else { // fork() retorna o PID do filho (ex: 2035) ao processo-pai
        printf("Eu sou o pai, com PID %d\n", getpid());
        wait(&status); 
        // A variável status é atualizada com o código passado na chamada da função _exit()
    }

    /* 
        wait():
            bloqueia o processo-pai até um processo-filho terminar. Retorna PID do processo-filho que terminou

            A variável status é atualizada com o código passado na chamada da função _exit()
    */

    return 0;
}