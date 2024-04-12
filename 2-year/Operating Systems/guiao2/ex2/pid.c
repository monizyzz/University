#include <stdio.h>
#include <unistd.h> 
#include <sys/wait.h>

int main() {
    pid_t pid;
    int status;

    pid = fork();

    if (pid == 0) {
        printf("Eu sou o filho, com PID %d e o meu pai é %d\n", getpid(), getppid());
        _exit(0);
    } else {
        printf("Eu sou o pai, com PID %d e o meu pai é %d\n", getpid(), getppid());
        wait(&status);
    }

    return 0;
}