#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

void defeitos(int n, char* imagens[n], int max) {
    int atual = 0;
    int status;

    for (int i = 0; i < n; i++) {
        if (fork() == 0) {
            int res = execlp("detectAnom", "detectAnom", imagens[i], NULL);
            _exit(res);
        }

        atual++;

        if (max == atual) {
            wait(&status);
            atual--;
        }

    }
}


void conta(int n, char* imagens[n]) {
    int p[2];
    pipe(p);

    if (fork() == 0) {
        close(p[0]);
        dup2(p[1], 1);
        close(p[1]);
        defeitos(n, imagens, 1);
        _exit(0);
    }

    int status;
    wait(&status);

    if (fork() == 0) {
        close(p[1]);
        dup2(p[0], 0);
        close(p[0]);
        execlp("wc", "wc", "-l", NULL);
        _exit(0);
    } 

    close(p[1]);
    close(p[0]);

    wait(&status);
}