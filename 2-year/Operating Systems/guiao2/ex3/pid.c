#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

int main () {
    pid_t pid;
    int status;
    
    for (int i = 0; i < 10; i++) {    
        pid = fork();

        if (pid == 0) { // processo-filho
            
            printf("Filho: %d (new) -- %d (pai)\n", getpid(), getppid());

            sleep(1);
            _exit(i);
        } else { // processo-pai
            pid_t terminated_pid = wait(&status);

            if (WIFEXITED(status)) { // se o processo filho terminou normalmente
                printf("iteração: %d -- %d (filho) -- %d (status)\n", i, terminated_pid, WEXITSTATUS(status));
            }
        }
    }

    printf("terminei!\n");

    return 0;
}