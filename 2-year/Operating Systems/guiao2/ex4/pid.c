#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

int main () {
    pid_t pid;
    int status;
    int i = 0;
    
    for (int j = 0; j < 10; j++) {    
        pid = fork();

        if (pid == 0) {
            
            printf("Filho: %d (new) -- %d (pai) -- %d (i)\n", getpid(), getppid(), i);

            sleep(2);
            _exit(i);
        } else { 
            i++;
        }
    }

    for (int i = 0; i < 10; i++) {
        pid_t terminated_pid = wait(&status);
        if (WIFEXITED(status)) {
            printf("PAI %d :: filho %d terminou.\n", WEXITSTATUS(status), terminated_pid);
        }
    }

    printf("terminei!\n");

    return 0;
}