#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h> 

int main(int argc, char** argv) {

    int res = 0, status;

    char *commands[3] = {"ls", "-l", NULL};

    pid_t pid = fork();

    if (pid == 0) {
        printf("Child process\n");

        // res = execlp("ls", "ls", "-l", NULL);
        res = execvp(commands[0], commands);
        _exit(0);

    } else {
        int terminated =  wait(&status);
        printf("The child process has finished with the status %d. %d\n", terminated, WEXITSTATUS(status));
    }

    return res;
}