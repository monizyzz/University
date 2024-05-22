#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int main() {
    int fd[2];
    int value;
    
    if (pipe(fd) == -1) {
        perror("pipe");
        return 1;
    } 

    int res = fork();

    if (res == 0) {
        close(fd[0]);

        for (int i = 0; i < 5; i++) {
            value = i;
            write(fd[1], &value, sizeof(value));
            sleep(1);
        }

        close(fd[1]);
    } else {
        close(fd[1]);

        ssize_t bytes_read;
        while ((bytes_read = read(fd[0], &value, sizeof(value))) > 0) {
            printf("Parent received value: %d\n", value);
        }

        close(fd[0]);
    }

    return 0;
}