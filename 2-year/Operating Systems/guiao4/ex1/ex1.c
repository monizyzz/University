#include <unistd.h>
#include <stdio.h>

int main() {
    int fd[2];
    int value = 10;
    
    if (pipe(fd) == -1) {
        perror("pipe");
        return 1;
    } 

    // fd[0] -> read
    // fd[1] -> write

    int res = fork();

    if (res == 0) {
        close(fd[1]);
        read(fd[0], &value, sizeof(value));
        printf("Child received value: %d\n", value);
        close(fd[0]);
    } else {
        close(fd[0]);
        sleep(5);
        write(fd[1], &value, sizeof(value));
        close(fd[1]);
    }
    
    return 0;
}