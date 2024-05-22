#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int main() {
    int fd[2];

    int pipe_return = pipe(fd);
    if (pipe_return == -1) {
        perror("erro na criação de pipe");
        return -1;
    }

    pid_t pid = fork();

    if (pid == 0) { // filho: produtor
        close(fd[0]);
        int value = 10;

        for (int i = 0; i < 100; i++) {   
            ssize_t bytes_written = write(fd[1], &value, sizeof(int));
            printf("Filho: escrevi o valor %d (%zd bytes) \n", value, bytes_written);
        }
        close(fd[1]);

        _exit(0);
    } else { // pai: consumidor
        close(fd[1]);

        sleep(5);

        int res = 0;
        for (int i = 0; i < 100; i++) {
            ssize_t bytes_read = (fd[0], &res, sizeof(int));
            printf("Pai: recebi o valor %d (%zd bytes)\n", res, bytes_read);
        }
        close(fd[0]);

        pid_t terminated_pid = wait(&res);

        int status;
        printf("Pai: o processo-filho %d terminou com o código de saída %d \n", terminated_pid, WEXITSTATUS(status));
    }

	return 0;
}