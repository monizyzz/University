#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>


#define MAX_LINE_SIZE 1024

int main() {

    int fifo_fd = open("fifo", O_WRONLY);
    printf("FIFO opened to write ...\n");

    if (fifo_fd == -1) {
        perror("open");
        return 1;
    }

    char buffer[MAX_LINE_SIZE];
    ssize_t bytes_read;

    while ((bytes_read = read(0, buffer, sizeof(buffer))) > 0) {
        if (write(fifo_fd, buffer, bytes_read) == -1) {
            perror("write");
            return 1;
        }
    }

    close(fifo_fd);

    return 0;
}