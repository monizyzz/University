#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>


#define MAX_LINE_SIZE 1024

int main() {

    int fifo_fd = open("fifo", O_RDONLY);
    printf("FIFO opened to read ...\n");

    if (fifo_fd == -1) {
        perror("open");
        return 1;
    }

    char buffer[MAX_LINE_SIZE];
    ssize_t bytes_read;

    while ((bytes_read = read(fifo_fd, buffer, sizeof(buffer))) > 0) {
        if (write(1, buffer, bytes_read) == -1) {
            perror("write");
            return 1;
        }
    }

    close(fifo_fd);

    return 0;
}