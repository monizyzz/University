#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>

int main () {
    int fd_origin = open("text.txt", O_RDONLY);
    
    if (fd_origin == -1) {
        perror("fd: ");
    }

    int fd_detin = open("text-copy.txt", O_CREAT | O_WRONLY | O_TRUNC, 0644);

    int buffer_size = 1024; 
    char* buffer = malloc(sizeof(char) * buffer_size);
    
    ssize_t bytes_read, bytes_written;

    while((bytes_read = read(fd_origin, buffer, buffer_size)) > 0) {
        bytes_written = write(fd_detin, buffer, bytes_read);
    }

    free(buffer);

    close(fd_origin);
    close(fd_detin);
} 