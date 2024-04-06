#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>

int main () {
    int buffer_size = 1024;
    
    // char buffer[1024];
    char* buffer = malloc(sizeof(char) * buffer_size);
    
    ssize_t bytes_read;

    while((bytes_read = read(0, buffer, buffer_size)) > 0) {
        write(1, buffer, bytes_read);
    }

    free(buffer);

    return 0;
}