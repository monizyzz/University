#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

int main() {

    int res = mkfifo("fifo", 0666);
    if (res == -1) {
        perror("mkfifo");
        return 1;
    }

    return 0;
}