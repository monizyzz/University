#include <stdio.h>
#include <unistd.h>

int main(int argc, char** argv) {
    // Will run $PATH
    int res = execlp("ls", "ls", "-l", NULL);

    printf("res = %d\n", res);

    return 0;
}