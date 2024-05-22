#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>

int main() {
    int bytes_read;
    char buffer[1024];

    int p[2];
    pipe(p);
    if (pipe(p) == -1) {
        perror("pipe");
        return 1;
    }

    int fork_res = fork();
    if (fork_res == -1) {
        perror("fork");
        return 2;
    }

    if (fork_res == 0) { // child
        close(p[1]); // se nao fechar o pipe, o wc nao termina e o programa fica entra em deadlock
        
        dup2(p[0], 0); // ler do pipe ao inves do stdin
        
        close(p[0]);

        int exec_res = execlp("wc", "wc", NULL);
        _exit(exec_res);

    } else { // parent
        close(p[0]); 

        while((bytes_read = read(0, buffer, 1024)) > 0) {
            write(p[1], buffer, bytes_read);
        }
            
        close(p[1]);

        wait(NULL);
    }

    return 0;
}