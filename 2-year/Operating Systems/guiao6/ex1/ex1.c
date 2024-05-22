#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>

int main() {
    // in 
    int fd_in = open("/etc/passwd", O_RDONLY);

    if (fd_in == -1) {
        perror("Erro ao abrir o ficheiro de input");
        return -1;
    }

    int res = dup2(fd_in, 0);

    if (res == -1) {
        perror("Erro ao redirecionar o input");
        return -1;
    }

    printf("dup2 in = %d\n", res);

    close(fd_in);



    // out
    int fd_out = open("saida.txt", O_WRONLY | O_CREAT | O_TRUNC, 0666);

    if (fd_out == -1) {
        perror("Erro ao abrir o ficheiro de output");
        return -1;
    }

    int fd_out_original = dup(1);

    int resOut = dup2(fd_out, 1);
    if (resOut == -1) {
        perror("Erro ao redirecionar o output");
        return -1;
    }
    

    printf("dup2 out = %d\n", resOut);

    close(fd_out);





    // error
    int fd_err = open("erros.txt", O_WRONLY | O_CREAT | O_TRUNC, 0666);

    if (fd_err == -1) {
        perror("Erro ao abrir o ficheiro de err");
        return -1;
    }

    int resErr = dup2(fd_err, 2);

    if (resErr == -1) {
        perror("Erro ao redirecionar o fd_err");
        return -1;
    }

    printf("dup2 err = %d\n", resErr);

    close(fd_err);


    

    char buffer[1024];
    int bytes_read;

    while(bytes_read = read(0, buffer, 1024) > 0) {
        write(1, buffer, bytes_read);
        write(2, buffer, bytes_read);
    }

    dup2(fd_out_original, 1);

    printf("Fim do programa\n");

    return 0;
}