#include "matrix.h"


int **createMatrix() {

    // seed random numbers
    srand(time(NULL));

    // Allocate and populate matrix with random numbers.
    printf("Generating numbers from 0 to %d...", MAX_RAND);
    int **matrix = (int **) malloc(sizeof(int*) * ROWS);
    for (int i = 0; i < ROWS; i++) {
        matrix[i] = (int*) malloc(sizeof(int) * COLUMNS);
        for (int j = 0; j < COLUMNS; j++) {
            matrix[i][j] = rand() % MAX_RAND;
        }
    }
    printf("Done.\n");

    return matrix;
}

void printMatrix(int **matrix) {

    for (int i = 0; i < ROWS; i++) {
        printf("%2d | ", i);
        for (int j = 0; j < COLUMNS; j++) {
            printf("%7d ", matrix[i][j]);
        }
        printf("\n");
    }
}

void lookupNumber(int** matrix, int value, int* vector){
    int i = 0;

    for (i = 0; i < ROWS; i++) {
        int fd[2];
        int pipe_return = pipe(fd);
        if (pipe_return == -1) {
            perror("erro na criação de pipe");
            return;
        }

        pid_t pid = fork();
        Minfo info;

        if (pid == -1) {
            perror("erro na criação de processo");
            return;
        }
        
        if (pid == 0) { 
            close(fd[0]);
            info.line_nr = i;
            info.ocur_nr = 0;

            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    info.ocur_nr++;
                }
            }
            write(fd[1], &info, sizeof(Minfo));
            close(fd[1]);
            _exit(0);

        } else { 
            close(fd[1]);
            ssize_t bytes_read;
            Minfo count;
            while ((bytes_read = read(fd[0], &count, sizeof(Minfo))) > 0) {
                if (count.ocur_nr > 0) { 
                    vector[count.line_nr] = count.ocur_nr;
                }
            }
            close(fd[0]);
            wait(NULL); // Wait for child process to finish
        }
    }
}