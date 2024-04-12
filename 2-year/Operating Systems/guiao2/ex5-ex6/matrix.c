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


// ex. 5
int valueExists(int **matrix, int value) {
    for (int i = 0; i < ROWS; i++) {
        pid_t pid = fork();

        if (pid == 0) {
            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    printf("Value %d exists.\n", value);
                    exit(0);
                }
            }
            exit(1);
        }
        
    }
    return 0;
}


// ex. 6
void linesWithValue(int **matrix, int value) {
    for (int i = 0; i < ROWS; i++) {
        pid_t pid = fork();

        if (pid == 0) {
            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    printf("Line %d has the value.\n", i);
                    exit(0);
                }
            }
            exit(1);
        }
        
    }
}