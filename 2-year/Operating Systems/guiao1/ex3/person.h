#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#define FILENAME "file_pessoas"

#define DEBUG 1

typedef struct person {
    char name[200];
    int age;
} Person;

int new_person (char *name, int age); 

int list_n_persons (int N);

int person_change_age (char *name, int age);