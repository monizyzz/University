#include "person.h"

// sprintf -> grab string and put it in a buffer

int new_person (char *name, int age) {
    Person p;
    p.age = age;
    strcpy(p.name, name);

    int fd = open(FILENAME, O_CREAT | O_APPEND | O_WRONLY, 0644);

    if (fd <= -1) {
        perror("Erro no open");
        return -1;
    }

    ssize_t bytes_written = write(fd, &p, sizeof(Person));

    if (bytes_written == -1) {
        perror("Erro no write");
        return -1;
    }


    long pos = lseek(fd, -sizeof(Person), SEEK_CUR); 
    

    printf("Pessoa adicionada: %s, %d\n", name, age);

    close(fd);

    return pos/sizeof(Person);
} 



int list_n_persons (int N) {
    Person p;

    int fd = open(FILENAME, O_RDONLY, 0644);


    int i = 0;
    while (i < N && read(fd, &p, sizeof(Person)) > 0) {
        printf("Registo %d {name: %s; age: %d}\n", i, p.name, p.age);
        
        i++;
    }
    
    close(fd);

    return i;
}



int person_change_age(char *name, int age) {
    Person p;
    int fd = open(FILENAME, O_WRONLY | O_RDONLY, 0644);
    int found = 0, n;

    while ((n = read(fd, &p, sizeof(p))) > 0) {
        if (strcmp(p.name, name) == 0) {
            printf("Nome Encontrado!\n");
			found = 1;
            p.age = age;
            lseek(fd, -sizeof(p), SEEK_CUR);
            write(fd, &p, sizeof(p));
            printf("Idade da pessoa %s alterada para %d\n", name, age);
            close(fd);
            return 0;
        }
    }

    if(n == -1)
		printf("Algo nao funciona\n");

    if(found != 1)
		printf("Pessoa nao encontrada\n");

    close(fd);

    return -1;
}