#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
    FILE * C_conflict;
    FILE * C_bit_conflict;

    char C, C_bit;
    int line_counter = 0;
    int vertex_counter = 0;

    C_conflict = fopen("C_conflict_log.txt", "r");
    C_bit_conflict = fopen("C_bit_conflict_log.txt", "r");

    printf("%llu\n", time(NULL));

    while ( (C = fgetc(C_conflict)) != EOF && (C_bit = fgetc(C_bit_conflict)) != EOF){
        if (C == '\n'){
            line_counter++;
            vertex_counter = 0;
        } else if (C != '-'){
            vertex_counter++;
        }

        
            
        
        if (C != C_bit){
            printf("Content mismatch in vertex %d with neighbor %d\n", line_counter, vertex_counter);
            return -1;
        }

        
    }
    printf("The contents are all same");
}