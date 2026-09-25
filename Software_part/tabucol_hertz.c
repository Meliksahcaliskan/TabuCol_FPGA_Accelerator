#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> 
#include <time.h>
#include <math.h>

#define MAX_FS(x) ((x) * (x-1) / 2)

typedef struct tabu_node
{
    int vertex;
    int color;
    struct tabu_node* next;
}tabu_node;

int **edge_matrix;
int **conflict_matrix;

static inline uint64_t rdtsc(void) {
    uint32_t lo, hi;
    __asm__ __volatile__ (
        "mfence\n\t"      /* serialise: wait for all prior memory ops */
        "rdtsc"
        : "=a"(lo), "=d"(hi)
        :
        : "memory"
    );
    return ((uint64_t)hi << 32) | lo;
}

double calculate_population_std(double data[], int n) {
    double sum = 0.0;
    double mean;
    double variance_sum = 0.0;

    // Step 1: Calculate the Mean
    for (int i = 0; i < n; i++) {
        sum += data[i];
    }
    mean = sum / n;

    // Step 2: Sum the squared differences from the mean
    for (int i = 0; i < n; i++) {
        variance_sum += (data[i] - mean) * (data[i] - mean);
    }

    // Step 3: Divide by N and take the square root
    return sqrt(variance_sum / n);
}

int scan_tabu_list(int vertex, int color, tabu_node* tabu_head){
    tabu_node *cur = tabu_head;

    do{
        if (cur->vertex == vertex && cur->color == color)
            return 1;
        cur = cur->next;
    }while(cur != tabu_head);

    return 0;
}

void print_tabu_list(tabu_node *head){
    tabu_node *cur = head;

    do{
        printf("x%d->%dx\n", cur->vertex, cur->color);
        cur = cur->next;
    }while(cur != head);

    return;
}

void print_conflict_matrix(int vertex_N){
    for (int i = 0; i < vertex_N; i++){
        for (int j = 0; j < vertex_N; j++)
            printf("%d-", conflict_matrix[i][j]);
        printf("\n");
    }
    printf("\n");

    return ;
}

int is_vertex_conflict(int target_vertex, int vertex_N){
    for (int i = 0; i < vertex_N; i++){
        if (conflict_matrix[target_vertex][i])
            return 1;
    }

    return 0;
}

int solve(int vertex_N, int color_num ,int iter, int rep,int *color_array, int **edge_matrix, tabu_node* tabu_cur, double *sec){
    

    int fs = 0;
    char chr;
    int i,j;
    int aspiration[MAX_FS(vertex_N)];

    
    int rand_vertex, vertex_min;
    int rand_color, color_min;
    int fs_temp;
    int fs_min;
    int rep_temp;

    int valid;  /// Tabu list controller
    int asp_valid;
    int debug = 0;

    struct timespec t_start, t_end;
    clock_gettime(CLOCK_MONOTONIC, &t_start);
    uint64_t cyc_start = rdtsc();
    // rastgele renk atama
    //printf("colors: ");
    for (i = 0; i < vertex_N; i++){
        color_array[i] = rand() % color_num;
        //printf(" %d ", color_array[i]);
    }

    
    //printf("\n");

    // fs sayısını hesaplama ve conflict arrayi ayarlama
    for (i = 0; i < vertex_N; i++){
        
        for (j = 0; j < i; j++){
            fs = fs + (edge_matrix[i][j]*(color_array[i] == color_array[j]));
            if (edge_matrix[i][j]*(color_array[i] == color_array[j])){
                conflict_matrix[i][j] = 1;
                conflict_matrix[j][i] = 1;
            }
        }
    }

    if (fs == 0)
        return 0;
    

    for (i = 0; i < MAX_FS(vertex_N); i++){
        aspiration[i] = i-1;
    }

    
    //printf("\nfs: %d\n", fs);
    if (!debug){

        while (fs > 0 && iter > 0){

            rep_temp = rep;
            fs_min = vertex_N*vertex_N+1;

        
            while (rep_temp > 0){
                
                
                valid = 0;
                asp_valid = 0;

                do{
                    fs_temp = fs;

                    do{
                        
                        rand_vertex = rand() % vertex_N;
                    }while (is_vertex_conflict(rand_vertex, vertex_N) == 0);

                    do{
                        rand_color = rand() % color_num;
                        
                    }while (rand_color == color_array[rand_vertex]);
                
                    /* if (iter < 15){
                        printf("While sonrasi tabu list.\n");
                        printf("Move: %d->%d\n", rand_vertex, rand_color);
                        print_tabu_list(tabu_cur);
                        printf("Is it valid: %d", scan_tabu_list(rand_vertex, rand_color, tabu_cur));
                        scanf("%c", &chr);
                    } */

                    for (i = 0; i < vertex_N; i++){
                        fs_temp -= edge_matrix[rand_vertex][i]*(color_array[rand_vertex] == color_array[i]);
                        fs_temp += edge_matrix[rand_vertex][i]*(color_array[i] == rand_color);
                    }

                    if (fs_temp <= aspiration[fs]){
                        aspiration[fs] = fs_temp-1;
                        break;
                    }
                        
                
                } while (scan_tabu_list(rand_vertex, rand_color, tabu_cur));
                /* printf("The random vertex: %d\nfirst color: %d\nrandom color: %d\n", rand_vertex, color_array[rand_vertex], rand_color);
                printf("fmin: %d--fs_temp: %d----rep: %d\n", fs_min, fs_temp, rep_temp); */
                
                
                
                if (fs_temp < fs_min){
                    fs_min = fs_temp;
                    vertex_min = rand_vertex;
                    color_min = rand_color;
                }
                    

                if (fs_temp < fs)
                    rep_temp = 1;
                
                rep_temp--;

            }
            fs = fs_min;
            
            /* if (iter < 15){
                printf("The current tabu: %d->%d\n", tabu_cur->vertex, tabu_cur->color);
                printf("The tabu list before move: \n");
                print_tabu_list(tabu_cur);
            } */
            
            //printf("\n\nfs: %d\n\n", fs);
            tabu_cur->vertex = vertex_min;
            tabu_cur->color = color_array[vertex_min];
            color_array[vertex_min] = color_min;
            tabu_cur = tabu_cur->next;

            /* if (iter < 15){
                printf("The tabu list after move: \n\n");
                print_tabu_list(tabu_cur);
                scanf("%c", &chr);
            } */
            

            

            //printf("Pre first move:\n");
            //print_conflict_matrix(vertex_N);
            //printf("colors: ");
            for (i = 0; i < vertex_N; i++){
                //printf(" %d ", color_array[i]);
                if (edge_matrix[vertex_min][i]){
                    conflict_matrix[i][vertex_min] = color_array[i] == color_min;
                    conflict_matrix[vertex_min][i] = color_array[i] == color_min;
                }
            }
            //printf("\n");
            //printf("After first move: \n");
            //print_conflict_matrix(vertex_N);
            /* printf("The conflict: ");
            for (i = 0; i < vertex_N; i++){
                printf(" %d ", conflict_array[i]);
            }
            printf("\n"); */
            iter--;
            
            /* if(iter % 100000 == 0){
                printf("Iter: %d\nfs: %d\n", iter, fs);
                //print_tabu_list(tabu_cur);
                //print_conflict_matrix(vertex_N);
            } */
                
            //printf("Iter: %d--fs: %d\n", iter, fs);
        }
    }
    
    uint64_t cyc_end = rdtsc();
    clock_gettime(CLOCK_MONOTONIC, &t_end);

    *sec = (double)(t_end.tv_sec  - t_start.tv_sec)
         + (double)(t_end.tv_nsec - t_start.tv_nsec) * 1e-9;
    
    return fs;
}

int main(int argc, char** argv){

    if (argc < 9){
        printf("Abnormal parameter count.\n");
        return -1;
    }

    int vertex_N = atoi(argv[1]);
    int color_num = atoi(argv[2]);
    int rep = atoi(argv[3]);
    int tabu_tenure = atoi(argv[4]);
    int iter = atoi(argv[5]);
    char * testbench_file = argv[6];
    char * testbench_file_single = argv[7];
    char * testbench_file_multi = argv[8];
    int i, j;


    /// tabu list 
    tabu_node* tabu_cur;
    tabu_node* head;
    tabu_cur = (tabu_node*)malloc(sizeof(tabu_node));
    head = tabu_cur;
    tabu_cur->color = -1;
    tabu_cur->vertex = -1;
    tabu_cur->next = NULL;
    for (i= 0; i < tabu_tenure-1; i++){
        tabu_cur->next = (tabu_node*)malloc(sizeof(tabu_node));
        tabu_cur = tabu_cur->next;
        tabu_cur->color = -1;
        tabu_cur->vertex = -1;
    }
    tabu_cur->next = head;
    tabu_cur = head;
    ////

    /// edge-conflict matrix 
    edge_matrix = (int **)malloc(vertex_N*sizeof(int*));
    conflict_matrix = (int **)malloc(vertex_N*sizeof(int*));

    for (i = 0; i < vertex_N; i++){
        edge_matrix[i] = (int *)malloc(vertex_N*sizeof(int));
        conflict_matrix[i] = (int *)malloc(vertex_N*sizeof(int));
    }

    int color_array[vertex_N];
    
    srand(time(NULL));

    for (i = 0; i < vertex_N; i++){
        for (j = 0; j < vertex_N; j++){
            edge_matrix[i][j] = 0;
            conflict_matrix[i][j] = 0;
        }
        
    }

    FILE* graph_file;

    graph_file = fopen(testbench_file, "r");

    int vertex1, vertex2;
    while(!feof (graph_file)){
        fscanf(graph_file, "%d %d\n", &vertex1, &vertex2);
        //printf("The edge between %d <--> %d\n", vertex1, vertex2);
        edge_matrix[vertex1][vertex2] = 1;
        edge_matrix[vertex2][vertex1] = 1;
    }
    fclose(graph_file);

    FILE* single_bench_file;
    FILE* multi_bench_file;

    single_bench_file = fopen(testbench_file_single, "a");
    multi_bench_file = fopen(testbench_file_multi, "a");


    time_t raw_time = time(NULL);
    if (raw_time == -1) {
        perror("Failed to get current time");
        return 1;
    }

    // 2. Convert the time to local time structure
    struct tm *local_time = localtime(&raw_time);
    if (local_time == NULL) {
        perror("Failed to convert to local time");
        return 1;
    }

    // 3. Format the time into a readable string
    char time_buffer[64];
    char time_buffer_single[64];
    // Formats like: YYYY-MM-DD HH:MM:SS (e.g., 2026-09-12 13:05:22)
    strftime(time_buffer, sizeof(time_buffer), "%Y-%m-%d %H:%M:%S", local_time);


    // Solving the graph  ////////////
    double sec, sec_sum, sec_min, sec_max;
    double sec_array[10];
    int final_fs = 0;
    int fs_solved;

    printf("Bankaiiii\n");
    
    fs_solved = solve(vertex_N, color_num, iter, rep,color_array, edge_matrix, tabu_cur, &sec);
    
    
    sec_sum = sec;
    sec_min = sec;
    sec_max = sec;
    sec_array[0] = sec;
    final_fs = fs_solved;

    printf("The solved fs: %d - time : %f\n", fs_solved, sec);

    

    fprintf(single_bench_file, "%s,%d,%d,%d,%f,%d,%d\n", 
                                                        time_buffer, 
                                                        0,
                                                        rep, 
                                                        final_fs, 
                                                        sec, 
                                                        iter, 
                                                        tabu_tenure
    );

    for (i = 0; i < 4; i++){
        fs_solved = solve(vertex_N, color_num, iter, rep,color_array, edge_matrix, tabu_cur, &sec);
        final_fs += fs_solved;
        printf("The solved fs: %d - time : %f\n", fs_solved, sec);
        sec_sum += sec;

        if (sec < sec_min)
            sec_min = sec;
        
        if (sec > sec_max)
            sec_max = sec;

        sec_array[i+1] = sec;

        raw_time = time(NULL);
        if (raw_time == -1) {
            perror("Failed to get current time");
            return 1;
        }

        local_time = localtime(&raw_time);
        if (local_time == NULL) {
            perror("Failed to convert to local time");
            return 1;
        }

        strftime(time_buffer_single, sizeof(time_buffer), "%Y-%m-%d %H:%M:%S", local_time);

        fprintf(single_bench_file, "%s,%d,%d,%d,%f,%d,%d\n", 
                                                            time_buffer_single,
                                                            i+1, 
                                                            rep, 
                                                            fs_solved, 
                                                            sec, 
                                                            iter, 
                                                            tabu_tenure
        );
    }

    fprintf(multi_bench_file, "%s,%d,%d,%f,%f,%f,%f,%d,%d\n", 
                                                                    time_buffer, 
                                                                    rep, 
                                                                    final_fs/10.0, 
                                                                    sec_sum/10.0,
                                                                    sec_min, 
                                                                    sec_max, 
                                                                    calculate_population_std(sec_array, 10),
                                                                    iter, 
                                                                    tabu_tenure
    );


    fclose(single_bench_file);
    fclose(multi_bench_file);

    for (i = 0; i < tabu_tenure; i++){
        head = tabu_cur;
        tabu_cur = tabu_cur->next;
        free(head);
        
    }

    for (i = 0; i < vertex_N; i++){
        free(edge_matrix[i]);
        free(conflict_matrix[i]);
    }
    free(edge_matrix);
    free(conflict_matrix);


return 0;
    
}
