
#include <stdio.h>
#include <stdlib.h>

typedef struct student {
    int id;
    int project_grade;
    int final_exam_grade;
    float final_course_mark;
} student;

student** create_class_list(char* filename, int* size) {
    student** ret;
    int i;

    FILE* fin = fopen(filename, "r");

    fscanf(fin, "%d", size);

    ret = malloc(*size * sizeof(student*));

    for (i = 0; i < *size; i++) {
        ret[i] = calloc(1, sizeof(student));
        fscanf(fin, "%d", &ret[i]->id);
    }

    fclose(fin);
    return ret;
}

int find_linsrch(int id, student** list, int size) {
    int i, ret = -1;

    for (i = 0; i < size; i++) {
        if (list[i]->id == id) {
            ret = i;
            break;
        }
    }

    return ret;
}

int find_binsrch(int id, student** list, int size) {
    int a = 0, b = size - 1, c;

    while (b >= a) {
        c = (a + b)/2;

        if (list[c]->id == id) {
            return c;
        }
        else if (list[c]->id < id) {
            a = c + 1;
        }
        else {
            b = c - 1;
        }
    }

    return -1;
}

void input_grades(char* filename, student** list, int size) {
    int i;
    int index, id, project_grade, final_exam_grade;
    student* s;
    FILE* fin = fopen(filename, "r");

    for (i = 0; i < size; i++) {
        fscanf(fin, "%d %d %d", &id, &project_grade, &final_exam_grade);
        index = find_linsrch(id, list, size);
        if (index == -1) continue;

        s = list[index];
        s->project_grade = project_grade;
        s->final_exam_grade = final_exam_grade;
    }

    fclose(fin);
}

void compute_final_course_marks(student** list, int size) {
    int i;
    student* s;

    for (i = 0; i < size; i++) {
        s = list[i];
        s->final_course_mark = s->final_exam_grade * 0.6 \
                             + s->project_grade * 0.4;
    }
}

void output_final_course_marks(char* filename, student** list, int n) {
    int i;
    FILE* fout = fopen(filename, "w");

    for (i = 0; i < n; i++) {
        fprintf(fout, "%d %.2f\n",
                list[i]->id,
                list[i]->final_course_mark);
    }

    fclose(fout);
}

void print_backwards(student** list, int size) {
    if (size) {
        printf("| Student ID: %d | Final Mark: %.2f |\n",
                list[size-1]->id,
                list[size-1]->final_course_mark);
        print_backwards(list, size - 1);
    }
}

void withdraw(int id, student** list, int* size) {
    int i, index;

    index = find_binsrch(id, list, *size);

    if (index == -1) {
        printf("Error in withdrawing student %d:\n"
               " |-> Student %d is not in the class\n", id, id);
    }

    else {
        free(list[index]);

        for (i = index; i < *size - 1; i++) {
            list[i] = list[i+1];
        }

        (*size)--;
    }
}

void destroy_list(student** list, int* size) {
    int i;

    for (i = 0; i < *size; i++) {
        free(list[i]);
    }
    free(list);
    *size = 0;
}

int main(void) {
    int n;
    student** class_list;
    class_list = create_class_list("classlist.txt", &n);
    input_grades("grades.txt", class_list, n);

    compute_final_course_marks(class_list, n);

    output_final_course_marks("outfile.txt", class_list, n);

    withdraw(1, class_list, &n);
    withdraw(1305001, class_list, &n);
    withdraw(1305004, class_list, &n);

    print_backwards(class_list, n);

    destroy_list(class_list, &n);

    return 0;
}

