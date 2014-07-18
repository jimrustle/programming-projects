#include <stdio.h>
#include <stdlib.h>

typedef struct node {
    int value;
    struct node* next;
} node;

node* create_list() {
    node* start = malloc(sizeof(node));
    start->value = 0;
    start->next = NULL;

    return start;
}

node* inc_list_len(node* n) {
    while (n->next != NULL){
        n = n->next;
    }

    node* new = create_list();
    new->value = 0;
    new->next = NULL;
    n->next = new;

    return new;
}

int length(node* l) {
    int count = 1;

    while (l->next != NULL){
        l = l->next;
        count++;
    }

    return count;
}

void _free_list(node * current, node* next){
    if (next != NULL){
        free(current);
        _free_list(next, next->next);
    }
    else {
        free(current);
    }
}

void free_list(node* l){
    _free_list(l, l->next);
}

void fill_list_w_array(int arr[], node* list){
    int i = 0;
    list->value = arr[i];
    while (list->next != NULL){
        i++;
        list = list->next;
        list->value = arr[i];
    }
}

void print_list(node* n) {
    printf("[%d]->", n->value);
    while (n->next != NULL){
        n = n->next;
        printf("[%d]->", n->value);
    }
    printf("NIL\n");
}

int main() {
    node* a = create_list();
    a->value = 3;
    print_list(a);
    inc_list_len(a);
    print_list(a);
    inc_list_len(a);
    inc_list_len(a);
    inc_list_len(a);
    inc_list_len(a);
    print_list(a);
    printf("Length: %d\n", length(a));
    fill_list_w_array((int[]){1,2,3,4,5,6}, a);
    print_list(a);
    free_list(a);
    return 0;
}
