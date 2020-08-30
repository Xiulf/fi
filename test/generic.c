#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct TypeLayout {
    size_t size;
    size_t align;
    size_t stride;
};

struct Vec {
    void* ptr;
    size_t cap;
    size_t len;
};

const struct TypeLayout bool_type_layout = {
    .size = 1,
    .align = 1,
    .stride = 1,
};

void vec_new(const struct TypeLayout* T_type_layout, struct Vec* self) {
    self->ptr = NULL;
    self->len = 0;
    self->cap = T_type_layout->size == 0 ? ~0 : 0;
}

void vec_push(const struct TypeLayout* T_type_layout, struct Vec* self, void* item) {
    if (self->ptr == NULL) {
        self->ptr = malloc(T_type_layout->stride);
        self->cap++;
    }

    if (self->len == self->cap) {
        self->cap *= 2;
        self->ptr = realloc(self->ptr, self->cap * T_type_layout->stride);
    }

    void* ptr = self->ptr + self->len * T_type_layout->stride;
    
    memcpy(ptr, item, T_type_layout->size);
    self->len++;
}

void identity(const struct TypeLayout* T_type_layout, void* x, void* ret) {
    memcpy(ret, x, T_type_layout->size);
}

void test(const struct TypeLayout* T_type_layout, void* x) {
    void* copy = malloc(T_type_layout->size);
    memcpy(copy, x, T_type_layout->size);

    // do whatever

    free(copy);
}

int main() {
    struct Vec vector;
    vec_new(&bool_type_layout, &vector);
    char item = 0;
    vec_push(&bool_type_layout, &vector, &item);
    char item2 = 1;
    vec_push(&bool_type_layout, &vector, &item2);
    char item3 = 1;
    vec_push(&bool_type_layout, &vector, &item3);

    printf("%ld", vector.len);

    return 0;
}
