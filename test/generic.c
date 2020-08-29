#include <stddef.h>
#include <stdio.h>

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
    .size = 0,
    .align = 1,
    .stride = 1,
};

void vec_new(const struct TypeLayout* T_type_layout, struct Vec* self) {
    self->ptr = NULL;
    self->len = 0;
    self->cap = T_type_layout->size == 0 ? ~0 : 0;
}

int main() {
    struct Vec vector;
    vec_new(&bool_type_layout, &vector);

    return 0;
}
