#include <string.h>

typedef struct Opaque Opaque;
typedef struct Type Type;
typedef struct ValueWitnessTable ValueWitnessTable;

struct Type {
    int size;
    int align;
    int stride;
    ValueWitnessTable* vwt;
};

struct ValueWitnessTable {
    void (*copy)(Opaque* dst, Opaque* src, Type* t);
    void (*move)(Opaque* dst, Opaque* src, Type* t);
    void (*drop)(Opaque* val, Type* t);
};

void identity(Opaque* ret, Opaque* x, Type* t) {
    t->vwt->move(ret, x, t);
}

void BASIC_COPY(Opaque* dst, Opaque* src, Type* t) {
    memcpy(dst, src, t->size);
}

void BASIC_MOVE(Opaque* dst, Opaque* src, Type* t) {
    memcpy(dst, src, t->size);
}

void BASIC_DROP(Opaque* val, Type* t) {}

static ValueWitnessTable BASIC_VAULE_WITNESS_TABLE = {
    .copy = BASIC_COPY,
    .move = BASIC_MOVE,
    .drop = BASIC_DROP,
};

static Type INT_TYPE = {
    .size = 4,
    .align = 4,
    .stride = 4,
    .vwt = &BASIC_VAULE_WITNESS_TABLE,
};

int main() {
    int ret;
    int x = 22;

    identity((Opaque*)&ret, (Opaque*)&x, &INT_TYPE);

    return ret;
}
