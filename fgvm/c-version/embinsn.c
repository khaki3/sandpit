#include <priv.h>

/*
 *  Embedded Instructions for a experimentaion
 */

#define DEFINSN(type, name) type name(fgvm *vm, fgvm_packet *p)
#define SETINSN(name) {0, (void *)name}
#define VAR(type, name, index)\
    type name  = *((type*)p->args[index])
#define RET(type, value)\
    type *name = malloc(sizeof(type)); *name = value; return name

DEFINSN(void *, _if) {
    int n, freed;
    VAR(bool, test, 0);

    if (test) {
        n = 1;
        freed = 2;
    } else {
        n = 2;
        freed = 1;
    }

    fgvm_packet *next = p->args[n];
    fgvm_packet_free(p->args[freed]);

    next->waiting   = p->waiting;
    next->arg_index = p->arg_index;
    p->waiting = NULL;

    fgvm_set_task(vm, (fgvm_packet *)next);
    return NULL;
}

DEFINSN(void *, _do) {
    fgvm_set_task(vm, (fgvm_packet *)(p->args[1]));
    return NULL;
}

DEFINSN(void *, set) {
    return p->args[0];
}

DEFINSN(int *, add) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(int, a + b);
}

DEFINSN(int *, sub) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(int, a - b);
}

DEFINSN(bool *, eq) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a == b);
}

DEFINSN(bool *, neq) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a != b);
}

DEFINSN(bool *, leq) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a <= b);
}

DEFINSN(bool *, geq) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a >= b);
}

DEFINSN(bool *, le) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a < b);
}

DEFINSN(bool *, ge) {
    VAR(int, a, 0);
    VAR(int, b, 1);
    RET(bool, a > b);
}

insn embinsns[] = {
    SETINSN(_if),
    SETINSN(_do),
    SETINSN(set),
    SETINSN(add),
    SETINSN(sub),
    SETINSN(eq),
    SETINSN(neq),
    SETINSN(leq),
    SETINSN(geq),
    SETINSN(le),
    SETINSN(ge),
};

insn_table embinsn_table = {
    sizeof(embinsns) / sizeof(insn), embinsns
};
