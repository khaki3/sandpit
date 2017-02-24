#ifndef FLOWVM_H
#define FLOWVM_H

#include <pthread.h>
#include <stdbool.h>
#include <fgvm/queue.h>

typedef unsigned char byte;

typedef struct {
    // type0: embedded-insn
    // type1: group-insn
    byte type : 1;

    // type0: pointer to the function of the embedded-insn
    // type1: pointer to group-insn
    void *body;
} insn;

typedef struct {
    unsigned ninsns;
    insn *insns;
} insn_table;

typedef struct {
    ///
    /// vm core-data
    ///
    insn_table *vminsn_table;
    queue *tasks;
    byte nthreads;
    pthread_t *threads;
    byte nrunning;

    ///
    /// for thread sleeping
    ///
    pthread_cond_t cond_tasks_is_empty;
    pthread_mutex_t mutex_tasks_is_empty;
    byte stop : 1;
} fgvm;

typedef struct fgvm_packet_rec {
    ///
    /// the destination of the dataflow
    ///
    struct fgvm_packet_rec *waiting;
    byte arg_index;

    ///
    /// statuses while waiting
    ///
    pthread_mutex_t mutex_write;
    byte nwaitings;

    ///
    /// program-body
    ///
    unsigned insn_id;
    byte nargs;
    void **args;
} fgvm_packet;

fgvm_packet *fgvm_packet_create(fgvm_packet *, byte, byte, unsigned, byte, void **);
void fgvm_packet_free(fgvm_packet *);

fgvm *fgvm_create();
void fgvm_load_group_insn(fgvm *, char *);
void fgvm_set_task(fgvm *, fgvm_packet *);
void fgvm_set_task_multiple(fgvm *, fgvm_packet *[]);
int fgvm_execute(fgvm *, byte);
int fgvm_execute_auto(fgvm *);
void fgvm_wait(fgvm *);
void fgvm_exit(fgvm *);

#endif
