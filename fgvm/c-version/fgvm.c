#include <priv.h>

void wake_up_threads(fgvm *vm) {
    pthread_mutex_lock(&(vm->mutex_tasks_is_empty));
    pthread_cond_broadcast(&(vm->cond_tasks_is_empty));
    pthread_mutex_unlock(&(vm->mutex_tasks_is_empty));
}

///
/// When this function returns NULL, threads must stop the execution.
///
fgvm_packet *fgvm_get_task(fgvm *vm) {
    fgvm_packet *ret;

    ret = (fgvm_packet *)queue_pop(vm->tasks);
    if (ret != NULL) {
        return ret;
    }

    // sleep until tasks have added
    pthread_mutex_lock(&(vm->mutex_tasks_is_empty));
    if ((--(vm->nrunning)) == 0) {
        pthread_cond_broadcast(&(vm->cond_tasks_is_empty));
    }

    while ((ret = (fgvm_packet *)queue_pop(vm->tasks)) == NULL && !(vm->stop)) {
        pthread_cond_wait(&(vm->cond_tasks_is_empty), &(vm->mutex_tasks_is_empty));
    }

    vm->nrunning++;
    pthread_mutex_unlock(&(vm->mutex_tasks_is_empty));

    return ret;
}

void fgvm_set_task(fgvm *vm, fgvm_packet *p) {
    bool queue_was_empty = queue_is_empty(vm->tasks);
    queue_push(vm->tasks, (void *)p);
    if (queue_was_empty) wake_up_threads(vm);
}

void fgvm_set_task_multiple(fgvm *vm, fgvm_packet *packets[]) {
    bool queue_was_empty = queue_is_empty(vm->tasks);
    queue_push_multiple(vm->tasks, (void **)packets);
    if (queue_was_empty) wake_up_threads(vm);
}

fgvm_packet *fgvm_packet_create(
    fgvm_packet *waiting, byte arg_index,
    byte nwaitings, unsigned insn_id, byte nargs, void **args) {
    fgvm_packet *ret = (fgvm_packet *)malloc(sizeof(fgvm_packet));

    ret->waiting   = waiting;
    ret->arg_index = arg_index;

    pthread_mutex_init(&(ret->mutex_write), NULL);
    ret->nwaitings = nwaitings;

    ret->insn_id = insn_id;
    ret->nargs   = nargs;
    ret->args    = args;

    return ret;
}

void fgvm_packet_free(fgvm_packet *p) {
    pthread_mutex_destroy(&(p->mutex_write));
    free(p->args);
    free(p);
}

fgvm *fgvm_create() {
    fgvm *vm = (fgvm *)malloc(sizeof(fgvm));

    vm->vminsn_table = insn_table_copy(&embinsn_table);
    vm->tasks        = queue_create();
    vm->stop         = false;
    pthread_cond_init(&(vm->cond_tasks_is_empty), NULL);
    pthread_mutex_init(&(vm->mutex_tasks_is_empty), NULL);

    return vm;
}

static void work(fgvm *vm) {
    fgvm_packet *p;
    while ((p = fgvm_get_task(vm)) != NULL) {
        packet_execute(vm, p);
    }
}

static void *thread_routine(void *vmptr) {
    work((fgvm *)vmptr);
    return NULL;
}

int fgvm_execute(fgvm *vm, byte nthreads) {
    vm->nthreads = nthreads;
    vm->threads  = (pthread_t *)malloc(sizeof(pthread_t) * nthreads);
    vm->nrunning = nthreads;

    for (int i = 0; i < nthreads; i++) {
        int status = pthread_create(&(vm->threads[i]), NULL, thread_routine, (void *)vm);
        if (status != 0) {
            return status;
        }
    }
    return 0;
}

int fgvm_execute_auto(fgvm *vm) {
    return fgvm_execute(vm, sysconf(_SC_NPROCESSORS_CONF));
}

void fgvm_wait(fgvm *vm) {
    pthread_mutex_lock(&(vm->mutex_tasks_is_empty));

    while (vm->nrunning != 0) {
        pthread_cond_wait(&(vm->cond_tasks_is_empty), &(vm->mutex_tasks_is_empty));
    }

    vm->stop = true;
    pthread_cond_broadcast(&(vm->cond_tasks_is_empty));
    pthread_mutex_unlock(&(vm->mutex_tasks_is_empty));

    for (int i = 0; i < vm->nthreads; i++) {
        pthread_join(vm->threads[i], NULL);
    }

    free(vm->threads);
    vm->threads = NULL;
    vm->stop = false;
}

void fgvm_exit(fgvm *vm) {
    if (vm->threads != NULL) {
        for (int i =0; i < vm->nthreads; i++) {
            pthread_cancel(vm->threads[i]);
        }
        free(vm->threads);
    }
    queue_free(vm->tasks);
    free(vm);
}
