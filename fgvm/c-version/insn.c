#include <priv.h>

insn_table *insn_table_copy(insn_table *tbl) {
    insn_table *ret;
    size_t insns_size = sizeof(insn) * tbl->ninsns;

    ret = (insn_table *)malloc(sizeof(insn_table));
    ret->ninsns = tbl->ninsns;
    ret->insns = (insn *)malloc(insns_size);
    memcpy(ret->insns, tbl->insns, insns_size);

    return ret;
}

insn_table *insn_table_join(insn_table *dest, insn_table *src) {
    insn *new_insns;
    unsigned new_ninsns   = dest->ninsns + src->ninsns;
    size_t new_insns_size = sizeof(insn) * new_ninsns;
    size_t src_insns_size = sizeof(insn) * src->ninsns;

    if ((new_insns = (insn *)realloc(dest->insns, new_insns_size)) == NULL) {
        return NULL;
    }

    dest->insns = new_insns;
    memcpy(dest->insns + dest->ninsns, src->insns, src_insns_size);
    dest->ninsns = new_ninsns;
    return dest;
}

static void execute_embedded_insn(fgvm *vm, fgvm_packet *p) {
    void *result = ((void *(*)(fgvm *, fgvm_packet *))vm->vminsn_table->insns[p->insn_id].body)(vm, p);
    fgvm_packet *waiting = p->waiting;
    byte arg_index = p->arg_index;

    if (waiting != NULL) {
        waiting->args[arg_index] = result;

        pthread_mutex_lock(&(waiting->mutex_write));
        byte nwaitings = --(waiting->nwaitings);
        pthread_mutex_unlock(&(waiting->mutex_write));

        if (nwaitings == 0) {
            fgvm_set_task(vm, waiting);
        }
    }
}

static void extract_group_insn(fgvm *vm, fgvm_packet *p) {
    void *ginsn       = vm->vminsn_table->insns[p->insn_id].body;
    unsigned npackets = READBLOCK(ginsn, unsigned);
    byte nargs        = READBLOCK(ginsn, byte);

    if (nargs != p->nargs) {
        return;
    }

    ///
    /// Construct packets
    ///
    void *packet_templ    = ginsn;
    fgvm_packet **packets =
        (fgvm_packet **)malloc(sizeof(fgvm_packet *) * npackets);
    unsigned *packets_index_rel =
        (unsigned *)malloc(sizeof(unsigned) * npackets);

    for (unsigned i = 0; i < npackets; i++) {
        packets_index_rel[i] = READBLOCK(packet_templ, unsigned);
        byte arg_index       = READBLOCK(packet_templ, byte);
        byte nwaitings       = READBLOCK(packet_templ, byte);
        unsigned insn_id     = READBLOCK(packet_templ, unsigned);
        byte packet_nargs    = READBLOCK(packet_templ, byte);
        void **packet_args   = (void **)malloc(sizeof(void *) * packet_nargs);

        for (byte j = 0; j < packet_nargs; j++) {
            packet_args[j] = READBLOCK(packet_templ, void *);
        }

        packets[i] = fgvm_packet_create(
            NULL
            , arg_index
            , nwaitings
            , insn_id
            , packet_nargs
            , packet_args
            );
    }

    for (unsigned i = 0; i < npackets; i++) {
        packets[i]->waiting = packets[packets_index_rel[i]];
    }

    ///
    /// Rewrite packets
    ///
    void *rewrite_info = packet_templ;

    // Set the address of args
    for (byte i = 0; i < nargs; i++) {
        unsigned nrewrites = READBLOCK(rewrite_info, unsigned);

        for (unsigned j = 0; j < nrewrites; j++) {
            unsigned packet_index = READBLOCK(rewrite_info, unsigned);
            byte arg_index        = READBLOCK(rewrite_info, byte);
            packets[packet_index]->args[arg_index] = p->args[i];
        }
    }

    // Set the address of return value
    do {
        unsigned packet_index = READBLOCK(rewrite_info, unsigned);
        packets[packet_index]->waiting   = p->waiting;
        packets[packet_index]->arg_index = p->arg_index;
    } while (0);

    // if
    if (packets[0]->insn_id == 0) {
        packets[0]->args[1] = packets[2];
        packets[0]->args[2] = packets[3];
    }

    ///
    /// Set packets
    ///
    for (unsigned i = 0; i < npackets; i++) {
        if (packets[i]->nwaitings == 0) {
            fgvm_set_task(vm, packets[i]);
        }
    }

    free(packets);
    free(packets_index_rel);
}

void packet_execute(fgvm *vm, fgvm_packet *p) {
    unsigned insn_id = p->insn_id;

    if (insn_id < vm->vminsn_table->ninsns) {
        switch (vm->vminsn_table->insns[insn_id].type) {
        case 0:
            execute_embedded_insn(vm, p);
            break;
        case 1:
            extract_group_insn(vm, p);
            break;
        }
    }
    fgvm_packet_free(p);
}
