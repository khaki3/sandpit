#ifndef PRIV_H
#define PRIV_H

#include <fgvm.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// util
#define READBLOCK(ptr, type)\
    *((type*)ptr);          \
    ptr = (type*)ptr + 1

// fgvm.c
void wake_up_threads(fgvm *);
fgvm_packet *fgvm_get_task(fgvm *);

// insn.c
insn_table *insn_table_copy(insn_table *);
insn_table *insn_table_join(insn_table *, insn_table *);
void packet_execute(fgvm *, fgvm_packet *);

// embinsn
insn_table embinsn_table;

#endif
