#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "vpi_user.h"

// Keep this simple for now: only sequential interface implemented in
// terms of a $seq_tick method.

// Speed will likely depend on context switch time.  The question on
// our end is mostly: flush or not?  Practically, simulation driver
// will operate in chunks: read a bunch of data until a condition is
// met, then send the next driver signal.  Some options:

// - Make flush explicit.  E.g. let the simulation driver send out a
//   flush command, possibly through some 1-bit signal.
//
// - Perform non-blocking read, but if it would block, perform a
//   flush, then perform blocking read.

// The latter seems to have some issues.  Let's always flush, then
// make it a command later.


// http://iverilog.wikia.com/wiki/Using_VPI
// https://en.wikipedia.org/wiki/Verilog_Procedural_Interface

// val = 41;
// $increment(val);
// $display("After $increment, val=%d", val);

#define MAX_REGS 64

struct seq {
    int nb_to, nb_from;
    vpiHandle r_to[MAX_REGS], r_from[MAX_REGS];
    FILE *f_to, *f_from;
};

// Implements the increment system task
static int seq_tick(char *ctx) {
    struct seq *seq = (void*)ctx;

    if (seq->nb_to) {
        uint32_t to_vals[seq->nb_to];
        for (int i=0; i<seq->nb_to; i++) {
            struct t_vpi_value val = {.format = vpiIntVal };
            vpi_get_value(seq->r_to[i], &val);
            to_vals[i] = val.value.integer;
            //vpi_printf("to : %d=%d\n", i, val.value.integer);
        }
        fwrite(to_vals, sizeof(to_vals), 1, seq->f_to);
    }

    if (seq->nb_from) {
        uint32_t from_vals[seq->nb_from];
        if (1 != fread(from_vals, sizeof(from_vals), 1, seq->f_from)) {
            //vpi_printf("end of input\n"); // doesn't print
            vpi_control(vpiFinish, 0);
        }
        for (int i=0; i<seq->nb_from; i++) {
            struct t_vpi_value val = {.format = vpiIntVal };
            //vpi_get_value(seq->r_from[i], &val);
            //vpi_printf("from : %d=%d\n", i, val.value.integer);
            val.value.integer = from_vals[i];
            vpi_put_value(seq->r_from[i], &val, NULL, vpiNoDelay);
        }
    }
    return 0;
}

static int seq_to(char *ctx) {
    struct seq *seq = (void*)ctx;
    // Registers driven from Seq
    vpiHandle systfref = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle iter = vpi_iterate(vpiArgument, systfref);
    vpiHandle reg;
    while (NULL != (reg = vpi_scan(iter))) {
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(reg, &val);
        vpi_printf("seq_to %d %d\n", seq->nb_to, val.value.integer);
        seq->r_to[seq->nb_to++] = reg;
    }
    //vpi_free_object(iter); // this crashes
    return 0;
}
static int seq_from(char *ctx) {
    struct seq *seq = (void*)ctx;
    // Registers driven from Seq
    vpiHandle systfref = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle iter = vpi_iterate(vpiArgument, systfref);
    vpiHandle reg;
    while (NULL != (reg = vpi_scan(iter))) {
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(reg, &val);
        vpi_printf("seq_from %d %d\n", seq->nb_from, val.value.integer);
        seq->r_from[seq->nb_from++] = reg;
    }
    //vpi_free_object(iter); // this crashes
    return 0;
}

// Registers the increment system task
#define TASK(...) if(1) {\
        s_vpi_systf_data _data = {__VA_ARGS__}; \
        vpi_register_systf(&_data); \
}

char *check_getenv(char *var, char *dflt) {
    char *val = getenv(var);
    if (!val) {
        vpi_printf("WARNING: Environment variable '%s' is not defined\n", var);
        val=dflt;
    }
    vpi_printf("%s=%s\n", var, val);
    return val;
}

void setup_seq(void) {
    struct seq *seq = calloc(1,sizeof(*seq));
    vpi_printf("Setting up Seq cosim.\n");
    seq->f_to   = fopen(check_getenv("SEQ_TO",""), "a");
    seq->f_from = fopen(check_getenv("SEQ_FROM",""), "r");

    TASK(vpiSysTask, 0, "$seq_tick",  seq_tick,  0, 0, (char*)seq);
    TASK(vpiSysTask, 0, "$seq_from",  seq_from,  0, 0, (char*)seq);
    TASK(vpiSysTask, 0, "$seq_to",    seq_to,    0, 0, (char*)seq);
}

void startup_hello(void) {
}

// Contains a zero-terminated list of functions that have to be called at startup
void (*vlog_startup_routines[])() = {
    setup_seq,
    0
};
