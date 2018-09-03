#include <stdlib.h>
#include "vpi_user.h"

// Keep this simple for now: only sequential interface.  The bridge
// between Seq code and Icarus is registered.

// http://iverilog.wikia.com/wiki/Using_VPI
// https://en.wikipedia.org/wiki/Verilog_Procedural_Interface

// val = 41;
// $increment(val);
// $display("After $increment, val=%d", val);

#define MAX_REGS 64

struct seq {
    int nb_to, nb_from;
    vpiHandle to[MAX_REGS], from[MAX_REGS];
};

// Implements the increment system task
static int seq_tick(char *ctx) {
    struct seq *seq = (void*)ctx;
    for (int i=0; i<seq->nb_to; i++) {
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(seq->to[i], &val);
        vpi_printf("%d=%d\n", i, val.value.integer);
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
        seq->to[seq->nb_to++] = reg;
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
        seq->from[seq->nb_from++] = reg;
    }
    //vpi_free_object(iter); // this crashes
    return 0;
}

// Registers the increment system task
#define TASK(...) if(1) {\
        s_vpi_systf_data _data = {__VA_ARGS__}; \
        vpi_register_systf(&_data); \
}
void setup_seq(void) {
    struct seq *seq = calloc(1,sizeof(*seq));
    vpi_printf("Setting up Seq cosim.\n");
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
