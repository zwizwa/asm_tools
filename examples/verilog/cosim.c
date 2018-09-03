# include "vpi_user.h"

// Keep this simple for now: only sequential interface.  The bridge
// between Seq code and Icarus is registered.

// http://iverilog.wikia.com/wiki/Using_VPI
// https://en.wikipedia.org/wiki/Verilog_Procedural_Interface

// val = 41;
// $increment(val);
// $display("After $increment, val=%d", val);

// Implements the increment system task
static int increment(char *userdata) {
    vpiHandle systfref  = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle args_iter = vpi_iterate(vpiArgument, systfref); {
        vpiHandle reg   = vpi_scan(args_iter);
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(reg, &val);
        vpi_printf("VPI routine received %d\n", val.value.integer);
        val.value.integer += 1;
        vpi_put_value(reg, &val, NULL, vpiNoDelay);
    }
    vpi_free_object(args_iter);
    return 0;
}

static int to_seq(char *userdata) {
    // Registers driven from Seq
    vpiHandle systfref = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle iter = vpi_iterate(vpiArgument, systfref);
    vpiHandle reg;
    int count = 0;
    while (NULL != (reg = vpi_scan(iter))) {
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(reg, &val);
        vpi_printf("to_seq %d %d\n", count, val.value.integer);
        count++;
    }
    //vpi_free_object(iter); // this crashes
    return 0;
}
static int from_seq(char *userdata) {
    // Registers driven from Seq
    vpiHandle systfref = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle iter = vpi_iterate(vpiArgument, systfref);
    vpiHandle reg;
    int count = 0;
    while (NULL != (reg = vpi_scan(iter))) {
        struct t_vpi_value val = {.format = vpiIntVal };
        vpi_get_value(reg, &val);
        vpi_printf("from_seq %d %d\n", count, val.value.integer);
        count++;
    }
    //vpi_free_object(iter); // this crashes
    return 0;
}

// Registers the increment system task
#define TASK(...) if(1) { s_vpi_systf_data _data = {__VA_ARGS__}; vpi_register_systf(&_data); }
void setup_seq(void) {
    vpi_printf("Setting up Seq cosim.\n");
    TASK(vpiSysTask, 0, "$increment", increment, 0, 0, 0);
    TASK(vpiSysTask, 0, "$from_seq",  from_seq,  0, 0, 0);
    TASK(vpiSysTask, 0, "$to_seq",    to_seq,    0, 0, 0);
}

void startup_hello(void) {
}

// Contains a zero-terminated list of functions that have to be called at startup
void (*vlog_startup_routines[])() = {
    setup_seq,
    0
};
