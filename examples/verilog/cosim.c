# include "vpi_user.h"

// http://iverilog.wikia.com/wiki/Using_VPI
// https://en.wikipedia.org/wiki/Verilog_Procedural_Interface

// val = 41;
// $increment(val);
// $display("After $increment, val=%d", val);

// Implements the increment system task
static int increment(char *userdata) {
    vpiHandle systfref, args_iter, argh;
    struct t_vpi_value argval;
    int value;

    // Obtain a handle to the argument list
    systfref = vpi_handle(vpiSysTfCall, NULL);
    args_iter = vpi_iterate(vpiArgument, systfref);

    // Grab the value of the first argument
    argh = vpi_scan(args_iter);
    argval.format = vpiIntVal;
    vpi_get_value(argh, &argval);
    value = argval.value.integer;
    vpi_printf("VPI routine received %d\n", value);

    // Increment the value and put it back as first argument
    argval.value.integer = value + 1;
    vpi_put_value(argh, &argval, NULL, vpiNoDelay);

    // Cleanup and return
    vpi_free_object(args_iter);
    return 0;
}
// Registers the increment system task
void register_increment() {
    s_vpi_systf_data data = {vpiSysTask, 0, "$increment", increment, 0, 0, 0};
    vpi_register_systf(&data);
}

// Contains a zero-terminated list of functions that have to be called at startup
void (*vlog_startup_routines[])() = {
    register_increment,
    0
};
