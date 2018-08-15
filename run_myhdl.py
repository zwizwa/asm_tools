# Wrapper for running MyHDL code derived from Seq

# This will generate .v and .vhd code for logic syntheses, and
# possibly run a Simulation to compare against an "output" list.  For
# more complicated synthesis setup and Python test benches, a custom
# script is more appropriate.

import sys
import imp
import inspect
import importlib.util
from myhdl import *
import lib

def load_module(hdl_module_name, filename):
    # Load the test bench module
    spec  = importlib.util.spec_from_file_location(hdl_module_name, filename)
    modul = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(modul)
    return modul

def interpret_module(hdl_module_name, modul):
    # Python function to instantiate HDL module
    hdl_fun = getattr(modul, hdl_module_name)
    ports = getattr(modul, "ports")
    #print(ports)
    ins = False
    outs = False
    if hasattr(modul, "outs"):
        outs = modul.outs
    if hasattr(modul, "ins"):
        ins = modul.ins

    return hdl_fun, ports, ins, outs


def inst_testbench(hdl_fun, ports, tb_input, tb_output):

    nb_in = len(tb_input[0])
    #print("nb_in", nb_in)

    CLK = Signal(bool(False))
    RST = ResetSignal(1,0,True)

    #RST = ResetSignal(0,1,True)

    nb_input  = len(tb_input[0])
    if tb_output:
        nb_output = len(tb_output[0])

    # Inputs are assumed to be 1-bit signals.  We model them as
    # registers, so first input vector determines reset values.
    in_signals  = [Signal(modbv(v)[t:]) for (v,(_,t)) in zip(tb_input[0],ports)]
    out_signals = [Signal(modbv(0)[t:]) for (_,t) in ports[nb_input:]]

    io_signals = in_signals + out_signals
    signals = [CLK, RST] + io_signals

    tb_inst = traceSignals(hdl_fun, *signals)
    
    # Generate main clock
    def clock():
        for _ in range(1000):
            # Wait first, so we see the reset value of the registers
            # separately from the effect of the first clock edge.
            yield(delay(10))
            CLK.next = not CLK

    # Connect inputs, verify outputs, print trace
    n = Signal(int(0))
    @always_seq(CLK.posedge, reset=RST)
    def io():
        # Keep track of time.
        n.next = n + 1

        if tb_output:
            # Perform output assert.
            print(n,tb_input[n],tb_output[n])

            for (a,b) in zip(out_signals, tb_output[n]):
                assert a == b

        else:
            # Just run the entire simulation.
            vals = [s + 0 for s in out_signals]
            print(vals)

        if n+1 >= len(tb_input):
            raise StopSimulation

        # tb_input[0] is set at reset.  By induction this needs n+1
        for (a,b) in zip(in_signals, tb_input[n+1]):
            a.next = b


    return [tb_inst, clock(), io]


def run_module(hdl_module_name, modul):

    hdl_fun, ports, tb_input, tb_output = interpret_module(hdl_module_name, modul)

    if tb_input:
        # Run it if it is a test bench
        insts = inst_testbench(hdl_fun, ports, tb_input, tb_output)
        Simulation(insts).run()

    else:
        # Otherwise synthesize code
        
        # Which are special cases.
        CLK = Signal(bool(False))

        # FIXME: workaround for HX8K board
        RST = ResetSignal(1,0,True)
        # RST = ResetSignal(0,1,True)

        port_signals = [Signal(modbv(0)[bits:]) for (_,bits) in ports]
        signals = [CLK, RST] + port_signals

        # Generate code
        toVerilog(hdl_fun, *signals)
        toVHDL(hdl_fun, *signals)

# Invoked as library call from MyHDL.hs
def run_text(mod_name, mod_text):
    modul = imp.new_module(mod_name)
    exec(mod_text, modul.__dict__)
    return run_module(mod_name, modul)



# Invoked as script.  See Makefile
if __name__ == '__main__':
    if sys.argv[2]:
        hdl_module_name = sys.argv[1]
        modul = load_module(hdl_module_name, sys.argv[2])
        run_module(hdl_module_name, modul)
