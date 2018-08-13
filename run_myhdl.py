# Wrapper for running MyHDL code derived from Seq

# This will generate .v and .vhd code for logic syntheses, and
# possibly run a Simulation to compare against an "output" list.  For
# more complicated synthesis setup and Python test benches, a custom
# script is more appropriate.
 
import sys
import inspect
import importlib.util
from myhdl import *
import ram

def load_module(hdl_fun_name, filename):
    # Load the test bench module
    spec  = importlib.util.spec_from_file_location(hdl_fun_name, filename)
    modul = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(modul)
    # Python function to instantiate HDL module
    hdl_fun = getattr(modul, hdl_fun_name)
    ports = inspect.getargspec(hdl_fun).args
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

    # Inputs are assumed to be 1-bit signals.  We model them as
    # registers, so first input vector determines reset values.
    in_signals  = [Signal(modbv(v)[1:]) for v in tb_input[0]]
    out_signals = [Signal(modbv(0)[1:]) for _ in tb_output[0]]

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

        print(n,tb_input[n],tb_output[n])

        for (a,b) in zip(out_signals, tb_output[n]):
            assert a == b

        if n+1 >= len(tb_output):
            raise StopSimulation

        # tb_input[0] is set at reset.  By induction this needs n+1
        for (a,b) in zip(in_signals, tb_input[n+1]):
            a.next = b


    return [tb_inst, clock(), io]


def load_and_run(hdl_fun_name, filename):

    hdl_fun, ports, tb_input, tb_output = load_module(hdl_fun_name, filename)

    # Run it if it is a test bench
    if tb_input and tb_output:
        insts = inst_testbench(hdl_fun, ports, tb_input, tb_output)
        Simulation(insts).run()
    else:
        print("not a testbench")
        
    # The first two arguments are CLK and RST.
    out_ports = ports[2:]
    # Which are special cases.
    CLK = Signal(bool(False))

    # FIXME: workaround for HX8K board
    #RST = ResetSignal(1,0,True)
    RST = ResetSignal(0,1,True)
    # For FPGA output, we assume 1-bit signals.
    out_signals = [Signal(modbv(0)[1:]) for _ in out_ports]
    signals = [CLK, RST] + out_signals

    # Generate code
    toVerilog(hdl_fun, *signals)
    toVHDL(hdl_fun, *signals)


if __name__ == '__main__':
    if sys.argv[2]:
        load_and_run(sys.argv[1], sys.argv[2])
