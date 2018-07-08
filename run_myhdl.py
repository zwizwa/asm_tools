# Wrapper for running MyHDL code derived from Seq

# This will generate .v and .vhd code for logic syntheses, and
# possibly run a Simulation to compare against an "output" list.  For
# more complicated synthesis setup and Python test benches, a custom
# script is more appropriate.
 
import sys
import inspect
import importlib.util
from myhdl import *

def make_out_signal():
    # This makes it possible to gerate Verilog
    return Signal(modbv(0)[32:])

def load_module(hdl_fun_name, filename):
    # Load the test bench module
    spec  = importlib.util.spec_from_file_location(hdl_fun_name, filename)
    modul = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(modul)
    # Python function to instantiate HDL module
    hdl_fun = getattr(modul, hdl_fun_name)
    ports = inspect.getargspec(hdl_fun).args
    print(ports)
    output = False
    if hasattr(modul, "output"):
        output = modul.output

    return hdl_fun, ports, output


def inst_testbench(hdl_fun, ports, tb_output):

    CLK = Signal(bool(False))
    RST = ResetSignal(1,0,True)

    # The first two arguments are CLK and RST.
    out_ports = ports[2:]

    # Is this a good idea?  Limit the size so .v and .vhdl can be generated.
    # Note that for FPGA output, we assume 1-bit signals.
    out_signals = [Signal(modbv(0)[32:]) for _ in out_ports]
    signals = [CLK, RST] + out_signals
    tb_inst = traceSignals(hdl_fun, *signals)

    # Generate main clock
    def clock():
        for _ in range(30):
            # Wait first, so we see the reset value of the registers
            # separately from the effect of the first clock edge.
            yield(delay(10))
            CLK.next = not CLK

    # Verify output
    n = Signal(int(0))
    @always_seq(CLK.posedge, reset=RST)
    def out_check():
        if n < len(tb_output):
            print (out_signals, tb_output[n])
            for (a,b) in zip(out_signals, tb_output[n]):
                assert a == b
        else:
            raise StopSimulation
        n.next = n + 1

    return [tb_inst, clock(), out_check]


def load_and_run(hdl_fun_name, filename):

    hdl_fun, ports, tb_output = load_module(hdl_fun_name, filename)

    # Run it if it is a test bench
    if tb_output:
        insts = inst_testbench(hdl_fun, ports, tb_output)
        Simulation(insts).run()
        
    # The first two arguments are CLK and RST.
    out_ports = ports[2:]
    # Which are special cases.
    CLK = Signal(bool(False))
    RST = ResetSignal(1,0,True)
    # For FPGA output, we assume 1-bit signals.
    out_signals = [Signal(modbv(0)[1:]) for _ in out_ports]
    signals = [CLK, RST] + out_signals

    # Generate code
    toVerilog(hdl_fun, *signals)
    toVHDL(hdl_fun, *signals)


if __name__ == '__main__':
    if sys.argv[2]:
        load_and_run(sys.argv[1], sys.argv[2])
