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

def load_module(hdl_modname, filename):
    # Load the test bench module
    tb_spec = importlib.util.spec_from_file_location("tb", filename)
    tb = importlib.util.module_from_spec(tb_spec)
    tb_spec.loader.exec_module(tb)
    hdl_mod = getattr(tb, hdl_modname)
    ports = inspect.getargspec(hdl_mod).args
    print(ports)

    return tb, hdl_mod, ports


def inst_testbench(hdl_mod, ports, tb_output):

    CLK = Signal(bool(False))
    RST = ResetSignal(1,0,True)

    # The first two arguments are CLK and RST.
    out_ports = ports[2:]

    # Is this a good idea?  Limit the size so .v and .vhdl can be generated.
    # Note that for FPGA output, we assume 1-bit signals.
    out_signals = [Signal(modbv(0)[32:]) for _ in out_ports]
    signals = [CLK, RST] + out_signals
    tb_inst = traceSignals(hdl_mod, *signals)

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

if __name__ == '__main__':
    if sys.argv[2]:
        hdl_modname = sys.argv[1]
        filename = sys.argv[2]

        tb, hdl_mod, ports = load_module(hdl_modname, filename)

        # Run it if it is a test bench
        if hasattr(tb, "output"):
            insts = inst_testbench(hdl_mod, ports, tb.output)
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
        toVerilog(hdl_mod, *signals)
        toVHDL(hdl_mod, *signals)

