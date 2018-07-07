# Wrapper for running generated MyHDL test benches.

# The idea is to keep things simple: this script will only "run" the
# test bench specified in the module, according to some simple ad-hoc
# interface.

# For final code generation, create a custum top-level MyHDL file.
 
import sys
import inspect
import importlib.util
from myhdl import *


def testbench(module_filename):

    # Load the test bench module
    tb_spec = importlib.util.spec_from_file_location("tb", module_filename)
    tb = importlib.util.module_from_spec(tb_spec)
    tb_spec.loader.exec_module(tb)
    print(tb.module)
    ports = inspect.getargspec(tb.module).args
    print(ports)

    # The first two arguments are CLK and RST.  The rest are all
    # assumed to be integer outputs.
    out_ports = ports[2:]
    CLK = Signal(int(0))
    RST = ResetSignal(1,0,True)
    out_signals = [Signal(int(0)) for p in out_ports]
    signals = [CLK, RST] + out_signals
    tb_inst = traceSignals(tb.module, *signals)

    # Stimulation: just main clock.
    def clock():
        for _ in range(30):
            # Wait first, so we see the reset value of the registers
            # separately from the effect of the first clock edge.
            yield(delay(10))
            CLK.next = not CLK


    insts = [tb_inst, clock()]

    # If there is an output list, instantiate a verifier.
    if hasattr(tb, "output"):
        n = Signal(int(0))
        @always_seq(CLK.posedge, reset=RST)
        def out_check():
            if n < len(tb.output):
                print (out_signals, tb.output[n])
                for (a,b) in zip(out_signals, tb.output[n]):
                    assert a == b
            else:
                raise StopSimulation
        insts += [out_check]

    # Otherwise just observe
    else:
        print("No 'output' attribute to verify against.")
        print("Printing output instead.")
        @always_seq(CLK.posedge, reset=RST)
        def out_print():
            print([s for s in out_signals])
        insts += [out_print]

    return insts

if __name__ == '__main__':
    if sys.argv[1]:
        Simulation(testbench(sys.argv[1])).run()
    else:
        print("need module argument")
        exit(1)
