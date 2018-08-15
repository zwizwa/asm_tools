from myhdl import *

def ram(write_clock, write_addr, write_data, write_enable,
        read_clock,   read_addr,  read_data):
    """ 
    Random access memory (RAM) with single read 
    and single write port
    """
    assert len(write_data) == len(read_data)
    assert len(write_addr) == len(read_addr)
    dw = len(write_data)
    na = 2**len(write_addr)
    memory = [Signal(intbv(0)[len(write_data):]) for _ in range(na)]
    
    @always(write_clock.posedge)
    def rtlwr():
        if write_enable:
            memory[write_addr].next = write_data

    @always(read_clock.posedge)
    def rtlrd():
        read_data.next = memory[read_addr]
            
    return instances()


# https://github.com/YosysHQ/yosys/issues/103
# The lattice chips don't have support for initial values on registers.
# Use a reset generator.

# FIXME: how to express reduction and in MyHDL?

def ice40_reset(CLK, RST):
    reset_count = Signal(modbv(0)[8:])
    @always_comb
    def i1():
        RST.next = reset_count == 0xff
    @always_seq(CLK.posedge, reset=None)
    def i2():
        if not RST:
            reset_count.next = reset_count + 1
    return [i1,i2]


# reg [7:0] resetn_counter = 0;
# assign resetn = &resetn_counter;
# always @(posedge clk) begin
#     if (!resetn)
#         resetn_counter <= resetn_counter + 1;
# end

