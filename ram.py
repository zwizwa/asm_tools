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
