VPI := cosim.vpi

test: testbench.run
clean:
	rm -rf *.vvp *.vpi *.o
%.vvp: %.v
	iverilog $< -o $@
%.run: %.vvp $(VPI)
	. ./vars.sh ; rm -f $$SEQ_TO ; vvp -m./cosim $<

%.o: %.c
	gcc -I/usr/include/iverilog -fpic -c $< -o $@
%.vpi: %.o
	gcc -shared -o $@ $< -lvpi

.SECONDARY:
