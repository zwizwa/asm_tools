VPI := cosim.vpi

test: testbench.run
clean:
	rm -rf *.vvp *.vpi
%.vvp: %.v
	iverilog $< -o $@
%.run: %.vvp $(VPI)
	vvp -m./cosim $<

%.vpi: %.c Makefile
	gcc -I/usr/include/iverilog -shared -o $@ $<

.SECONDARY:
