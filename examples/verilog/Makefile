VPI := cosim.vpi

DEPS := Makefile

cosim: testbench.run
clean:
	rm -rf *.vvp *.vpi *.o
%.vvp: %.v $(DEPS)
	iverilog $< -o $@
%.run: %.vvp $(VPI) $(DEPS)
	vvp -m./cosim $<

%.o: %.c $(DEPS)
	gcc -Wall -Werror -I/usr/include/iverilog -fpic -c $< -o $@
%.vpi: %.o $(DEPS)
	gcc -shared -o $@ $< -lvpi

.SECONDARY:
