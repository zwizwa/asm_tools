DEPS := Makefile

all: cosim.vpi

clean:
	rm -rf *.vvp *.vpi *.o
%.o: %.c $(DEPS)
	gcc -Wall -Werror -I/usr/include/iverilog -fpic -c $< -o $@
%.vpi: %.o $(DEPS)
	gcc -shared -o $@ $< -lvpi

.SECONDARY:
