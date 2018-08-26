test: testbench.run
clean:
	rm -rf *.elf
%.elf: %.v
	iverilog $< -o $@
%.run: %.elf
	$$(readlink -f $<)

