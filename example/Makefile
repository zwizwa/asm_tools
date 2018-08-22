test: testbench.elf
clean:
	rm -rf *.elf
%.elf: %.v
	iverilog $< -o $@
