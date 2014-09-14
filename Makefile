.PHONY: all clean
all: pru.elf

clean:
	rm -rf *.elf *~

%.elf: %.c
	gcc -o $@ $<
