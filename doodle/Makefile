.phony: info all clean
all: doodle.elf 
	@echo "See Makefile"
	./doodle.elf

clean:
	rm -f *.elf *.hi *.o

doodle.elf: doodle.hs
	ghc $< -o $@
