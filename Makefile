#gawk sort order
export LC_CTYPE=C

.SUFFIXES: .asm .o .gbc

all: des.gb

main.o: main.asm vram.asm des.asm
	rgbasm -E -h -o $@ $<

des.gb: main.o
	rgblink -n des.sym -o $@ $<
	rgbfix -jv -i XXXX -k XX -l 0x33 -m 0x01 -p 0 -r 0 -t des $@

clean:
	rm -f main.o des.gb des.sym
