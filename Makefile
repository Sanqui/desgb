all: des.gb

gfx/tiles.2bpp: gfx/tiles.png
	rgbgfx -o gfx/tiles.2bpp gfx/tiles.png

main.o: main.asm vram.asm des.asm constants.asm gfx/tiles.2bpp
	rgbasm -E -h -o $@ $<

des.gb: main.o
	rgblink -n des.sym -o $@ $<
	rgbfix -jv -i XXXX -k XX -l 0x33 -m 0x01 -p 0 -r 0 -t des $@

clean:
	rm -f main.o des.gb des.sym gfx/tiles.2bpp
