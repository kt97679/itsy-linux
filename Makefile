all: itsy-linux

itsy-linux: itsy-linux.asm
	nasm itsy-linux.asm -fbin -l itsy-linux.lst -o itsy-linux
	chmod +x itsy-linux
clean:
	rm itsy-linux.lst itsy-linux
