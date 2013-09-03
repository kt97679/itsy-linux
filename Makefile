all: itsy-linux

itsy-linux:
	nasm itsy-linux.asm -fbin -l itsy-linux.lst -o itsy-linux
	chmod +x itsy-linux
