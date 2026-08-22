all: test

itsy-linux: itsy-linux.asm
	nasm itsy-linux.asm -fbin -l itsy-linux.lst -o itsy-linux
	chmod +x itsy-linux
clean:
	rm itsy-linux.lst itsy-linux
test: itsy-linux
	cat extend.f tester-min.f | ./itsy-linux
