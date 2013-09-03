itsy-linux
==========

Linux port of itsy forth by John Metcalf

John Metcalf (http://www.retroprogramming.com/) implemented minimal forth system:

http://www.retroprogramming.com/2012/03/itsy-forth-1k-tiny-compiler.html
http://www.retroprogramming.com/2012/04/itsy-forth-dictionary-and-inner.html
http://www.retroprogramming.com/2012/04/itsy-forth-primitives.html
http://www.retroprogramming.com/2012/06/itsy-forth-compiler.html
http://www.retroprogramming.com/2012/09/itsy-documenting-bit-twiddling-voodoo.html

Resulting binary has very impressive size (978 bytes) and can be used to bootstrap complete forth system.

Original itsy code is producing .com files and can be found in "msdos" directory of this repository.

I ported it to 32-bit linux code. Sample session:

$ make
nasm itsy-linux.asm -fbin -l itsy-linux.lst -o itsy-linux
chmod +x itsy-linux
$ ./itsy-linux 
: say_hi 72 emit 105 emit 33 emit 10 emit ;
say_hi
Hi!
^C
$ 

