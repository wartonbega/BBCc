
.phony: 
	compile_compiler

a.out: output.o
	clang output.o -o a.out -e _main -m64 -arch x86_64

output.o: output.asm
	nasm -f macho64 output.asm -o output.o

output.asm: compile_compiler
	./zig-out/bin/bbc

compile_compiler: 
	zig build