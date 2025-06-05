
.phony: 
	compile_compiler

run_arm: a.out
	arch -x86_64 ./a.out || (echo "\noutput: $$?"; exit 0)

a.out: output.o
	clang output.o -o a.out -e main_wrapper -m64 -arch x86_64

output.o: output.asm
	nasm -f macho64 output.asm -o output.o -g

output.asm: compile_compiler
	./zig-out/bin/bbc

compile_compiler: 
	zig build