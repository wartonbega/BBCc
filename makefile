
.phony: 
	compile_compiler

SOURCE?=test.bbc

run_interpretor: compile_compiler
	./zig-out/bin/bbc $(SOURCE)

run_arm: a.out
	arch -x86_64 ./a.out || (echo "\noutput: $$?"; exit 0)

a.out: output.o
	clang output.o -o a.out -e global.main.wrapper -m64 -arch x86_64 -lc

output.o: output.asm
	nasm -f macho64 output.asm -o output.o -g

output.asm: compile_compiler
	./zig-out/bin/bbc $(SOURCE)

compile_compiler: 
	zig build