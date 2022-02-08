nasm -f elf64 -o out/out.o out/out.asm &&
gcc -g -static out/out.o -o out/out