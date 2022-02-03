./lib/nasm-2.15.05/nasm.exe -f win64 -o out/out.obj out/out.asm
link out\out.obj /debug /subsystem:console /out:hello.exe legacy_stdio_definitions.lib msvcrt.lib
