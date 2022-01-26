nasm -f win64 -o out/out.obj out/out.asm
link out\out.obj /subsystem:console /out:hello.exe legacy_stdio_definitions.lib msvcrt.lib
