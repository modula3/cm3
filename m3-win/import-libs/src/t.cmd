
del t.obj t.o a.dll b.dll a.exe b.exe 2>nul

gcc -c t.c
ld t.o -o a.dll -lc -trace ../NT386GNU/user32.lib ../NT386GNU/winspool.lib ../NT386GNU/kernel32.lib -dll
link -dump -imports a.dll

cl -c -Zl -Zi t.c
link t.obj -debug -out:b.exe -entry:Entry ../NT386GNU/user32.lib ../NT386GNU/winspool.lib ../NT386GNU/kernel32.lib -incremental:no -opt:ref -subsystem:console
link -dump -imports b.exe
