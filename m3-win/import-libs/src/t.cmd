
del t.obj t.o a.dll b.dll a.exe b.exe *.pdb 2>nul

if exist ..\nt386 (
    mkdir ..\nt386gnu 2>nul
    copy /y ..\nt386\*.lib ..\nt386gnu
)
if exist ..\nt386gnu (
    mkdir ..\nt386 2>nul
    copy /y ..\nt386gnu\*.lib ..\nt386
)

gcc -c t.c
ld t.o -o a.dll -lc -trace ../NT386GNU/user32.lib ../NT386GNU/winspool.lib ../NT386GNU/kernel32.lib -dll
link -dump -imports a.dll

cl -c -Zl -Zi t.c
link t.obj -debug -out:b.exe -entry:Entry ../NT386GNU/user32.lib ../NT386GNU/winspool.lib ../NT386GNU/kernel32.lib -incremental:no -opt:ref -subsystem:console
link -dump -imports b.exe
