all: ..\NT386\m3processstubcon.exe ..\NT386\m3processstubwin.exe

..\NT386\M3ProcessStubCon.obj: M3ProcessStub.c
	cl -c -Zi M3ProcessStub.c -Fo..\NT386\M3ProcessStubCon.obj

..\NT386\M3ProcessStubWin.obj: M3ProcessStub.c
	cl -c -Zi -DM3STUBGUI M3ProcessStub.c -Fo..\NT386\M3ProcessStubWin.obj

..\NT386\m3processstubcon.exe: ..\NT386\M3ProcessStubCon.obj
	link32 -machine:i386 -base:0x10000 -subsystem:console -out:..\NT386\m3processstubcon.exe ..\NT386\M3ProcessStubCon.obj crtdll.lib kernel32.lib user32.lib gdi32.lib comdlg32.lib winspool.lib -ignore:505

..\NT386\m3processstubwin.exe: ..\NT386\M3ProcessStubWin.obj
	link32 -machine:i386 -base:0x10000 -subsystem:windows -out:..\NT386\m3processstubwin.exe ..\NT386\M3ProcessStubWin.obj crtdll.lib kernel32.lib user32.lib gdi32.lib comdlg32.lib winspool.lib -ignore:505
	