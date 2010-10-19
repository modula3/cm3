/*
This is a wrapper for \cygwin\bin\dos2unix.exe that doesn't require changing %PATH%.
Put it in some directory already in %PATH%, such as %WinDir% or %WinDir%\system32.

Build it with:
    cl -Ox dos2unix.c -link -entry:Entry -subsystem:console kernel32.lib -merge:.rdata=.text -opt:ref
*/

#include <windows.h>

/* These are globals just to sleazily avoid -GS and memset dependencies. */

CHAR Executable[] = "C:\\cygwin\\bin\\dos2unix.exe";
CHAR SystemDrive[3];
STARTUPINFO StartInfo = {sizeof(StartInfo)};
PROCESS_INFORMATION ProcessInfo;

void Entry(void)
{
    DWORD ExitCode;
	
    GetEnvironmentVariable("SystemDrive", SystemDrive, 3);
    if (SystemDrive[0])
        Executable[0] = SystemDrive[0];

    ExitCode = CreateProcess(Executable, GetCommandLine(), NULL, NULL, FALSE, 0, NULL, NULL, &StartInfo, &ProcessInfo);
    if (ExitCode == FALSE)
    {
        ExitCode = GetLastError();
    }
    else
    {
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        GetExitCodeProcess(ProcessInfo.hProcess, &ExitCode);
    }
    ExitProcess(ExitCode);
}
