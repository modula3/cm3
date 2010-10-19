/*
This is a wrapper for \cygwin\bin\cvs.exe that doesn't require changing %PATH%.
Put it in some directory already in %PATH%, such as %WinDir% or %WinDir%\system32.
As well, on the Elego Windows VM, \Program Files\CVSNT has been renamed to CVSNT.1
to get it out of %PATH%. It ruins line endings.
As well \SFU was mostly renamed to \SFU.1, to remove it as a possible factor, though
it turns out it was probably completely innocent.
As well, CVS_RSH changed to /bin/ssh. Plain "ssh" as all other Hudson jobs use would
probably work if in this wrapper we prepended c:\cygwin\bin to %PATH%.

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
