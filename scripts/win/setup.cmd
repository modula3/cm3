@if (1 == 0) /*
@cscript.exe /e:jscript /nologo "%~f0" %*
@exit /b %errorlevel% */
@end

/*
Modula-3 setup for the portable binary + workspace archive format.
It reads a list of relative paths from its neighboring setup.txt,
cd's to those directories and runs cm3 -ship.
*/

var WshShell = WScript.CreateObject("WScript.Shell");
var FileSystemObject = WScript.CreateObject("Scripting.FileSystemObject");

function Echo(s)
{
    WScript.Echo(s)
}

function GetFullPath(s)
{
    return FileSystemObject.GetAbsolutePathName(s);
}

function DirectoryExists(s)
{
    return FileSystemObject.FolderExists(s);
}

function IsEnvironmentVariableDefined(s)
{
    s = "%" + s + "%";
    return (WshShell.ExpandEnvironmentStrings(s) != s);
}

function GetEnvironmentVariable(s)
{
    return WshShell.ExpandEnvironmentStrings("%" + s + "%");
}

function GetEnvironmentVariableElse(s, t)
{
    return IsEnvironmentVariableDefined(s) ? GetEnvironmentVariable(s) : t;
}

//
// Use of environment variables here is probably neither convenient
// nor appropriate, but it is what the preceding version did.
//

var Action = GetEnvironmentVariableElse("ACTION", "cm3 -ship");
var ShipArgs = GetEnvironmentVariableElse("SHIPARGS", "");

var ScriptFullName = WScript.ScriptFullName;
var ScriptDirectory = GetFullPath(WScript.ScriptFullName + "\\..");

var Lines = FileSystemObject.OpenTextFile(ScriptDirectory + "\\setup.txt")
            .ReadAll()
            .match(/^.+$/gm);
var i = 0;
var ExitCode = 0;
var ErrorCount = 0;
var Shell = GetEnvironmentVariableElse("COMSPEC", "cmd.exe");
for (i = 0; i < Lines.length; ++i)
{
    var Directory = ScriptDirectory + "\\" + Lines[i];
    if (DirectoryExists(Directory))
    {
        Echo("...shipping " + Directory + "...")
        //WshShell.CurrentDirectory = Directory;
        Echo("cd " + WshShell.CurrentDirectory)
        //
        // It is important to merge stdout and stderr to avoid deadlock.
        // We need to wrap in cmd for 2>&1 to work, and there is perhaps
        // no other way to achieve such merging.
        //
        var CommandLine = (Shell + " /c " + Action + " " + ShipArgs + " 2>&1").replace(/ +/g, " ");
        Echo(CommandLine);
        var Process = WshShell.Exec(CommandLine);
        while (Process.Status == 0)
        {
            if (Process.StdOut.AtEndOfStream == false)
                Echo(Process.StdOut.ReadAll());
            if (Process.Status == 0)
                WScript.Sleep(100);
        }
        if (Process.ExitCode != 0)
        {
            ErrorCount += 1;
            Echo("failed " + Directory + " with ExitCode " + Process.ExitCode);
            if (ExitCode == 0)
                ExitCode = Process.ExitCode;
        }
    }
    else
    {
        Echo("skipping " + Directory)
    }
}

if (ExitCode != 0)
    Echo(ScriptFullName + " failing with ExitCode:" + ExitCode + "; " + ErrorCount + " errors")

WScript.Quit(ExitCode)
