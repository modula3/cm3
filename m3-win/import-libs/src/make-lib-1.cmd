@echo off

@rem
@rem Jay Krell
@rem jaykrell@cornell.edu
@rem October 18, 2006
@rem

@rem 
@rem This is the first stage in producing Windows import .libs.
@rem Given preexisting import .libs, produce a text file that drives the next stage.
@rem This stage requires Perl and preexisting .libs.
@rem Later stages require just a compiler and linker.
@rem
@rem The preexisting import .libs are found in the %LIB% environment variable.
@rem They can in fact be the output of this very process.
@rem

@setlocal

@pushd %~dp0

@rem @call :make-lib-1 "%~f0" advapi32.lib advapi32.quake cl.exe link.exe perl.exe
@rem goto :eof

@rem
@rem odbc32
@rem odbccp32
@rem
@rem removed, something is up/down with them
@rem

@for %%i in (
    advapi32
    comctl32
    comdlg32
    gdi32
    glu32
    kernel32
    netapi32
    opengl32
    user32
    winspool
    wsock32
) do @(
    @echo :make-lib-1 "%~f0" %%i.lib %%i.quake cl.exe link.exe perl.exe
    @call :make-lib-1 "%~f0" %%i.lib %%i.quake cl.exe link.exe perl.exe
    @if errorlevel 1 goto :eof
)
@goto :eof

:make-lib-1
if exist %3 del %3
@if not exist "%~$LIB:2" (
    @echo error %2 must exist in %%lib%%
    @error 2> nul
    @goto :eof
)
@echo %% >> %3
@echo %% This is GENERATED Quake source code, to initialize data. >> %3
@echo %% Quake is the Modula-3 "build extension language". >> %3
@echo %% That is, most of the "build system" implemented in Modula-3, but >> %3
@echo %% there is a small "scripting" language to extend it. >> %3
@echo %% >> %3
@echo %% This data is then consumed by a second pass that uses the Modula-3 compiler and a C++ compiler and linker. >> %3
@echo %% >> %3
@echo %% Here is some information about the environment in which this code was generated: >> %3
@echo %% >> %3
@echo %%   cl is "%~$PATH:4" >> %3
@echo %%   link is "%~$PATH:5" >> %3
@echo %%   perl is "%~$PATH:6" >> %3
@echo %%   current working directory is %CD% >> %3
@echo %% >> %3
@echo perl -w -x %1 "%~$LIB:2" %3
@call perl -w -x %1 "%~$LIB:2" %3
@goto :eof

#!perl -w
#line 73

use strict;
use warnings FATAL => 'uninitialized';
use File::Temp qw(tempfile tempdir);
use File::Basename;
use IO::File;

STDOUT->autoflush(1);
STDERR->autoflush(1);

my $a = sprintf("%% $0 @ARGV run at %s on %s by %s.\n", scalar localtime(), $ENV{ComputerName}, $ENV{UserName});
print($a);

my $FileHandle = new IO::File($ARGV[1], "w+");
$FileHandle->print($a);

sub GetVersion
{
    my $Command = shift;
    die if !defined $Command;
    my $Pipe;
    if (!open($Pipe, "$Command 2>&1 |"))
    {
        die "$Command failed ($!)\n";
    }
    while (my $Line = <$Pipe>)
    {
        chomp($Line);
        if ($Line =~ /version/i)
        {
            return $Line;
        }
    }
}

$FileHandle->printf("%%\n");
$FileHandle->printf("%%   cl is %s.\n", GetVersion("cl"));
$FileHandle->printf("%%   link is %s.\n", GetVersion("link"));

my $Lib = shift;
die if !defined $Lib;
my $LibBaseName = lc GetPathBaseName($Lib);
my $Program = $0;
my $x86 = 1;
my $Machine;
my $Pipe;
my %Exports;
my $Command;
my $State = 0;
my $Line;

#
# Older versions do not understand switches such as /headers, /nologo, /symbols, /exports, so just use /all
# and be done with it.
#
$Command = "link /dump /all \"$Lib\"";
$FileHandle->printf("%%\n");
$FileHandle->print("% $Command\n");
$FileHandle->printf("%%\n");
print("$Command\n");
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
LFile:
while ($Line = <$Pipe>)
{
    my $Function;
    my $Signature;

    #print("%% $Line");
    chomp($Line);
    $Line =~ s/\s+/ /g;
    $Line =~ s/^ //;
    $Line =~ s/ $//;
    if ($Line =~ /^Summary$/)
    {
        last LFile;
    }
    elsif ($State == 0)
    {
        if ($Line =~ /^FILE HEADER VALUES$/)
        {
            print("1: $Line\n");
            $State = 1;
        }
    }
    elsif ($State == 1)
    {
        if ($Line =~ /^14[cC] machine \([iI3xX]+86\)$/)
        {
            $Machine = 1;
            $x86 = 1;
            $State = 2;
            print("2a: $Line\n");
        }
        elsif ($Line =~ /^\S+ +machine +\S+$/)
        {
            # not tested (non-x86)
            $Machine = 1;
            $x86 = 0;
            $State = 2;
            print("2b: $Line\n");
        }
        elsif ($Line =~ /^OPTIONAL HEADER VALUES$/)
        {
            $State = 2;
            print("2c: $Line\n");
        }
    }
    elsif ($State == 2)
    {
        #
        # Older link /dump such as VC4 do not understand /exports, therefore
        # we need to deduce exports from other information.
        #
        # VC2 odbc32.lib requires the 005 00000000 SECT3 notype External | __imp__ part.
        # We could also use the library member here.
        #
        my $SymbolName;
        my $Name;
        #print("$Line\n");
        if ($x86)
        {
            if ($Line =~ /^(?:(?:(?:Communal|COMDAT); sym=)|(?:005 00000000 SECT3 notype External \|)) __imp__(.+)\@(\d+)$/)
            {
                die if !defined $1;
                die if !defined $2;
                $Function = $1;
                $Signature = $2;
            }
            elsif ($Line =~ /^(?:(?:(?:Communal|COMDAT); sym=)|(?:005 00000000 SECT3 notype External \|)) __imp__(.+)$/)
            {
                die if !defined $1;
                $Function = $1;
                $Signature = "__cdecl";
            }
            elsif ($Line =~ /^Version\s*:/i
                    && (<$Pipe> =~ /^\s*Machine\s*:/i)
                    && (<$Pipe> =~ /^\s*TimeDateStamp\s*:/i)
                    && (<$Pipe> =~ /^\s*SizeOfData\s*:/i)
                    && (<$Pipe> =~ /^\s*DLL name\s*:/i)
                    && (($SymbolName) = (<$Pipe> =~ /^\s*Symbol name\s*:\s([^?].+)/i))
                    && (<$Pipe> =~ /^\s*Type\s*:\s*code$/i)
                    && (<$Pipe> =~ /^\s*Name type\s*:\s*undecorate$/i)
                    && (<$Pipe> =~ /^\s*Hint\s*:/i)
                    && (($Name) = (<$Pipe> =~ /^\s*Name\s*:\s(.+)/i))
                    )
            {
                chomp($SymbolName);
                $SymbolName =~ s/^_//;
                if ($SymbolName =~ /^(.+)\@(\d+)$/)
                {
                    $Function = $1;
                    $Signature = $2;
                }
                else
                {
                    $Function = $SymbolName;
                    $Signature = "__cdecl";
                    print("Function: $Function, Signature: $Signature\n");
                    die();
                }
                #print("Function: $Function, Signature: $Signature\n");
                #die();
            }
        }
        elsif ($Line =~ /^(?:Communal|COMDAT); sym= __imp_(.+)$/)
        {
            # not tested (non-x86)
            die if !defined $1;
            $Function = $1;
            $Signature = "__cdecl";
        }
        elsif ($Line =~ /^Exports$/)
        {
            $State = 3;
            print("3: $Line\n");
        }
    }
    elsif ($State == 3)
    {
        if ($Line =~ /^ordinal name$/)
        {
            $State = 4;
            print("4: $Line\n");
        }
    }
    elsif ($State == 4)
    {
        if ($Line =~ /(\S+)$/)
        {
            my $Name = $1;
            if ($x86)
            {
                if ($Name =~ /^_(.+)\@(\d+)$/)
                {
                    $Function = $1;
                    $Signature = $2;
                }
                elsif ($Name =~ /^_(.+)$/)
                {
                    $Function = $1;
                    $Signature = "__cdecl";
                }
            }
            else
            {
                # not tested (non-x86)
                $Function = $1;
                $Signature = "__cdecl";
            }
        }
    }
    if ($Function)
    {
        #
        # Exclude various functions.
        # CharPrevEx changed signatures and CharPrev/Next are useless -- just use Unicode and add/subtract 1.
        # WEP, SystemFunction don't belong in import .libs. The all caps names look bogus.
        # Remove various other undocumented functions that are in public import libs.
        #
        next if ($Function eq "WEP");
        next if ($Function =~ /^Char(Next|Prev)/);
        next if ($Function =~ /^SystemFunction/);
        next if ($Function eq "InitializeDll");
        next if ($Function eq "DllInitialize");
        next if ($Function eq "DEVICECAPABILITIES");
        next if ($Function eq "EXTDEVICEMODE");
        next if ($Function eq "ADVANCEDSETUPDIALOG");
        next if ($Function eq "DEVICEMODE");
        next if ($Function eq "LsaGetSystemAccessAccount"); # not documented
        next if ($Function eq "wvsprintfA"); # useless esp. for Modula-3, use C runtime if necessary
        next if ($Function eq "wvsprintfW"); # useless esp. for Modula-3, use C runtime if necessary
        next if ($Function eq "wsprintfA"); # useless esp. for Modula-3, use C runtime if necessary
        next if ($Function eq "wsprintfW"); # useless esp. for Modula-3, use C runtime if necessary
        $Exports{$Function} = $Signature;
        # print("%% Function is $Function\n");
    }
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}

#
# Capture a function to reference to build a client to discover the extension below..
# Do this before adding in missing functions since we cannot reference those.
#
my $Function = (keys %Exports)[0];
my $Signature = $Exports{$Function};

#
# Add functions missing in Visual C++ 2.0 import .libs.
# These were all added in Windows NT 3.51 and Windows 95.
#
if (lc GetPathBaseName($Lib) eq "comctl32")
{
    $Exports{ImageList_GetIcon} = "12";
    $Exports{ImageList_LoadImageA} = "28";
    $Exports{ImageList_Remove} = "8";
    $Exports{ImageList_ReplaceIcon} = "12";
}
elsif (lc GetPathBaseName($Lib) eq "kernel32")
{
#
# Note that binaries built using the Visual C++ 8.0 C runtime won't run on very old versions of Windows
# due to the use of these functions.
#
# GetSystemTimeAsFileTime is missing in Windows NT 3.1, requires Windows NT 3.5 or Windows 95
# IsDebuggerPresent is missing in Windows NT 3.x and Windows 95, requires Windows NT 4 or Windows 98
# InterlockedCompareExchange is missing in Windows NT 3.x and Windows 95, requires Windows NT 4 or Windows 98, or can be a comiler instrinsic
#
    $Exports{InterlockedCompareExchange} = "12";
    $Exports{GetSystemTimeAsFileTime} = "4";
    $Exports{IsDebuggerPresent} = "0";
}

$FileHandle->print("local readonly $LibBaseName = {\n");

for my $Function (keys %Exports)
{
    if ($Function eq "Extension")
    {
        die("The name \"Extension\" is reserved"); # This is fixable if necessary.
    }
    my $Signature = $Exports{$Function};
    $FileHandle->print("\"$Function\" : \"$Signature\",\n");
}

#
# extract .dll name, in particular the extension
# Do this via linking to the .lib and then using link /dump /imports
#

my $CallingConvention;
my $Parameters;

die if !defined $Function;
die if !defined $Signature;

# print("Function is $Function, Signature is $Signature\n");

if ((!$x86) || ($Signature eq "__cdecl"))
{
    $CallingConvention = "__cdecl";
    $Parameters = "void";
}
else
{
    $CallingConvention = "__stdcall";
    if ($Signature ne "0")
    {
        for (my $i = 1 ; $i <= $Signature / 4; ++$i)
        {
            if ($Parameters)
            {
                $Parameters .= ", u a$i";
            }
            else
            {
                $Parameters = "u a$i";
            }
        }
    }
    else
    {
        $Parameters = "void";
    }
}

die if !defined($CallingConvention);
die if !defined($Parameters);

my $Declaration = "__declspec(dllimport) void $CallingConvention $Function($Parameters);";
my $Reference = "__declspec(dllexport) void* Reference(void) { return (void*)&$Function; }";

my $TempDir = tempdir(CLEANUP => 1);
my ($FileHandleC, $FileNameC) = tempfile(DIR => $TempDir, SUFFIX => ".c");
my ($FileHandleDll, $FileNameDll) = tempfile(DIR => $TempDir, SUFFIX => ".dll");
my ($FileHandleObj, $FileNameObj) = tempfile(DIR => $TempDir, SUFFIX => ".obj");

close($FileHandleDll);
close($FileHandleObj);

$FileHandleC->print(
    "typedef unsigned u;\n"
    . $Declaration . "\n"
    . $Reference . "\n"
    );

close($FileHandleC);

$Command = "cl /nologo /c /Fo\"$FileNameObj\" \"$FileNameC\"";
# print("% $Command\n");
print("$Command\n");
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
while ($Line = <$Pipe>)
{
    print($Line);
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}

$Command = "link /nologo /dll \"$FileNameObj\" /noentry /nodefaultlib /out:\"$FileNameDll\" \"$Lib\"";
# print("% $Command\n");
print("$Command\n");
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
while ($Line = <$Pipe>)
{
    print($Line);
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}

$Command = "link /dump /imports \"$FileNameDll\"";
# print("% $Command\n");
print("$Command\n");
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
my $Extension;
LFile:
while ($Line = lc <$Pipe>)
{
    # print($Line);
    chomp($Line);
    $Line =~ s/\s+/ /g;
    $Line =~ s/^ //;
    $Line =~ s/ $//;
    if ($Line =~ /^section contains the following imports:?$/)
    {
        while ($Line = lc <$Pipe>)
        {
            chomp($Line);
            $Line =~ s/\s+/ /g;
            $Line =~ s/^ //;
            $Line =~ s/ $//;
            if ($Line =~ /^$LibBaseName\.(\S+)$/)
            {
                $Extension = $1;
                last LFile;
            }
        }
    }
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}
$FileHandle->print("\"Extension\" : \"$Extension\"\n");

$FileHandle->print("}\n");

$FileHandle->print("Files\{\"$LibBaseName\"} = $LibBaseName\n");

sub GetPathBaseName
{
    my $x = shift;
    die if !defined $x;
    $x =~ s/^.+\\//g;
    $x =~ s/^.+\///g;
    $x =~ s/\.[^.]*$//g;
    return $x;
}
