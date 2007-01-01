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

@rem @call :make-lib-1 "%~f0" advapi32.lib advapi32.quake cl.exe link.exe
@rem goto :eof

@for %%i in (
    advapi32
    comctl32
    comdlg32
    gdi32
    glu32
    kernel32
    netapi32
    odbc32
    odbccp32
    opengl32
    user32
    winspool
    wsock32
) do @(
    @call :make-lib-1 "%~f0" %%i.lib %%i.quake cl.exe link.exe
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
@echo %% This is Quake source code, to initialize data. >> %3
@echo %% Quake is the Modula-3 build extension language. >> %3
@echo %% cl is "%~$PATH:4" >> %3
@echo %% link is "%~$PATH:5" >> %3
@call perl -w -x %1 "%~$LIB:2" >> %3
@goto :eof

#!perl -w
#line 60

use strict;
use File::Temp qw(tempfile tempdir);
use File::Basename;

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

printf("%% $0 @ARGV run at %s on %s by %s.\n", scalar localtime(), $ENV{ComputerName}, $ENV{UserName});
printf("%% cl is %s.\n", GetVersion("cl"));
printf("%% link is %s.\n", GetVersion("link"));

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
# Older versions do not understand switches such as /headers, /nologo /symbols, /exports, so just use /all
# and be done with it.
#
$Command = "link /dump /all \"$Lib\"";
print("% $Command\n");
print STDERR "$Command\n";
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
LFile:
while ($Line = <$Pipe>)
{
    my $Function;
    my $Signature;

    # print "%% $Line";
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
        }
        elsif ($Line =~ /^\S+ +machine +\S+$/)
        {
            # not tested (non-x86)
            $Machine = 1;
            $x86 = 0;
            $State = 2;
        }
        elsif ($Line =~ /^OPTIONAL HEADER VALUES$/)
        {
            $State = 2;
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
        }
    }
    elsif ($State == 3)
    {
        if ($Line =~ /^ordinal name$/)
        {
            $State = 4;
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
        # WEP, SystemFunction don't belong in import .libs.
        #
        next if ($Function eq "WEP");
        next if ($Function =~ /^Char(Next|Prev)/);
        next if ($Function =~ /^SystemFunction/);
        next if ($Function =~ /^DllInitialize$/);
        $Exports{$Function} = $Signature;
        # print "%% Function is $Function\n";
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
    $Exports{InterlockedCompareExchange} = "12";
    $Exports{GetSystemTimeAsFileTime} = "4";
    $Exports{IsDebuggerPresent} = "0";
}

print "local readonly $LibBaseName = {\n";

for my $Function (keys %Exports)
{
    if ($Function eq "Extension")
    {
        die("The name \"Extension\" is reserved"); # This is fixable if necessary.
    }
    my $Signature = $Exports{$Function};
    print "\"$Function\" : \"$Signature\",\n";
}

#
# extract .dll name, in particular the extension
# Do this via linking to the .lib and then using link /dump /imports
#

my $CallingConvention;
my $Parameters;

die if !defined $Function;
die if !defined $Signature;

# print STDERR "Function is $Function, Signature is $Signature\n";

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

print $FileHandleC
    "typedef unsigned u;\n"
    . $Declaration . "\n"
    . $Reference . "\n"
    ;

close($FileHandleC);

$Command = "cl /nologo /c /Fo\"$FileNameObj\" \"$FileNameC\"";
# print("% $Command\n");
print STDERR "$Command\n";
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
while ($Line = <$Pipe>)
{
    print STDERR $Line;
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}

$Command = "link /nologo /dll \"$FileNameObj\" /noentry /nodefaultlib /out:\"$FileNameDll\" \"$Lib\"";
# print("% $Command\n");
print STDERR "$Command\n";
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
while ($Line = <$Pipe>)
{
    print STDERR $Line;
}
close($Pipe);
if (($? >>= 8) != 0)
{
    die "$Command failed ($?)\n";
}

$Command = "link /dump /imports \"$FileNameDll\"";
# print("% $Command\n");
print STDERR "$Command\n";
if (!open($Pipe, "$Command 2>&1 |"))
{
    die "$Command failed ($!)\n";
}
my $Extension;
LFile:
while ($Line = lc <$Pipe>)
{
    # print STDERR $Line;
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
print("\"Extension\" : \"$Extension\"\n");

print "}\n";

print "Files\{\"$LibBaseName\"} = $LibBaseName\n";

sub GetPathBaseName
{
    my $x = shift;
    die if !defined $x;
    $x =~ s/^.+\\//g;
    $x =~ s/^.+\///g;
    $x =~ s/\.[^.]*$//g;
    return $x;
}
