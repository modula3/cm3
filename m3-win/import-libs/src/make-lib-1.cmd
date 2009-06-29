@rem $Id: make-lib-1.cmd,v 1.11 2009-06-29 19:53:44 jkrell Exp $
@perl -w -x "%~f0" %* 2>&1
@exit /b %ErrorLevel%
#!perl -w
#line 5

#
# Jay Krell
# jaykrell@cornell.edu
# October 18, 2006
#

# 
# This is the first stage in producing Windows import .libs.
# Given preexisting import .libs, produce a text file that drives the next stage.
# This stage requires Perl and preexisting .libs.
# Later stages require just a compiler and linker.
#
# The preexisting import .libs are found in the %LIB% environment variable.
# They can in fact be the output of this very process.
#
# MOST toolsets contain sufficient correct import .libs.
# The notable exceptions are:
#   Older CM3 distributions include "bad" import .libs (depending on context,
#   they were ok at the time)
#   Some "Express Editions" omit many .libs.
#

use strict;
use warnings FATAL => 'uninitialized';
use File::Temp qw(tempfile tempdir);
use File::Basename;
use IO::File;
use FindBin;
use English;
use Cwd;

STDOUT->autoflush(1);
STDERR->autoflush(1);

SetCurrentDirectory($FindBin::Bin);

my $Cl = "cl.exe";
my $Link = "link.exe";

my $LibSearchPath = $ENV{LIB};
if (!$LibSearchPath)
{
    die("The LIB environment variable must be set.");
}

my $ClFullPath = SearchPath($Cl);
if (!$ClFullPath)
{
    die("$Cl not found in path (This can happen when using Cygwin Perl instead of ActiveState Perl.)");
}
my $LinkFullPath = SearchPath($Link);
if (!$LinkFullPath)
{
    die("$Link not found in path (This can happen when using Cygwin Perl instead of ActiveState Perl.)");
}

for my $a (
    "advapi32",
    "comctl32",
    "comdlg32",
    "gdi32",
    "glu32",
    "kernel32",
    "netapi32",
    "odbc32",
    # "odbccp32",
    "opengl32",
    "user32",
    "winspool",
    "wsock32",
    )
{
    my $Lib = "$a.lib";
    my $Quake = "$a.quake";

    unlink($Quake);
    my $LibFullPath = SearchPath($Lib, $LibSearchPath);
    if (!$LibFullPath)
    {
        die("$Lib must in a directory in the LIB environment variable ($LibSearchPath)");
    }
    my $CD = GetCurrentDirectory();

    my $FileHandle = new IO::File($Quake, "w");
    binmode($FileHandle);

    $FileHandle->print(<<End);
% \$Id\$
%
% This is GENERATED Quake source code, to initialize data.
% Quake is the Modula-3 "build extension language".
% That is, most of the "build system" implemented in Modula-3, but
% there is a small "scripting" language to extend it.
%
% This data is then consumed by a second pass that uses the Modula-3 compiler and a C++ compiler and linker.
%
% Here is some information about the environment in which this code was generated:
%
%   cl is $ClFullPath
%   link is $LinkFullPath
%   perl is $EXECUTABLE_NAME
%   current working directory is $CD
%
End

    my $a = sprintf("%% $0 @ARGV run at %s on %s by %s.\n", scalar localtime(), $ENV{ComputerName}, $ENV{UserName});
    print($a);

    $FileHandle->print($a);

    $FileHandle->printf("%%\n");
    $FileHandle->printf("%%   cl is %s.\n", GetVersion("cl"));
    $FileHandle->printf("%%   link is %s.\n", GetVersion("link"));

    my $LibBaseName = lc GetPathBaseName($Lib);
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
    if ($LibFullPath =~ /\s/)
    {
        $LibFullPath = "\"" . $LibFullPath . "\"";
    }
    $Command = "link /dump /all $LibFullPath";
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
        if ($Line eq "")
        {
            next;
        }
        if ($Line =~ /^Summary$/)
        {
            last LFile;
        }
        elsif ($State == 0)
        {
            if ($Line =~ /^FILE HEADER VALUES$/)
            {
                #print("1: $Line\n");
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
                #print("2a: $Line\n");
            }
            elsif ($Line =~ /^\S+ machine \S+$/)
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
                    #die("8: Function is $Function");
                }
                elsif ($Line =~ /^(?:(?:(?:Communal|COMDAT); sym=)|(?:005 00000000 SECT3 notype External \|)) __imp__(.+)$/)
                {
                    die if !defined $1;
                    $Function = $1;
                    $Signature = "__cdecl";
                    #die("9: Function is $Function");
                }
            }
            if ($Line =~ /^Version *:/i
                    && (<$Pipe> =~ /^Machine *:/i)
                    && (<$Pipe> =~ /^TimeDateStamp *:/i)
                    && (<$Pipe> =~ /^SizeOfData *:/i)
                    && (<$Pipe> =~ /^DLL name *:/i)
                    && (($SymbolName) = (<$Pipe> =~ /^Symbol name *: ([^?].+)/i))
                    && (<$Pipe> =~ /^Type *: *code$/i)
                    && (<$Pipe> =~ /^Name type *: *undecorate|ordinal$/i)
                    && (<$Pipe> =~ /^Hint|Ordinal *:/i)
                    #&& (($Name) = (<$Pipe> =~ /^Name *: (.+)/i))
                    )
            {
                chomp($SymbolName);
                if ($x86)
                {
                    $SymbolName =~ s/^_//;
                }
                if ($SymbolName =~ /^(.+)\@(\d+)$/)
                {
                    $Function = $1;
                    $Signature = $2;
                    #die("A: Function is $Function");
                }
                else
                {
                    $Function = $SymbolName;
                    $Signature = "__cdecl";
                    #die("B: Function: $Function, Signature: $Signature\n");
                }
            }
            #
            # This is needed, or so I thought, for for odbccp32.lib, and reasonable all around.
            # I dumped the .dll instead of the .lib by accident.
            #
            if ($Line =~ /^Section contains the following exports for */i)
            {
                $State = 5;
                print("=> state 5\n");
            }
            elsif ($Line =~ /^(?:Communal|COMDAT); sym= __imp_(.+)$/)
            {
                die if !defined $1;
                $Function = $1;
                $Signature = "__cdecl";
                if ($x86)
                {
                    $Function =~ s/^_//;
                    if ($Function =~ /^(.+)\@(\d+)$/)
                    {
                        $Function = $1;
                        $Signature = $2;
                    }
                }
                #die("C: Function is $Function");
            }
            elsif ($Line =~ /^Exports$/)
            {
                $State = 3;
                #print("3: $Line\n");
            }
        }
        elsif ($State == 3)
        {
            if ($Line =~ /^ordinal name$/)
            {
                $State = 4;
                #print("4: $Line\n");
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
                        #die("1: Function is $Function");
                        $Signature = $2;
                    }
                    elsif ($Name =~ /^_(.+)$/)
                    {
                        $Function = $1;
                        #die("2: Function is $Function");
                        $Signature = "__cdecl";
                    }
                }
                else
                {
                    # not tested (non-x86)
                    $Function = $1;
                    #die("3: Function is $Function");
                    $Signature = "__cdecl";
                }
            }
        }
        elsif ($State == 5)
        {
            if ($Line =~ /
                  (^[0-9A-Fa-f]+\ *characteristics$)
                | (^[0-9A-Fa-f]+\ *time date stamp)
                | (^[0-9.]+\ *version$)
                | (^[0-9A-Fa-f]+\ *ordinal base$)
                | (^[0-9A-Fa-f]+\ *number of functions$)
                | (^[0-9A-Fa-f]+\ *number of names$)
                /xi
                )
            {
                # nothing
            }
            elsif ($Line =~ /^ordinal *hint *RVA *name$/i)
            {
                $State = 6;
                print("=> state 6\n");
            }
        }
        elsif ($State == 6)
        {
            if ($Line =~ /^Section contains the following imports:$/i)
            {
                last;
            }
            if ($Line =~ /^\S+ \S+ (\S+) \(forwarded to \S+\)$/)
            {
                if ($x86)
                {
                    next;
                }
                else
                {
                    $Function = $1;
                    #die("4: Function is $Function");
                }
            }
            else
            {
                my ($Ordinal, $Hint, $RVA, $ExportedName, $Equals, $MangledName) = split(/ /, $Line);
                if (!$MangledName)
                {
                    last;
                }
                if ($x86)
                {
                    $MangledName =~ s/^_//;
                }
                if ($MangledName =~ /^(.+)\@(\d+)$/)
                {
                    $Function = $1;
                    $Signature = $2;
                    #die("5: Function is $Function");
                }
                else
                {
                    $Function = $MangledName;
                    $Signature = "__cdecl";
                    #die("6: Function: $MangledName, Signature: $Signature\n");
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
            #print("Function is !$Function!\n");
        }
    }
    close($Pipe);
    if (($? >>= 8) != 0)
    {
        die("$Command failed ($?)");
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

    for my $Function (sort keys %Exports)
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

    my $TempDir = tempdir(CLEANUP => 0);
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
        die("$Command failed ($!)");
    }
    while ($Line = <$Pipe>)
    {
        print($Line);
    }
    close($Pipe);
    if (($? >>= 8) != 0)
    {
        die("$Command failed ($?)");
    }

    $Command = "link /nologo /dll \"$FileNameObj\" /noentry /nodefaultlib /out:\"$FileNameDll\" \"$Lib\"";
    # print("% $Command\n");
    print("$Command\n");
    if (!open($Pipe, "$Command 2>&1 |"))
    {
        die("$Command failed ($!)");
    }
    while ($Line = <$Pipe>)
    {
        print($Line);
    }
    close($Pipe);
    if (($? >>= 8) != 0)
    {
        die("$Command failed ($?)");
    }

    $Command = "link /dump /imports \"$FileNameDll\"";
    # print("% $Command\n");
    print("$Command\n");
    if (!open($Pipe, "$Command 2>&1 |"))
    {
        die("$Command failed ($!)");
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
        die("$Command failed ($?)");
    }
    if ($Extension ne "dll")
    {
        $FileHandle->print("\"Extension\" : \"$Extension\"\n");
    }

    $FileHandle->print("}\n");

    $FileHandle->print("Files\{\"$LibBaseName\"} = $LibBaseName\n");
}

sub GetPathBaseName
{
    my $x = shift;
    die if !defined $x;
    $x =~ s/^.+\\//g;
    $x =~ s/^.+\///g;
    $x =~ s/\.[^.]*$//g;
    return $x;
}

sub RemoveLastPathElement
{
    my $x = shift;
    $x =~ s/[^\\\/]*$//;
    return $x;
}

sub JoinPath
{
    my $x = shift;
    my $y = shift;
    if ($x =~ /[\\\/]$/)
    {
        $y =~ s/^[\\\/]+//;
        return $x . $y;
    }
    if ($y =~ /^[\\\/]/)
    {
        return $x . $y;
    }
    return "$x\\$y";
}

sub SearchPath
{
    my $ToFind = shift;
    my $Paths = (shift || $ENV{PATH});
    for my $Path (split(/;/, $Paths))
    {
        my $Candidate = JoinPath($Path, $ToFind);
        if (-f $Candidate)
        {
            return $Candidate;
        }
    }
}

sub GetCurrentDirectory
{
    return getcwd();
}

sub SetCurrentDirectory
{
    my $a = shift;
    return chdir($a);
}

sub GetVersion
{
    my $Command = shift;
    die if !defined $Command;
    my $Pipe;
    if (!open($Pipe, "$Command 2>&1 |"))
    {
        die("$Command failed ($!)");
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
