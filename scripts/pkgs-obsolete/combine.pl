use strict;
use warnings uninitialized => "FATAL";
my %Hashes;

for my $FileName (
    "front",
    "std",
    "core",
    "base",
    "min",
    )
{
    open(my $FileHandle, "<", $FileName);
    while (my $Line = <$FileHandle>)
    {
        chomp($Line);
        $Line =~ s/\s//g;
        if ($Line eq "")
        {
            next;
        }
        $Hashes{$Line}{$FileName} = 1;
    }
}

open(my $FileHandle, "<", "all");
while (my $Line = <$FileHandle>)
{
    chomp($Line);
    $Line =~ s/\s//g;
    if ($Line eq "")
    {
        next;
    }
    if ($Hashes{$Line})
    {
        print($Line . " " . join(" ", sort keys %{$Hashes{$Line}}) . "\n")
    }
    else
    {
        print($Line . "\n")
    }
}
