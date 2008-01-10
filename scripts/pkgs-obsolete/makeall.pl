use strict;
use warnings uninitialized => "FATAL";
my @AllArray;
my %AllHash;

#
# This only makes approx correct output.
# It must be verified and fixed with verifyorder.
#

for my $FileName (
    "std", # should be first, it is the largest
    "front",
    "core",
    "base",
    "min",
    "all", # must be last, since it doesn't have ordering
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
        if (!$AllHash{$Line})
        {
            push(@AllArray, $Line);
            $AllHash{$Line} = 1;
        }
    }
}

print(join("\n", @AllArray))
