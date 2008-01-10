use strict;
use warnings uninitialized => "FATAL";
my %Arrays;
my %Hashes;

#
# verify that files are in the same order, where they intersect
#

for my $FileName (
    "front",
    "std",
    "core",
    "base",
    "min",
    "all",
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
        push(@{$Arrays{$FileName}}, $Line);
        $Hashes{$FileName}{$Line} = 1;
    }
}

for (my $i = 0 ; $i != scalar keys(%Arrays)  - 1 ; ++$i)
{
    for (my $j = ($i + 1) ; $j != scalar keys(%Arrays) ; ++$j)
    {
        my $Name1 = (keys %Arrays)[$i];
        my $Name2 = (keys %Arrays)[$j];
        print("comparing $i with $j ($Name1 with $Name2)\n");
        my @Intersection1;
        my @Intersection2;
        for my $a (@{$Arrays{$Name1}})
        {
            if ($Hashes{$Name2}{$a})
            {
                #print("$a is in both $Name1 and $Name2\n");
                push(@Intersection1, $a);
            }
            else
            {
                #print("$a is in both $Name1 but not $Name2\n");
            }
        }
        for my $a (@{$Arrays{$Name2}})
        {
            if ($Hashes{$Name1}{$a})
            {
                #print("$a is in both $Name1 and $Name2\n");
                push(@Intersection2, $a);
            }
            else
            {
                #print("$a is in both $Name2 but not $Name1\n");
            }
        }
        if (scalar @Intersection1 != scalar @Intersection2)
        {
            print("ERROR: Intersections differ in length $Name1 $Name2\n");
            next;
        }
        for (my $a = 0 ; $a != scalar @Intersection1 ; ++$a)
        {
            my $b = $Intersection1[$a];
            my $c = $Intersection2[$a];
            if ($b ne $c)
            {
                print("ERROR: $Name1 and $Name2 are in a different order near $b and $c\n");
            }
        }
    }
}
