#
# cd m3-sys/m3tests/src
# del /s std*
# cvs -z3 upd
# perl src.pl
#
use strict;
for my $a (split(/[\r\n]+/, `cmd /c dir /s/b/a-d std*`))
{
    next if (-s $a == 0);
    print("$a\n");
    open my $b, "<", $a || die("failed to open $a");
    my $c = join("", <$b>);
    close($b);
    my $d = $c;
    $d =~ s/"\.\.\/src\//"..\//mg;
    if ($c eq $d)
    {
        print("unlink($a)\n");
        unlink($a);
    }
    else
    {
        print("change $a\n");
        open my $b, ">", $a;
        print $b $d;
    }
}
