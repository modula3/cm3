

my $a = shift;
open(my $b, "<", $a);
my $c = join("", <$b>);

my $d = $c;
$d =~ s/#ifdef __cplusplus\nextern "C" {\n#endif\n/EXTERN_C_START\n/;

if ($c ne $d)
{
  open(my $b, ">", $a);
  print $b $c;
}
