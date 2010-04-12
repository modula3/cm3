open(my $a, "<", "Unix.txt");
open(my $c, ">>", "Uconstants.c");
open(my $i3, ">>", "Unix.i3");
while (my $b = <$a>)
{
  $b =~ s/[\r\n]+$//;
  print $i3 "(* CONST *) <*EXTERNAL Unix__$b*> VAR $b: int;\r\n";
  print $c "#ifdef $b\r\nX($b)\r\n#endif\r\n\r\n";
}
