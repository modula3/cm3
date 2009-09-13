open(my $a, "<", "Unix.txt");
open(my $c, ">>", "Uconstants.c");
open(my $i3, ">>", "Unix.i3");
while (my $b = <$a>)
{
  chomp($b);
  print $i3 <<End;
(* CONST *) <*EXTERNAL Unix__$b*> VAR $b: int;
End
  print $c <<End;
\#ifdef $b
X($b)
\#endif

End
}
