#file:     mkview.pl
#abstract: make a v<name>.tex which includes
#          a working <name>.tex
#usage:    a) perl mkview.pl       #makes all names in default list
#          b) perl mkview.pl mymod #makes just vmymod
#
if ($#ARGV >=0) {
#---allow command line control---
  push(@names,shift(@ARGV));
}
else {
#---defualt is full list---
  @names=("arch",
          "concept",
          "utils",
          "bits",
          "integer",
          "real32",
          "real64",
          "cmplx",
          "poly",
          "vect",
          "mat",
          "root",
          "interp",
          "sle");
};
          
foreach $name (@names) {
  local($vname)="v$name";
  print "$vname\n";
  open(OUTFILE,">$vname.tex") || die "cannot open $vname.tex\n";
  select(STDOUT);
  select(OUTFILE);
  print "\\documentclass[fleqn]{article}\n";
  print "\\include{preamble}\n";
  print "\\begin{document}\n";
  print "\\include{$name}\n";
  print "\\end{document}\n";
  close(OUTFILE);

  select(STDOUT);
  system("latex $vname");
}
          
