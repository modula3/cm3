#!/usr/bin/awk -f

#
# $Id$
#

function process_file(fn,extension,type) {
  cmd = "cat < " fn;

  newname = fn;
  sub(/\.f\.tmpl/,"_" extension ".f",newname);

  printf "%s ", newname > Mk;

  while (cmd | getline) {
    gsub(/ TYPE /, " " type " ");
    gsub(/_EXTENSION/, "_" extension);
    print $0 > newname;
  }
  close(cmd);
  close(newname);

  sub(/\.f/,"",newname);
  printf "s_source(\"%s\")\n", newname > M3k;
}

BEGIN { 
  Mk = "forfiles.mk";
  M3k = "assem.m3k";

  system ("rm -f " Mk " " M3k);

  if (ARGC==2 && ARGV[1]=="clean") {
    print "Cleaning Fortran sources...";
    system("rm -f *_[sd]p.[fs]");
    exit(0);
  }

  printf "FORFILES=\"" > "forfiles.mk";

  forfiles[0] = "mul_mtransposem.f.tmpl";
  forfiles[1] = "mulmv.f.tmpl";
  forfiles[2] = "lu2_backsubstitute.f.tmpl";
  forfiles[3] = "indexeddot.f.tmpl";
  forfiles[4] = "delta.f.tmpl";

  for (i in forfiles) {
    process_file(forfiles[i], "sp", "real"); 
    process_file(forfiles[i], "dp", "double precision"); 
  }

  printf "\"\n" > Mk; 

  printf "for f in ${FORFILES}; do\n" > Mk;
  printf "  echo ${f}:\n" > Mk; 
  printf "  ifort -msse2 -axTPSW -S ${f}\n" >Mk;
  printf "done\n" > Mk;

  close(Mk);
  close(M3k);
  system ("source " Mk);
}

