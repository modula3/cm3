MODULE Main;

(*
  This test is problematic since in can be correct depending on
  the order of modules in the m3makefile or if T2.m3 is recompiled
  after a complete build. It seems to be a linker problem but the
  fact that error involves open array constructors may suggest
  a problem there.
  If one rebuilds this directory and runs the program the correct
  output should be 00 as the value of the NUMBER function for
  the default parameter passed to the R.DoIt procedure which is called
  once by T1 and then by T2.
  However, on my machine the output is 0N where N is some random number.
  If one touches T2.m3 and recompiles. The correct answer is output.
  ( the T2_m.o object file is unchanged but the executable is different )
  If one deletes the target directory and changes the order of T1 and T2
  in the m3makefile and builds, the correct answer is output.

*)

IMPORT T1,T2;

BEGIN
  T1.Proc();
  T2.Proc();
END Main.
