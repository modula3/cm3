MODULE Main;

IMPORT IO, Process, Stdio, Rd, Wr;
IMPORT M3CG, M3CG_Rd, M3CG_Wr, M3CG_BinRd, M3CG_BinWr, MxConfig, Target;
IMPORT M3CG_LLVM;

(*
  usage is problematic depending on program writing to file or stderr
  could be
  m3cgcat -binary < path/file.mc > somefile.mll
  m3llvm < somefile.mll > somefile.ll  with the bin reader 
  or m3llvm < somefile.mll > somefile.ll with the ascii reader 
*)

PROCEDURE DoIt() =
VAR 
  rd_in := Stdio.stdin;
  wr_out := Stdio.stdout;
  cg : M3CG_LLVM.U;
  BEGIN
    Init ();

    cg := M3CG_LLVM.New(wr_out);
(*
  M3CG_BinRd.Inhale(rd_in, cg);
*)
    M3CG_Rd.Inhale(rd_in, cg);

    cg.dumpLLVMIR();
  END DoIt;


PROCEDURE Init () =
  VAR machine: TEXT;
  BEGIN
    machine := MxConfig.Get ("TARGET");
    IF machine = NIL THEN
      IO.Put ("unable to find TARGET definition in configuration file" & Wr.EOL);
      Process.Exit (1);
    ELSIF NOT Target.Init (machine) THEN
      IO.Put ("unable to initialize Target: " & machine & Wr.EOL);
      Process.Exit (1);
    END;
  END Init;

BEGIN
  DoIt();
END Main.
