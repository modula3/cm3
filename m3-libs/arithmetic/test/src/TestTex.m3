MODULE TestTex EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Tex module.

1/1/96    <name>   Initial version

*)

IMPORT FileWr, Wr, Fmt, Process;
IMPORT Thread, OSError;
IMPORT TempFiles AS Tmp;

(*=======================*)
CONST
  Module = "TestTex.";
(*----------------------*)
PROCEDURE TestTexVector():BOOLEAN=
CONST
  ftn = Module & "TestTexVector";
  filename = "test";
VAR
  result:=TRUE;
  out := FileWr.Open(filename & ".tex");

<*FATAL OSError.E, Thread.Alerted, Wr.Failure *>
BEGIN
  Debug(1,ftn,"begin\n");

  Wr.PutText(out,"\\documentclass[a4paper]{article}\n");
  Wr.PutText(out,"\\begin{document}\n");
  Wr.PutText(out,"Important result: " & Fmt.Int(42) & "\n");
  Wr.PutText(out,"\\end{document}\n");

  Wr.Close(out);

  Tmp.Note(filename&".log");
  Tmp.Note(filename&".aux");
  EVAL Process.Wait(Process.Create ("latex", ARRAY OF TEXT{filename}));
  EVAL Process.Wait(Process.Create ("xdvi",  ARRAY OF TEXT{filename}));

  RETURN result;
END TestTexVector;
(*-------------------------*)
PROCEDURE TestTex():BOOLEAN=
CONST ftn = Module & "TestTex";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestTexVector();
  RETURN result;
END TestTex;
(*=======================*)
BEGIN
END TestTex.
