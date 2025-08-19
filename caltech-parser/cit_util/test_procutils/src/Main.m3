(*
   Copyright (c) 2010 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id: Main.m3,v 1.2 2010/07/08 08:35:58 mika Exp $ *)

MODULE Main;
IMPORT SlowTextCompress;
IMPORT FileRd, Stdio, Thread, FileWr, Process, ProcUtils, Fmt, Debug;
IMPORT Wr, TextWr, Text;

PROCEDURE CopyUsingScript(in : TEXT) =
  VAR
    wrIn : Wr.T;
    wr := NEW(TextWr.T).init();
    writer := ProcUtils.WriteHere(wr);
    c : ProcUtils.Completion;
  BEGIN

    c := ProcUtils.RunText("./script",
                           stdout := writer,
                           stdin := ProcUtils.GimmeWr(wrIn));

    Wr.PutText(wrIn, in);
    Wr.Close(wrIn);

    c.wait();

    WITH out = TextWr.ToText(wr) DO
      Debug.Out("CopyUsingScript: in " & Fmt.Int(Text.Length(in)) &
                " bytes, out " & Fmt.Int(Text.Length(out)) & " bytes")
    END
  END CopyUsingScript;

VAR
  rd := FileRd.Open("in.bz2");
  wr := FileWr.Open("out");
BEGIN
  Debug.Out("Running \"sleep 10\" with standard timeout");
  Debug.Out(ProcUtils.ToText("sleep 10"));
  Debug.Out("Done successfully");

  Debug.Out("Running \"sleep 10\" with 5-second timeout");
  TRY
    Debug.Out(ProcUtils.ToText("sleep 10", timeout := 5.0d0));
    Debug.Out("Done successfully---should not have happened");
  EXCEPT
    ProcUtils.Timeout => Debug.Out("Timed out, as expected.")
  |
    ProcUtils.ErrorExit => Debug.Out("Failure exit, maybe as expected.")
  END;
  
  TRY
    SlowTextCompress.RdWr(SlowTextCompress.Mode.Decompress,rd,wr);
  EXCEPT
    ProcUtils.ErrorExit(e) =>         
       Process.Crash("ProcUtils.ErrorExit in UnStuffPickle: " &
                      ProcUtils.FormatError(e))
  END;


  VAR 
    in := "";
    out, in2 : TEXT;
    inWr := TextWr.New();
  BEGIN
    FOR i := 0 TO 10000 DO
      Wr.PutText(inWr,Fmt.Int(i)  & "_")
    END;
    in := TextWr.ToText(inWr);


    CopyUsingScript(in);

    out := SlowTextCompress.Text(SlowTextCompress.Mode.Compress,in);
    in2 := SlowTextCompress.Text(SlowTextCompress.Mode.Decompress,out) 
  END;

  LOOP Thread.Pause(1.0d0) END
END Main.
