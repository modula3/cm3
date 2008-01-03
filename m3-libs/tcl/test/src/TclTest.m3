(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jan 14 10:24:23 PST 1994 by kalsow     *)
(*      modified on Tue Apr 07 17:31:11 PDT 1992 by muller     *)

UNSAFE MODULE TclTest EXPORTS Main;

IMPORT Tcl, Text;
IMPORT Rd, Wr, Stdio, Fmt;

<*FATAL ANY*>

PROCEDURE FactCmd (<*UNUSED*> self: FactClosure;
                   interp: Tcl.T; args: Tcl.Args) RAISES {Tcl.Error} =
  BEGIN
    IF NUMBER (args) # 2 THEN
      interp.setResult ("fact has exactly one argument");
      RAISE Tcl.Error (Tcl.ErrorCode.Error);
    ELSE
      interp.setResult (Fmt.Int (Fact (interp.getInt (args [1])))); END;
  END FactCmd;

PROCEDURE Fact (i: INTEGER): INTEGER =
  BEGIN
    IF i = 1 THEN
      RETURN 1;
    ELSE 
      RETURN i * Fact (i - 1); END;
  END Fact;


VAR
  i: Tcl.T;
  buf: Tcl.CmdBuf;
  s: Text.T;

TYPE
  FactClosure = Tcl.CmdClosure OBJECT OVERRIDES
                  apply := FactCmd; END;

BEGIN
  i := Tcl.T.new (NIL);
  i.createCommand ("fact", NEW (FactClosure));

  buf := Tcl.CmdBuf.new (NIL);

  TRY
    LOOP 
      Wr.PutText (Stdio.stdout, "-> ");
      Wr.Flush (Stdio.stdout);
      s := buf.assemble (Rd.GetLine (Stdio.stdin) & "\n");
      IF s # NIL THEN
        TRY
          i.recordAndEval (s);
          Wr.PutText (Stdio.stdout, i.getResult () & "\n"); 
          Wr.Flush (Stdio.stdout);
        EXCEPT
          | Tcl.Error =>
            Wr.PutText (Stdio.stderr, "error: " & i.getResult () & "\n"); 
            Wr.Flush (Stdio.stdout); END; END; END;

  EXCEPT
  | Rd.EndOfFile => END;

  i.delete ();
  buf.delete ();
END TclTest.
