(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jan 14 10:24:31 PST 1994 by kalsow     *)
(*      modified on Fri Apr 03 18:33:16 PST 1992 by muller     *)

UNSAFE MODULE TclCTest EXPORTS Main;

IMPORT TclC;
IMPORT Rd, Wr, Stdio, Fmt;
FROM M3toC IMPORT TtoS, CopyTtoS, CopyStoT;
FROM Ctypes IMPORT int, char_star;

<*FATAL ANY*>

PROCEDURE FactCmd (<*UNUSED*> clientData: TclC.ClientData;
		   interp: TclC.Interp_star;
   	 	   argc: int;
		   argv: TclC.Argv): int =
  BEGIN
    IF argc # 2 THEN
      interp.result := TtoS ("fact has exactly one argument");
      RETURN TclC.TCL_ERROR; 
    ELSE
      VAR i: INTEGER; BEGIN
        IF TclC.GetInt (interp, argv[1], ADR (i)) = TclC.TCL_ERROR THEN
          RETURN TclC.TCL_ERROR; END;
        interp.result := CopyTtoS (Fmt.Int (Fact (i)));
        RETURN TclC.TCL_OK; END; END;
  END FactCmd;

PROCEDURE Fact (i: INTEGER): INTEGER =
  BEGIN
    IF i = 1 THEN
      RETURN 1;
    ELSE 
      RETURN i * Fact (i - 1); END;
  END Fact;


VAR
  i: TclC.Interp_star;
  buf: TclC.CmdBuf;
  s: char_star;
  status: int;

BEGIN
  i := TclC.CreateInterp ();
  TclC.CreateCommand (i, TtoS ("fact"), FactCmd, 0, NIL);
  buf := TclC.CreateCmdBuf ();

  TRY
    LOOP 
      Wr.PutText (Stdio.stdout, "-> ");
      Wr.Flush (Stdio.stdout);
      s := TclC.AssembleCmd (buf, TtoS (Rd.GetLine (Stdio.stdin) & "\n"));
      IF s # NIL THEN
        status := TclC.RecordAndEval (i, s, 0);
        CASE status OF 
        | TclC.TCL_OK =>
            Wr.PutText (Stdio.stdout, CopyStoT (i.result) & "\n"); 
            Wr.Flush (Stdio.stdout);
        | TclC.TCL_ERROR =>
            Wr.PutText (Stdio.stderr, "error: " & CopyStoT (i.result) & "\n"); 
            Wr.Flush (Stdio.stdout);
        ELSE
            Wr.PutText (Stdio.stderr, "TclC.Eval returned with code = " 
					& Fmt.Int (status) & "\n"); 
            Wr.Flush (Stdio.stdout);
                                                        END; END; END; 
  EXCEPT
  | Rd.EndOfFile => END;

  TclC.DeleteCmdBuf (buf);
  TclC.DeleteInterp (i);
END TclCTest.
