(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:01:39 PST 1994 By kalsow     *)

MODULE Main;

IMPORT SLisp, Wr, Rd;
FROM Stdio IMPORT stdout, stdin;

<*FATAL ANY*>

VAR interp: SLisp.T;

PROCEDURE Fact (<*UNUSED*> self: SLisp.Builtin; interp: SLisp.T; 
                args: SLisp.List): SLisp.Sexp RAISES {SLisp.Error} =
  VAR res := NEW (SLisp.Integer); n := interp.evalInt (args.head);
  BEGIN
    res^ := 1;
    FOR i := 1 TO n DO
      res^ := res^ * i; END;
    RETURN res;
  END Fact;

BEGIN
  interp := SLisp.T.new (NIL);

  interp.defineFun (NEW (SLisp.Builtin, name := "fact", apply := Fact,
                         minArgs := 1, maxArgs := 1));

  TRY
    LOOP
      TRY
        LOOP
          Wr.PutText (stdout, "\n-> ");
          Wr.Flush (stdout);
          SLisp.Write (stdout, interp.eval (SLisp.Read (stdin))); END;
      EXCEPT
      | SLisp.Error => END; END;
  EXCEPT 
  | Rd.EndOfFile => END; 
END Main.

