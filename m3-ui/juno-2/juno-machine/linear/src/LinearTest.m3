(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  8 14:45:02 PDT 1997 by heydon                   *)

MODULE LinearTest EXPORTS Main; 

(* Application to test the Gaussian elimination solver implemented in
   juno-machine/src/RedundantLSolve.m3.

   This program reads descriptions of linear systems from the standard input,
   and writes the solution to the standard output. A linear system is
   described by input taking the following form:

     m n
     row-1 b1
     row-2 b2
     ...
     row-m bm

   where "m" and "n" are integers, each "row-i" consists of "n-1" real
   numbers, and each "bi" is a real number. The program solves for "x" in the
   linear equation "A x = b", where "A" is the matrix consisting of the
   "row-i"'s, and "b" is the vector of the "bi"'s.

   The program exits when it encounters end-of-file. *)

IMPORT Rd, Wr, Params, Fmt, Lex, Scan, FloatMode, Process;
IMPORT RedundantLSolve;
FROM Stdio IMPORT stdout, stdin, stderr;
FROM Thread IMPORT Alerted;

PROCEDURE SyntaxError(msg: TEXT; arg: TEXT := NIL) =
  BEGIN
    Wr.PutText(stderr, "Error: " & msg);
    IF arg # NIL THEN Wr.PutText(stderr, " `" & arg & "'") END;
    Wr.PutText(stderr, "\n");
    Wr.PutText(stderr, "Syntax: LinearTest [ count ]\n");
    Process.Exit(1)
  END SyntaxError;

VAR
  a0, a: REF RedundantLSolve.Matrix;
  x: REF RedundantLSolve.Vector;
  m, n: CARDINAL;
  iterCnt := 1;

<* FATAL Rd.Failure, Wr.Failure, Alerted *>

(* We only pass arrays of the proper size to "RedundantLSolve.P()". *)

BEGIN
  (* parse command-line *)
  IF Params.Count > 1 THEN
    IF Params.Count # 2 THEN SyntaxError("too many arguments") END;
    VAR arg := Params.Get(1); BEGIN
      TRY iterCnt := Scan.Int(arg) EXCEPT
        Lex.Error, FloatMode.Trap =>
          SyntaxError("iteration count is not an integer", arg);
      END;
      IF iterCnt < 1 THEN
        SyntaxError("iteration count must be positive", arg)
      END
    END;
  END;

  (* main work *)
  TRY
    LOOP
      (* Read in array *)
      Lex.Skip(stdin);
      IF Rd.EOF(stdin) THEN EXIT END;
      m := Lex.Int(stdin);
      n := Lex.Int(stdin);
      IF m = 0 OR n = 0 THEN EXIT END;
      a0 := NEW(REF RedundantLSolve.Matrix, m, n);
      FOR i := 0 TO m - 1 DO
        FOR j := 0 TO n - 1 DO
          a0[i, j] := Lex.Real(stdin);
        END;
      END;
      a := NEW(REF RedundantLSolve.Matrix, m, n);
      x := NEW(REF RedundantLSolve.Vector, n - 1);

      (* solve the system *)
      FOR i := 1 TO iterCnt DO
        a^ := a0^;
        RedundantLSolve.P(m, n - 1, a^, x^)
      END;

      (* print the result *)
      FOR i := 0 TO NUMBER(x^) - 1 DO
        Wr.PutText(stdout, Fmt.Real(x[i]) & " ");
      END;
      Wr.PutText(stdout, "\n\n");
      Wr.Flush(stdout)
    END
  EXCEPT
  | Lex.Error =>
      Wr.PutText(stdout, "\nError: Input value has bad syntax.\n");
      Wr.Flush(stdout)
  | FloatMode.Trap =>
      Wr.PutText(stdout, "\nError: Input value too large or small.\n");
      Wr.Flush(stdout)
  END
END LinearTest.
