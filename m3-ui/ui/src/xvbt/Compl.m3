(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 11 10:45:20 PST 1993 by kalsow   *)

MODULE Compl;

IMPORT ComplSeq;

PROCEDURE Get (): T =
  BEGIN
    LOCK mu DO
      IF seq.size() = 0 THEN RETURN NEW(T); ELSE RETURN seq.remhi(); END;
    END;
  END Get;

PROCEDURE Free (t: T) =
  BEGIN
    LOCK mu DO seq.addhi(t); END;
  END Free;

VAR
  mu  := NEW(MUTEX);
  seq := NEW(ComplSeq.T).init();

BEGIN
END Compl.
