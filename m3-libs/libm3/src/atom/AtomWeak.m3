(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Sep 20 12:24:50 PDT 1993 by kalsow   *)
(*      modified on Tue Apr 13 11:19:38 PDT 1993 by mcjones  *)

MODULE AtomWeak EXPORTS Atom;

IMPORT TextF, TextToRefanyTable, WeakRef;

REVEAL T = BRANDED REF TEXT;

VAR
  mutex := NEW(MUTEX);
  table := TextToRefanyTable.New();

PROCEDURE FromText(t: TEXT): T =
  VAR
    r: REFANY;
    a: T;
  BEGIN
    LOCK mutex DO
      IF table.in(t, r) THEN
        a := WeakRef.ToRef(NARROW(r, WeakRef.T))
      ELSE
        a := NEW(T);
        a^ := t;
        EVAL table.put(t, WeakRef.FromRef(a, CleanUpProc))
      END
    END;
    RETURN a
  END FromText;

PROCEDURE CleanUpProc(READONLY w: T; a: REFANY) =
  VAR r: REFANY;
  BEGIN
    LOCK mutex DO
      IF NOT table.delete(NARROW(a, T)^, r) THEN <*ASSERT FALSE*> END
    END
  END CleanUpProc;

PROCEDURE ToText(a: T): TEXT =
  BEGIN
    RETURN a^
  END ToText;

PROCEDURE Hash(a: T): INTEGER =
  (* Stolen from John Ellis's Modula-2+ TextTable.Hash. *)
  CONST Multiplier  = -1664117991; 
    (* = LOOPHOLE( Round( .6125423371 * 2^32 ), INTEGER ) *)
  VAR hash: INTEGER := 0;
  BEGIN
    FOR i := 0 TO (* Text.Length(a^)-1 *) MAX (0, NUMBER (a^^) - 1) - 1 DO
      hash := hash * Multiplier + ORD( (* Text.GetChar(a^) *) a^[i] )
    END;
    RETURN hash
  END Hash;

BEGIN
END AtomWeak.

