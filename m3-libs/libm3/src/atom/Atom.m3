(* Copyright (C) 1994 Digital Equipment Corporation.        *)
(* Distributed only by permission.                          *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Mar 10 16:10:31 PST 1994
 by kalsow  *)
(*      modified on Tue Apr 13 11:19:38 PDT 1993 by mcjones *)

MODULE Atom;

IMPORT Text, AtomAtomTbl;

REVEAL
  T = BRANDED Brand REF RECORD
        text : TEXT;
        hash : INTEGER;
      END;

TYPE
  NewAtomTbl = AtomAtomTbl.Default OBJECT OVERRIDES
    keyEqual := TblEqual;
  END;

VAR
  mutex := NEW(MUTEX);
  table := NEW(NewAtomTbl).init();
  next  : T := NIL;  (* the next atom to be returned by FromText *)

PROCEDURE FromText(t: TEXT): T =
  VAR a: T;
  BEGIN
    LOCK mutex DO
      IF (next = NIL) THEN next := NEW (T) END;
      next.text := t;
      next.hash := Text.Hash (t);
      IF NOT table.get(next, a) THEN
        a := next;
        next := NIL;
        EVAL table.put(a, a)
      END
    END;
    RETURN a
  END FromText;

PROCEDURE ToText(a: T): TEXT =
  BEGIN
    RETURN a.text
  END ToText;

PROCEDURE Hash(a: T): INTEGER =
  BEGIN
    RETURN a.hash
  END Hash;

PROCEDURE TblEqual(<*UNUSED*>self: NewAtomTbl;  READONLY a1, a2: T): BOOLEAN =
  (* This procedure is only used as the "keyEqual" method
     of the internal atom table.  It cannot rely on REF
     equality.  *)
  BEGIN
    RETURN (a1 = a2)
        OR ((a1.hash = a2.hash) AND Text.Equal (a1.text, a2.text));
  END TblEqual;

PROCEDURE Equal(a1, a2: T): BOOLEAN =
  BEGIN
    RETURN a1 = a2
  END Equal;

EXCEPTION Error; <*FATAL Error*>

PROCEDURE Compare(<*UNUSED*> a1, a2: T): [-1..1] =
  BEGIN RAISE Error END Compare;

BEGIN
END Atom.

