(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon May 17 22:34:00 PDT 1993 by mjordan    *)

UNSAFE MODULE FRefRefTbl;

IMPORT RefRefTbl, Refany, Word;

REVEAL
  Default = RefRefTbl.Default BRANDED OBJECT
  OVERRIDES
    keyEqual := KeyEqual;
    keyHash := KeyHash;
  END;

PROCEDURE KeyEqual(<*UNUSED*> t: Default; READONLY k1, k2: Refany.T): BOOLEAN=
  BEGIN
    RETURN k1 = k2;
  END KeyEqual;

PROCEDURE KeyHash(<*UNUSED*> t: Default; READONLY k: Refany.T): Word.T=
  BEGIN
    RETURN LOOPHOLE(k, Word.T);
  END KeyHash;

BEGIN
END FRefRefTbl.


    
