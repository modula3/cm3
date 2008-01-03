(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Revelation.def                                        *)
(* Last modified on Tue Dec 20 15:09:01 PST 1994 by kalsow     *)
(*      modified on Fri Feb 23 03:41:40 1990 by muller         *)


INTERFACE Revelation;

IMPORT Type, Value, Decl;

TYPE
  Set <: REFANY;

TYPE
  TypeList = REF RECORD  next: TypeList;  type: Type.T   END;
  TypeSet = RECORD
    cnt    : CARDINAL;
    types  : ARRAY [0..9] OF Type.T;
    others : TypeList;
   END;

PROCEDURE NewSet  (module: Value.T): Set;
PROCEDURE Push    (s: Set): Set;
PROCEDURE Pop     (s: Set);

PROCEDURE Parse   (READONLY att: Decl.Attributes);

PROCEDURE Inherit (s: Set;  import: Value.T);

PROCEDURE TypeCheck  (s: Set);

PROCEDURE Declare (s: Set;  VAR full_info, partial_info: INTEGER);

PROCEDURE LookUp (key: Type.T): Type.T;

PROCEDURE LookUpAll (key: Type.T;  VAR(*OUT*) x: TypeSet);

PROCEDURE Reuse (s: Set);

END Revelation.
