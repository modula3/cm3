(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordType.i3                                         *)
(* Last Modified On Wed Sep  7 15:41:01 PDT 1994 By kalsow     *)
(*      Modified On Sun Sep  9 06:32:29 1990 By muller         *)

INTERFACE RecordType;

IMPORT M3ID, Type, Scope, Value;

PROCEDURE Parse (): Type.T;
PROCEDURE ParseFieldList ();

PROCEDURE Split (t: Type.T;  VAR fields: Value.T): BOOLEAN;

PROCEDURE LookUp (t: Type.T;  name: M3ID.T;  VAR field: Value.T): BOOLEAN;

PROCEDURE SizeAndAlignment (fields: Scope.T;
                            VAR(*OUT*) recSize, recAlign: INTEGER;
                            VAR(*OUT*) is_solid: BOOLEAN);

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER;

END RecordType.
