(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Wed Jul 13 11:52:34 PDT 1994 by mcjones        *)

UNSAFE MODULE TimeHTML;

IMPORT Word;

EXCEPTION URE; <*FATAL URE*>

PROCEDURE Equal(<*UNUSED*>t1, t2: T): BOOLEAN = BEGIN RAISE URE END Equal;

PROCEDURE Hash(<*UNUSED*>t: T): Word.T = BEGIN RAISE URE END Hash;

PROCEDURE Compare(t1, t2: T): [-1..1] =
  BEGIN
    IF t1.time > t2.time THEN RETURN -1
    ELSIF t1.time = t2.time THEN RETURN 0
    ELSIF t1.time < t2.time THEN RETURN 1
    ELSE RAISE URE
    END
  END Compare;

BEGIN
END TimeHTML.
