(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 13 15:54:04 PDT 1993 by swart      *)
(*      modified on Tue Apr 13 11:16:19 PDT 1993 by mcjones    *)

MODULE Refany;

IMPORT Word;

PROCEDURE Equal(r1, r2: T): BOOLEAN = BEGIN RETURN r1 = r2 END Equal;

EXCEPTION Error; <*FATAL Error*>

PROCEDURE Hash(<*UNUSED*> r: T): Word.T = BEGIN RAISE Error END Hash;

PROCEDURE Compare (<*UNUSED*> r1, r2: T): [-1 .. 1] =
  BEGIN
    RAISE Error
  END Compare;


BEGIN
END Refany.
