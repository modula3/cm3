(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Wed Dec 14 15:26:22 PST 1994 by mcjones        *)


INTERFACE TimeHTML;

IMPORT HTML, Time, Word;

TYPE T = RECORD time: Time.T; html: HTML.T END;

CONST Brand = "TimeHTML";

PROCEDURE Equal(t1, t2: T): BOOLEAN;
(* Raise unchecked runtime error. *)

PROCEDURE Hash(t: T): Word.T;
(* Raise unchecked runtime error. *)

PROCEDURE Compare(t1, t2: T): [-1..1];
(* Earlier time follows later time, for "most-recent-first" lists. *)

END TimeHTML.
