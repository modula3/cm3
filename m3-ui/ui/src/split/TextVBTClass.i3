(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: TextVBTClass.i3, coded by cgn Sun Jun 28 16:13:38 1987 *)
(* Last modified on Mon Feb 24 13:54:53 PST 1992 by muller  *)
(*      modified on Tue Nov 19 19:12:00 PST 1991 by gnelson *)
(*      modified on Wed Sep 11  9:50:53 PDT 1991 by msm *)
<*PRAGMA LL*>

INTERFACE TextVBTClass;

IMPORT TextVBT;

REVEAL TextVBT.T <: T;

TYPE T = TextVBT.Public OBJECT 
  <* LL >= {SELF} *>
  text: TEXT
  END;

(* If you modify the "text" field, you must call "VBT.Mark". *)

END TextVBTClass.
