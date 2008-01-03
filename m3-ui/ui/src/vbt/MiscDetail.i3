(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 25 19:48:16 PDT 1992 by msm     *)
(*      modified on Mon Feb 24 13:57:19 PST 1992 by muller  *)
(*      modified on Fri Sep  6 17:25:31 PDT 1991 by gnelson *)
<*PRAGMA LL*>

INTERFACE MiscDetail;

(* A "VBT.MiscRec" contains a detail field consisting of two integers.  It is
   sometimes useful to be able to associate arbitrary data with these
   integers.  This interface provides a simple mechanism to encode a
   REFANY as an INTEGER, thus suitable for passing in one field of a
   MiscDetail.  In typical use, the client should call ToRef immediately
   upon receipt of the MiscRec, since the sender will usually disable
   conversion when the call returns. *)

PROCEDURE FromRef(ra: REFANY): INTEGER;
(* Return a value "i" such that "ToRef(i)" returns "ra".  -1 always
   represents "NIL". *)

PROCEDURE ToRef(i: INTEGER): REFANY;
(* If "i" does not reference anything, return "NIL". *)

PROCEDURE Delete(i: INTEGER);
(* Cause "ToRef(i)" to return "NIL". *)

END MiscDetail.


