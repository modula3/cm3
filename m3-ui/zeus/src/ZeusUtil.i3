(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Jun 23 15:06:16 PDT 1993 by steveg *)
(*      modified on Fri Feb  5 23:02:07 PST 1993 by johnh  *)

INTERFACE ZeusUtil;

IMPORT RefList, Point, Rd, Trestle;
<* PRAGMA LL *>


EXCEPTION BadSnapshot(TEXT);

PROCEDURE KeywordCheck (arg: REFANY; t: TEXT) RAISES {BadSnapshot};
<* LL = arbitrary *>
(* arg should be a RefList.T whose first element is an SxSymbol.T whose name
   is t.  If it isn't, complain. *)

PROCEDURE ScreenPosOK (scr: Trestle.ScreenID; pt: Point.T): BOOLEAN;
<* LL = VBT.mu *>
(* RETURN TRUE iff the screen exists and pt is on it. *)

PROCEDURE RdToList(rd: Rd.T): RefList.T;
<* LL = arbitrary *>
  (* read one s-expression from rd.  If it's a list, return it, else return
     NIL.  Catch any exceptions and return NIL if one occurs. *)

PROCEDURE EatChar (rd: Rd.T; c: CHAR): BOOLEAN;
  (* If the next non-white-space character in rd is c, swallow it and
     return TRUE.  Otherwise push it back on the reader and return
     FALSE. *)

END ZeusUtil.
