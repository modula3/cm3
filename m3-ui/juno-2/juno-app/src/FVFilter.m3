(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 11 14:10:54 PDT 1994 by heydon                   *)
<* PRAGMA LL *>

MODULE FVFilter;

IMPORT FormsVBT;

CONST Cursor = ARRAY OF TEXT{"XC_iron_cross", "XC_watch"};

PROCEDURE MakePassive(fv: FormsVBT.T; nm: TEXT; cursor := CursorKind.Passive) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.MakePassive(fv, nm, cursor := Cursor[ORD(cursor)])
  END MakePassive;

PROCEDURE MakeActive(fv: FormsVBT.T; nm: TEXT) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.MakeActive(fv, nm)
  END MakeActive;

BEGIN
END FVFilter.
