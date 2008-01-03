(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Dec 15 10:22:05 PST 1992 by mhb      *)
(*      modified on Tue Jun 16 12:58:06 PDT 1992 by muller   *)

MODULE VBTColors;

IMPORT PaintOp, VBT;

TYPE
  Colors = BRANDED REF PaintOp.ColorScheme;

PROCEDURE Put (v: VBT.T; colors: PaintOp.ColorScheme) =
  VAR c := NEW(Colors);
  BEGIN
    c^ := colors;
    VBT.PutProp(v, c)
  END Put;

PROCEDURE Get (v: VBT.T): PaintOp.ColorScheme =
  VAR c: Colors := VBT.GetProp(v, TYPECODE(Colors));
  BEGIN
    IF c = NIL THEN
      RETURN PaintOp.bgFg
    ELSE
      RETURN c^
    END
  END Get;

BEGIN
END VBTColors.
