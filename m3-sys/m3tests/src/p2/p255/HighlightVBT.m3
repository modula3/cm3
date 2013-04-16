(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* from Trestle *)

MODULE HighlightVBT;

IMPORT VBT;

REVEAL HighlightVBT_T = VBT.VBT_T BRANDED OBJECT OVERRIDES init := Be; END;

PROCEDURE Be(
    <*UNUSED*>v: HighlightVBT_T;
    <*UNUSED*>op: VBT.PaintOp_T;
    <*UNUSED*>txt: VBT.Pixmap_T;
    <*UNUSED*>READONLY delta: VBT.Point_T) =
BEGIN
END Be;

BEGIN
END HighlightVBT.
