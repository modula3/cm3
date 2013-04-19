(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* from Trestle *)

MODULE HighlightVBT;

IMPORT VBT;

REVEAL HighlightVBT_T = VBT.VBT_T BRANDED OBJECT OVERRIDES init := Be; END;

PROCEDURE Be(
    <*UNUSED*>v: HighlightVBT_T;
    <*UNUSED*>paintOpValue: VBT.PaintOp_T;
    <*UNUSED*>READONLY paintOpReadOnly: VBT.PaintOp_T;
    <*UNUSED*>pixmapValue: VBT.Pixmap_T;
    <*UNUSED*>READONLY pixmapReadOnly: VBT.Pixmap_T;
    <*UNUSED*>recordValue: VBT.Point_T;
    <*UNUSED*>READONLY recordReadOnly: VBT.Point_T;
    <*UNUSED*>fixedArrayValue: VBT.FixedArray;
    <*UNUSED*>READONLY fixedArrayReadOnly: VBT.FixedArray;
    <*UNUSED*>openArrayValue: VBT.OpenArray;
    <*UNUSED*>READONLY openArrayReadOnly: VBT.OpenArray;
    <*UNUSED*>bigSetValue: VBT.BigSet;
    <*UNUSED*>READONLY bigSetReadOnly: VBT.BigSet;
    <*UNUSED*>smallSetValue: VBT.SmallSet;
    <*UNUSED*>READONLY smallSetReadOnly: VBT.SmallSet;
    ) =
BEGIN
END Be;

BEGIN
END HighlightVBT.
