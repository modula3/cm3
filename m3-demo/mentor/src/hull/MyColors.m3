(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE MyColors;

IMPORT PaintOp;

PROCEDURE Black (): PaintOp.T =
  BEGIN
    RETURN (PaintOp.FromRGB(0.0, 0.0, 0.0, PaintOp.Mode.Accurate))
  END Black;

PROCEDURE White (): PaintOp.T =
  BEGIN
    RETURN (PaintOp.FromRGB(1.0, 1.0, 1.0, PaintOp.Mode.Accurate))
  END White;

PROCEDURE Gray (): PaintOp.T =
  BEGIN
    RETURN (PaintOp.FromRGB(0.6, 0.6, 0.6, PaintOp.Mode.Accurate))
  END Gray;

PROCEDURE Test (): PaintOp.T =   (* currently brown *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.0, 0.45, 1.0, (* was .55 *)
                            PaintOp.Mode.Accurate))
  END Test;

PROCEDURE Head (): PaintOp.T =   (* currently green *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.0, 0.88, 0.0, PaintOp.Mode.Accurate))
  END Head;

PROCEDURE Tail (): PaintOp.T =   (* currently purple *)
  BEGIN
    RETURN (PaintOp.FromRGB(1.0, 0.0, 1.0, PaintOp.Mode.Accurate))
  END Tail;

PROCEDURE Right (): PaintOp.T =  (* currently pale red *)
  BEGIN
    RETURN (PaintOp.FromRGB(1.0, 0.85, 0.8, PaintOp.Mode.Accurate))
  END Right;

PROCEDURE Left (): PaintOp.T =   (* currently pale green *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.8, 1.0, 0.8, PaintOp.Mode.Accurate))
  END Left;

PROCEDURE Front (): PaintOp.T =  (* currently red *)
  BEGIN
    RETURN (PaintOp.FromRGB(1.0, 0.2, 0.0, PaintOp.Mode.Accurate))
  END Front;

PROCEDURE Back (): PaintOp.T =   (* currently purple *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.9, 0.0, 1.0, PaintOp.Mode.Accurate))
  END Back;

PROCEDURE Shaft (): PaintOp.T =  (* currently green *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.0, 0.85, 0.0, PaintOp.Mode.Accurate))
  END Shaft;

PROCEDURE Outside (): PaintOp.T = (* currently pale purple *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.95, 0.8, 1.0, PaintOp.Mode.Accurate))
  END Outside;

PROCEDURE Band (): PaintOp.T =   (* currently red *)
  BEGIN
    RETURN (PaintOp.FromRGB(0.77, 0.57, 0.0, PaintOp.Mode.Accurate))
  END Band;

BEGIN
  FontString := "-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*"
END MyColors.

