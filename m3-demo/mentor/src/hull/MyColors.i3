(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE MyColors;

IMPORT PaintOp;

PROCEDURE Black(): PaintOp.T;
PROCEDURE White(): PaintOp.T;
PROCEDURE Gray(): PaintOp.T;
PROCEDURE Test(): PaintOp.T;
PROCEDURE Head(): PaintOp.T;
PROCEDURE Tail(): PaintOp.T;
PROCEDURE Left(): PaintOp.T;
PROCEDURE Right(): PaintOp.T;
PROCEDURE Front(): PaintOp.T;
PROCEDURE Back(): PaintOp.T;
PROCEDURE Shaft(): PaintOp.T;
PROCEDURE Outside(): PaintOp.T;
PROCEDURE Band(): PaintOp.T;

VAR FontString: TEXT;

END MyColors.

