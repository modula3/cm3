(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* from Trestle and libm3 *)

INTERFACE VBT;

TYPE T = OBJECT METHODS
init(op: PaintOp_T := PaintOp_TransparentSwap;
     txt: Pixmap_T := Pixmap_Gray;
     READONLY delta := Point_T{0});
END;

TYPE Pixmap_T = RECORD pm: INTEGER END;
CONST Pixmap_Gray = Pixmap_T{2};

TYPE PaintOp_T = RECORD op:INTEGER END;
CONST PaintOp_Swap = PaintOp_T{3};
CONST PaintOp_TransparentSwap = PaintOp_T{13};

TYPE Point_T = RECORD h: INTEGER END;

END VBT.
