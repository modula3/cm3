(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* from Trestle and libm3 *)

INTERFACE VBT;

TYPE VBT_T = OBJECT METHODS
init(
    paintOpValue := PaintOp_TransparentSwap;
    READONLY paintOpReadOnly := PaintOp_TransparentSwap;
    pixmapValue := Pixmap_Gray;
    READONLY pixmapReadOnly:= Pixmap_Gray;
    recordValue := Point_T{0};
    READONLY recordReadOnly := Point_T{1};
    fixedArrayValue := FixedArray0; (* typeid:T4F238AAE *)
    READONLY fixedArrayReadOnly := FixedArray0;
    openArrayValue := OpenArray0;
    READONLY openArrayReadOnly := OpenArray0;
    bigSetValue := BigSet0;         (* typeid:T67A7B112 *)
    READONLY bigSetReadOnly := BigSet0;
    smallSetValue := SmallSet0;
    READONLY smallSetReadOnly := SmallSet0;
    );
END;

TYPE SmallSet = SET OF [0..10];
CONST SmallSet0 = SmallSet{1};

TYPE BigSet = SET OF [0..1024];
CONST BigSet0 = BigSet{1};

TYPE FixedArray = ARRAY [0..0] OF INTEGER;
CONST FixedArray0 = FixedArray{1};

TYPE OpenArray = ARRAY OF INTEGER;
CONST OpenArray0 = OpenArray{1};

TYPE Pixmap_T = RECORD Pixmap_T_field: INTEGER END;
CONST Pixmap_Gray = Pixmap_T{1};

TYPE PaintOp_T = RECORD PaintOp_T_field:INTEGER END;
CONST PaintOp_Swap = PaintOp_T{2};
CONST PaintOp_TransparentSwap = PaintOp_T{3};

TYPE Point_T = RECORD Point_T_field: INTEGER END;

END VBT.
