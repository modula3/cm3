(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AdjMatrixVBT;

IMPORT VBT, PaintOp;

TYPE T <: Public;
  Public = Prefix OBJECT
  METHODS
    init(n: INTEGER; grid := TRUE): T;
    setColor(n, m: INTEGER; color: PaintOp.T);
    getColor(n, m: INTEGER): PaintOp.T;
    setText(n, m: INTEGER; t: TEXT);
    getText(n, m: INTEGER): TEXT;
    setHighlight(n, m: INTEGER; color: PaintOp.T);
    getHighlight(n, m: INTEGER): PaintOp.T;
    setRowLabel(n: INTEGER; color: PaintOp.T);
    getRowLabel(n: INTEGER): PaintOp.T;
    setColumnLabel(m: INTEGER; color: PaintOp.T);
    getColumnLabel(m: INTEGER): PaintOp.T;
    display(state: BOOLEAN);
  END;

  Prefix <: VBT.T; (* => T <: VBT.T *)

END AdjMatrixVBT.
