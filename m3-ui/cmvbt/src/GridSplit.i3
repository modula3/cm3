
(* Copyright 1997 Critical Mass, Inc. All Rights Reserved.
   See file COPYRIGHT for full description. *)

INTERFACE GridSplit;

(* A "GridSplit" is a split which lays its
   children on a rectangular grid. The size
   of the grid is not determined by the 
   children, however. *)

IMPORT VBT, Pixmap, PaintOp, Point, Split, Rect;

TYPE
  T <: Public;
  Private <: Split.T;
  Public = Private OBJECT METHODS
    init(nrows, ncols: CARDINAL := 0;
         rowsize : CARDINAL := 14;
         colsize : CARDINAL := 150;
         margin   : CARDINAL := 1;
         bg : PaintOp.T := PaintOp.Bg;
         fg : PaintOp.T := PaintOp.Fg;
         txt: Pixmap.T := Pixmap.Solid): T;
  END;
  (* The call "v.init" initializes "v" with dimentions "nrows" times
     "ncols". Row and column size is specified by "rowsize" and
     "colsize", and the margin between the cells in the grid is
     specified by "margin". The paint operations "bg" and "fg" and
     the texture "txt". *)


PROCEDURE Put(v: T; row, col: CARDINAL; ch: VBT.T);
  (* Put "ch" at the cell "[row,col]" *)

PROCEDURE Get(v: T; row, col: CARDINAL): VBT.T;
  (* Get the contents of the cell "[row,col]" *)

PROCEDURE InsertRow(v: T; row: CARDINAL; READONLY data: ARRAY OF VBT.T);
  (* Replace the conntents of "row" with the specified "data", move
     everything, including the original content of "row" down one
     row. *)

PROCEDURE RemoveRow(v: T; row: CARDINAL; VAR data: ARRAY OF VBT.T);
  (* Remove contents of "row", moving contents all rows after up one row. *)

PROCEDURE SwapRows(v: T; r1, r2: CARDINAL);
  (* Swap contents of rows "r1" and "r2". *)

PROCEDURE GetRow(v: T; row: CARDINAL; VAR data: ARRAY OF VBT.T);
  (* Set "data" to be the contents of "row". It is a checked runtime
     error for "row" to be greater than the number of rows in "v". *)

PROCEDURE PutRow(v: T; row: CARDINAL; READONLY data: ARRAY OF VBT.T);
  (* Put "data" as content of row "row". It is a checked run-time
     error if row is greater than number of rows in "v". *)

PROCEDURE GrowRows(v: T; nrows: CARDINAL := 1);
  (* Adds "nrows" to the number of rows in "v". *)


PROCEDURE Dim (v: T; VAR nrows, ncols: CARDINAL);
PROCEDURE NumRows(v: T): CARDINAL;
PROCEDURE NumCols(v: T): CARDINAL;
(* "Dim", "NumRows" and "NumCols" return the number of 
   rows and columns for "v". *)

PROCEDURE GetColWidth (v: T; col: CARDINAL): CARDINAL;
PROCEDURE SetColWidth (v: T; col: CARDINAL; width: CARDINAL);
(* Get and set column with for column "col". *)

PROCEDURE GetRowHeight (v: T; row: CARDINAL): CARDINAL;
PROCEDURE SetRowHeight (v: T; row: CARDINAL; height: CARDINAL);
(* Get and set height for "row" in "v". *)

PROCEDURE GetCoord (v: T;
                    top: CARDINAL := 0;
                    bottom: CARDINAL := LAST(CARDINAL);
                    left: CARDINAL := 0;
                    right: CARDINAL := LAST(CARDINAL)): Rect.T;
(* Return the rectangle specified by row and column pairs "[top,left]"
     and "[bottom,right]". *)


PROCEDURE Locate(v: T; p: Point.T): Location;
(* Find out the location of a point within the VBT. If 
   the point is within the outer left or top margin,
   "row" or "col" may be set to -1. The type "Location"
   describes where, within "v" is the cursor located. *)

TYPE
  Placement = { VerMargin, HorMargin, InsideCell, CrossMargins, None };
  Location = RECORD
    row, col: [-1..LAST(CARDINAL)] := -1;
    pl: Placement;
  END;

END GridSplit.
