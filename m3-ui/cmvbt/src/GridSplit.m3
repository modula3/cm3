
(* Copyright 1996-2000 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE GridSplit;
IMPORT VBT, Split, ProperSplit, VBTClass, Region, Rect;
IMPORT Point, PaintOp, Axis, Pixmap;

REVEAL
  Private = ProperSplit.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
    tbl : REF ARRAY OF ARRAY OF VBT.T := NIL;
    nrows, ncols: [0..LAST(CARDINAL)];
    rowsize, colsize: CARDINAL := 0;
    colsz: REF ARRAY OF CARDINAL;
    rowsz: REF ARRAY OF CARDINAL;
    margin: CARDINAL := 1;
    txt: Pixmap.T;
    bg:  PaintOp.T;
    op:  PaintOp.T;
  OVERRIDES
    init      := Init;
    repaint   := Repaint;  
    reshape   := Reshape;
    shape     := Shape;
    rescreen  := Rescreen;
  END;

CONST
  SizeIncrement = 10;

PROCEDURE Init(v: T; 
               nrows, ncols: CARDINAL;
               rowsize, colsize: CARDINAL;
               margin: CARDINAL;
               bg: PaintOp.T := PaintOp.Fg;
               op: PaintOp.T := PaintOp.Bg;
               txt: Pixmap.T := Pixmap.Solid): T =
  BEGIN

    v.margin := margin;
    v.txt := txt;
    v.op := op;
    v.bg := bg;
    v.tbl := NEW(REF ARRAY OF ARRAY OF VBT.T, nrows, ncols); 
    v.nrows := nrows;
    v.ncols := ncols;
    v.rowsz := NEW(REF ARRAY OF CARDINAL, nrows);
    v.colsz := NEW(REF ARRAY OF CARDINAL, ncols);

    v.rowsize := rowsize;
    v.colsize := colsize;

    FOR row := 0 TO nrows-1 DO
      v.rowsz[row] := rowsize;
    END;

    FOR col := 0 TO ncols-1 DO
      v.colsz[col] := colsize;
    END;

    VBT.PaintTexture (v, VBT.Domain(v), v.bg, v.txt);

    RETURN v;
  END Init;

(*

<*UNUSED*> PROCEDURE DumpSizes (v: T) =
BEGIN 

  IO.Put (Fmt.F ("Domain: (%s,%s)-(%s,%s)\n" , Fmt.Int(v.domain.west), 
                              Fmt.Int(v.domain.east), 
                              Fmt.Int(v.domain.north), 
                              Fmt.Int(v.domain.south)));
  IO.Put ("Rows : ");
  FOR i := 0 TO v.nrows-1 DO
    IO.PutInt(v.rowsz[i]); IO.Put ("=");
    IO.PutInt (v.tbl[i,0].domain.north);
    IO.Put (" ");
  END;
  IO.Put ("\n");

  IO.Put ("Cols : ");
  FOR i := 0 TO v.ncols-1 DO
    IO.PutInt(v.colsz[i]); IO.Put ("=");
    IO.PutInt (v.tbl[0,i].domain.west); IO.Put (",");
    IO.PutInt (v.tbl[0,i].domain.east); IO.Put (" ");
    IO.Put (" ");
  END;
  IO.Put ("\n\n");

END DumpSizes;
*)


PROCEDURE Put(v: T; row, col: CARDINAL; ch: VBT.T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    <* ASSERT row < v.nrows *>
    <* ASSERT col < v.ncols *>

    IF v.tbl[row,col] # NIL THEN
      Split.Delete (v, v.tbl[row,col]);
    END;
    v.tbl[row,col] := ch;
    IF ch # NIL THEN
      Split.AddChild (v, ch);
      VBT.NewShape(v);
    END;
  END Put;

PROCEDURE Get(v: T; row, col: CARDINAL): VBT.T =
  BEGIN
    RETURN v.tbl[row,col];
  END Get;

PROCEDURE GetRow (v: T; row: CARDINAL; VAR data: ARRAY OF VBT.T) =
  BEGIN
    data := v.tbl[row];
  END GetRow;

PROCEDURE PutRow(v: T; row: CARDINAL; READONLY data: ARRAY OF VBT.T) =
  BEGIN
    FOR col := FIRST(data) TO LAST(data) DO
      Put(v, row, col, data[col]);
    END;
  END PutRow;

PROCEDURE InsertRow(v: T; row: CARDINAL; READONLY data: ARRAY OF VBT.T) =
  VAR
    orig_nrows := NumRows(v);
  BEGIN
    GrowRows(v);
    FOR i := orig_nrows-1 TO row BY -1 DO
      v.tbl[i+1] := v.tbl[i];
    END;
    v.tbl[row] := data;
    Split.AddChildArray (v, data);
  END InsertRow;

PROCEDURE SwapRows(v: T; r1,r2: CARDINAL) = 
  VAR
    temp := NEW(REF ARRAY OF VBT.T, NUMBER(v.tbl[r1]));
  BEGIN
    temp^ := v.tbl[r1];
    v.tbl[r1] := v.tbl[r2];
    v.tbl[r2] := temp^;
    VBT.Mark(v);
  END SwapRows;

PROCEDURE RemoveRow(v: T; row: CARDINAL; VAR deleted: ARRAY OF VBT.T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    deleted := v.tbl[row];
    FOR i := row TO v.nrows-2 DO
      v.tbl[i] := v.tbl[i+1];
    END;
    DEC(v.nrows);
    FOR i := FIRST(deleted) TO LAST(deleted) DO
      Split.Delete (v, deleted[i]);
    END;
    VBT.NewShape(v);
  END RemoveRow;

(*PROCEDURE RemoveRow (v: T; start, n: CARDINAL) = 
  (* Remove "n" rows starting with "start". *)
  BEGIN
    SUBARRAY(v.tbl, start, n) := SUBARRAY(v.tbl, start + n, n);
    VBT.NewShape(v);
  END RemoveRow;
*)

PROCEDURE GetRowHeight (v: T; row: CARDINAL): CARDINAL =
  BEGIN
    RETURN v.rowsz[row];
  END GetRowHeight;

PROCEDURE GetColWidth (v: T; col: CARDINAL): CARDINAL =
  BEGIN
    RETURN v.colsz[col];
  END GetColWidth;

PROCEDURE SetColWidth (v: T; col: CARDINAL; width: CARDINAL) = 
  BEGIN
    v.colsz[col] := width;
    VBT.NewShape(v);
  END SetColWidth;

PROCEDURE Dim(v: T; VAR nrows, ncols: CARDINAL) =
  BEGIN
    nrows := v.nrows;
    ncols := v.ncols;
  END Dim;

PROCEDURE NumRows(v: T): CARDINAL = 
  BEGIN
    RETURN v.nrows;
  END NumRows;

PROCEDURE NumCols(v: T): CARDINAL = 
  BEGIN
    RETURN v.ncols;
  END NumCols;

PROCEDURE GrowRows(v: T; nrows: CARDINAL := 1) =
  VAR
    oldtbl  := v.tbl;
  BEGIN
    IF NUMBER(v.rowsz^) >= v.nrows THEN
      WITH incrows = MAX (SizeIncrement, nrows) DO
        v.rowsz := NEW(REF ARRAY OF CARDINAL, v.nrows + incrows);
        v.tbl := NEW(REF ARRAY OF ARRAY OF VBT.T, v.nrows + incrows, v.ncols);
      END;
    END;

    FOR row := 0 TO v.nrows-1 DO
      v.tbl[row] := oldtbl[row];
    END;
   
    INC(v.nrows, nrows);

    FOR row := v.nrows-nrows TO v.nrows-1 DO
      FOR col := 0 TO v.ncols-1 DO
        v.tbl[row,col] := NIL;
      END;
    END;

    FOR row := 0 TO v.nrows-1 DO
      v.rowsz[row] := v.rowsize;
    END;

    VBT.NewShape(v);
 END GrowRows;

PROCEDURE GetCoord (v: T; top, bottom, left, right: CARDINAL): Rect.T =
  VAR
    res := VBT.Domain(v);
  BEGIN
    res.north := res.north + v.margin;
    FOR row := 0 TO top-1 DO
      INC(res.north, v.rowsz[row] + v.margin);
    END;
    res.south := res.north;
    FOR row := top TO MIN(bottom, v.nrows-1) DO
      INC(res.south, v.rowsz[row] + v.margin);
    END;
    DEC (res.south, v.margin);

    res.west := res.west + v.margin;
    FOR col := 0 TO left-1 DO
      INC(res.west, v.colsz[col] + v.margin);
    END;
    res.east := res.west;
    FOR col := left TO MIN(right, v.ncols-1) DO
      INC(res.east, v.colsz[col] + v.margin);
    END;
    DEC (res.east, v.margin);
    
    RETURN res;
  END GetCoord;

PROCEDURE SetRowHeight (v: T; row: CARDINAL; height: CARDINAL) =
  BEGIN
    v.rowsz[row] := height;
    VBT.NewShape(v);
  END SetRowHeight;

PROCEDURE Width(v: T): CARDINAL =
  VAR
    res := v.margin * (v.ncols + 1);
  BEGIN
    FOR i := 0 TO v.ncols - 1 DO 
      INC (res, v.colsz[i]);
    END;
    RETURN res;
  END Width; 

PROCEDURE Height(v: T): CARDINAL =
  VAR
    res := v.margin * (v.nrows + 1);
  BEGIN
    FOR i := 0 TO v.nrows - 1 DO 
      INC (res, v.rowsz[i]);
    END;
    RETURN res;
  END Height; 

(****
PROCEDURE OutR (tag: TEXT;  READONLY r: Rect.T) =
  BEGIN
    IO.Put (tag & ": [" & Fmt.Int (r.west) & ", " & Fmt.Int (r.north) & "] -> ["
               & Fmt.Int (r.east - r.west) & ", " & Fmt.Int (r.south - r.north)
               & "]\r\n");
  END OutR;
****)

PROCEDURE Repaint(v: T; READONLY br: Region.T) RAISES {} =
  VAR
    dom := VBT.Domain(v);
    bigbad := Region.BoundingBox (br);
    r: Rect.T;
    height := Height(v);
    width  := Width(v);
  BEGIN
    (* erase any bits outside the table... *)
    IF (dom.north + height < dom.south) THEN
      r := dom;  INC(r.north, height);
      r := Rect.Meet(r, bigbad);
      IF NOT Rect.IsEmpty (r) THEN  VBT.PaintTexture (v, r, v.bg, v.txt); END;
    END;
    IF (dom.west + width < dom.east) THEN
      r := dom;  INC(r.west, width);
      r := Rect.Meet(r, bigbad);
      IF NOT Rect.IsEmpty (r) THEN  VBT.PaintTexture (v, r, v.bg, v.txt); END;
    END;

    (* Horizontal repainting; slide the margin bar
       through, painting only the band which was
       covered by the bad region. *)

    r := Rect.Stretch (dom, Axis.T.Hor, 
                       dom.west, dom.west + v.margin);
    VBT.PaintTexture (v, Rect.Meet (r, bigbad), v.bg, v.txt);
    FOR i := 0 TO v.ncols-1 DO
      r := Rect.MoveH (r, v.margin + v.colsz[i]);
      IF r.east >= bigbad.west AND r.west < bigbad.east THEN
        VBT.PaintTexture (v, Rect.Meet(r, bigbad), v.bg, v.txt);
      END;
    END;

    (* Vertical repainting; slide the margin bar
       through, painting only the band which was
       covered by the bad region. *)

    r := Rect.Stretch (dom, Axis.T.Ver, 
                       dom.north, dom.north + v.margin);
    VBT.PaintTexture (v, Rect.Meet(r, bigbad), v.bg, v.txt);
    FOR i := 0 TO v.nrows-1 DO
      r := Rect.MoveV (r, v.margin + v.rowsz[i]);
      IF r.south >= bigbad.north AND r.north < bigbad.south THEN
        VBT.PaintTexture (v, Rect.Meet(r, bigbad), v.bg, v.txt);
      END;
    END;

    FOR row := 0 TO v.nrows-1 DO
      FOR col := 0 TO v.ncols-1 DO
        IF v.tbl[row,col] # NIL AND 
           Rect.Meet(VBT.Domain(v.tbl[row,col]), bigbad) # Rect.Empty THEN
             VBT.ForceRepaint(v.tbl[row,col], br);
        END;
      END;
    END;

  END Repaint;


PROCEDURE Rescreen(v: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  BEGIN
    VBT.NewShape(v);
    Split.T.rescreen(v, cd)
  END Rescreen;


PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} = 
  BEGIN
    VBTClass.LocateChanged(v);
    Reshape2(v, saved := cd.saved);
    IF NOT Rect.IsEmpty (cd.new) THEN Repaint (v, Region.FromRect(cd.new)) END;
  END Reshape;

PROCEDURE Reshape2(v: T; irow, icol : CARDINAL := 0;
                   saved: Rect.T := Rect.Empty) RAISES {} =
  VAR
    changed := FALSE;
    dom := VBT.Domain(v);
    voff: INTEGER := dom.north;
    hoff: INTEGER := dom.west;
  BEGIN
    FOR row := irow TO v.nrows-1 DO
      INC (voff, v.margin); hoff := dom.west;
      FOR col := icol TO v.ncols-1 DO 
        INC (hoff, v.margin);
        WITH ch = v.tbl[row,col] DO
          VAR
            new := Rect.T { north := voff, 
                            south := voff + v.rowsz[row],
                            west :=  hoff, 
                            east :=  hoff + v.colsz[col] }; 
          BEGIN
            IF ch # NIL THEN
              IF new # VBT.Domain(ch) THEN
                VBTClass.Reshape (ch, new, saved);
                VBT.Mark (ch);
                changed := TRUE;
              END; 
            END; 
          END; 
        END;
        INC (hoff, v.colsz[col]);
      END;
      INC (voff, v.rowsz[row]);
    END;
    IF changed THEN VBT.Mark (v) END;
  END Reshape2;

PROCEDURE Shape (v: T; ax: Axis.T; 
                 <*UNUSED*>n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR sz: CARDINAL;
  BEGIN
    IF ax = Axis.T.Ver
      THEN sz := Height(v);
      ELSE sz := Width(v);
    END;
    RETURN VBT.SizeRange { sz, sz, sz+1 };
  END Shape;

PROCEDURE Locate (v: T; p: Point.T): Location =
  VAR
    loc := Location { pl := Placement.None, row := 0, col := 0};
    voff, hoff: INTEGER;
  CONST
    slop: INTEGER = 2;
  BEGIN

    (* Question: what happens to the Domain? 
       Answer, you have to keep it in mind! *)

      IF p.h <= v.domain.west + v.margin AND
         p.v <= v.domain.north + v.margin THEN
        loc.pl := Placement.CrossMargins; loc.row := -1; loc.col := -1;
      ELSIF p.h <= v.domain.west + v.margin THEN
        loc.pl := Placement.VerMargin; loc.col := -1;
      ELSIF p.v <= v.domain.north + v.margin THEN
        loc.pl := Placement.HorMargin;
        loc.row := -1; 
      END;

    (* Find the bottom-most and west-most margin
       which contains the point in question. *)

    voff := Height(v) + v.domain.north;
    FOR row := v.nrows-1 TO 0 BY -1 DO
      IF p.v > voff THEN
        loc.row := row+1;
        EXIT;
      END;
      DEC (voff, v.rowsz[row] + v.margin);
    END;

    hoff := Width(v) + v.domain.west;
    FOR col := v.ncols-1 TO 0 BY -1 DO
      IF p.h > hoff THEN
        loc.col := col+1;
        EXIT;
      END;
      DEC (hoff, v.colsz[col] + v.margin);
    END;

    IF loc.row < 0 OR loc.col < 0 OR 
       loc.row >= v.nrows OR loc.col >= v.ncols 
    THEN RETURN loc; 
    END;

    (* 0 < loc.col < v.ncols  and 0 < loc.rows < v.nrows *)

    IF p.h > hoff + v.colsz[loc.col] - slop THEN
      IF p.v > voff + v.rowsz[loc.row] - slop
        THEN loc.pl := Placement.CrossMargins;
        ELSE loc.pl := Placement.VerMargin;
      END;
    ELSE
      IF p.v > voff + v.rowsz[loc.row] - slop
        THEN loc.pl := Placement.HorMargin; 
        ELSE loc.pl := Placement.InsideCell;
      END;
    END;

    RETURN loc;
  END Locate;

BEGIN
END GridSplit.
