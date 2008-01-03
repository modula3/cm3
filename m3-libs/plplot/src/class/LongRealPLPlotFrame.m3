MODULE LongRealPLPlotFrame;

IMPORT LongRealPLPlot AS PL,
       (*LongRealPLPlotStream AS Stream,*)
       LongRealPLPlotFigure AS Figure;
IMPORT RefSet, RefSeq, Math;


REVEAL
  T = Public BRANDED OBJECT
        stream: RefSet.T;        (* Stream.T; *)(*UNUSED*)
      END;

  Single = SinglePublic BRANDED OBJECT
             id                     : CARDINAL;
             figures                : RefSeq.T;  (* Figure.T *)
             color                  : CARDINAL;
             sizeX, sizeY           : LONGREAL;
             marginLeft, marginRight: LONGREAL;
             marginBottom, marginTop: LONGREAL;
             (* margin between cell border and the actual plot window,
                measured in character heights *)

             range         : Figure.Box;
             spareX, spareY: LONGREAL     := 0.1D0;
             auto := DirSet{Dir.Horizontal, Dir.Vertical};
             boxTilesX, boxTilesY: PL.DirTileSet;

           OVERRIDES
             init := SingleInit;
             draw := SingleDraw;
             (* add := SingleAdd; <* OBSOLETE *> *)

             setRange  := SingleSetRange;
             setRangeX := SingleSetRangeX;
             setRangeY := SingleSetRangeY;
             getRange  := SingleGetRange;

             setRelSizeX := SingleSetRelSizeX;
             setRelSizeY := SingleSetRelSizeY;
             getSizeX    := SingleGetSizeX;
             getSizeY    := SingleGetSizeY;
           END;

  Matrix = MatrixPublic BRANDED OBJECT
             subFrames: REF ARRAY OF ARRAY OF T;
           OVERRIDES
             init := MatrixInit;
             draw := MatrixDraw;

             getSizeX := MatrixGetSizeX;
             getSizeY := MatrixGetSizeY;
           END;


TYPE
  Dir = {Horizontal, Vertical};
  DirSet = SET OF Dir;

PROCEDURE SingleInit
  (SELF: Single; READONLY figures: ARRAY OF Figure.T; color: CARDINAL; ):
  Single =
  BEGIN
    SELF.color := color;
    SELF.figures := NEW(RefSeq.T).init(NUMBER(figures));
    FOR i := FIRST(figures) TO LAST(figures) DO
      SELF.figures.addhi(figures[i]);
    END;

    SELF.boxTilesX :=
      PL.DirTileSet{PL.DirTile.LowerBorder, PL.DirTile.UpperBorder,
                    PL.DirTile.TickLabelsConv, PL.DirTile.TicksMajor,
                    PL.DirTile.TicksMinor};
    SELF.boxTilesY :=
      SELF.boxTilesX + PL.DirTileSet{PL.DirTile.LabelBaseParallel};

    SELF.sizeX := 1.0D0;
    SELF.sizeY := Math.sqrt(0.5D0);
    SELF.marginLeft := 0.0D0;
    SELF.marginRight := 0.0D0;
    SELF.marginBottom := 0.0D0;
    SELF.marginTop := 0.0D0;
    IF PL.DirTile.TickLabelsConv IN SELF.boxTilesX THEN
      SELF.marginLeft := SELF.marginLeft + 8.5D0;
    END;
    IF PL.DirTile.TickLabelsUnconv IN SELF.boxTilesX THEN
      SELF.marginRight := SELF.marginRight + 8.5D0;
    END;
    IF PL.DirTile.TickLabelsConv IN SELF.boxTilesY THEN
      SELF.marginBottom := SELF.marginBottom + 4.0D0;
    END;
    IF PL.DirTile.TickLabelsUnconv IN SELF.boxTilesY THEN
      SELF.marginTop := SELF.marginTop + 4.0D0;
    END;
    RETURN SELF;
  END SingleInit;

<* OBSOLETE *>
PROCEDURE SingleAdd (SELF: Single; figure: Figure.T; ) =
  BEGIN
    SELF.figures.addhi(figure);
  END SingleAdd;

PROCEDURE SingleSetRange (SELF: Single; range: Figure.Box; ) =
  BEGIN
    SELF.range := range;
    SELF.auto := DirSet{};
  END SingleSetRange;

PROCEDURE SingleSetRangeX (SELF: Single; range: Figure.Range; ) =
  BEGIN
    SELF.range.x := range;
    SELF.auto := SELF.auto - DirSet{Dir.Horizontal};
  END SingleSetRangeX;

PROCEDURE SingleSetRangeY (SELF: Single; range: Figure.Range; ) =
  BEGIN
    SELF.range.y := range;
    SELF.auto := SELF.auto - DirSet{Dir.Vertical};
  END SingleSetRangeY;

PROCEDURE SingleSetRelSizeX (SELF: Single; size: LONGREAL; ) =
  BEGIN
    SELF.sizeX := size;
  END SingleSetRelSizeX;

PROCEDURE SingleSetRelSizeY (SELF: Single; size: LONGREAL; ) =
  BEGIN
    SELF.sizeY := size;
  END SingleSetRelSizeY;

PROCEDURE SingleGetSizeX (SELF: Single; ): LONGREAL =
  VAR
    chr := PL.GetCharacterHeight();
    bnd := PL.GetBoundaries();
  BEGIN
    RETURN (SELF.marginLeft + SELF.marginRight) * chr.ht
             + (bnd.xmax - bnd.xmin) * SELF.sizeX;
  END SingleGetSizeX;

PROCEDURE SingleGetSizeY (SELF: Single; ): LONGREAL =
  VAR
    chr := PL.GetCharacterHeight();
    bnd := PL.GetBoundaries();
  BEGIN
    RETURN (SELF.marginBottom + SELF.marginTop) * chr.ht
             + (bnd.ymax - bnd.ymin) * SELF.sizeY;
  END SingleGetSizeY;


PROCEDURE ExtendRange (rng: Figure.Range; spare: LONGREAL; ):
  Figure.Range =
  BEGIN
    WITH size = rng.max - rng.min DO
      RETURN Figure.Range{rng.min - size * spare, rng.max + size * spare};
    END;
  END ExtendRange;

PROCEDURE RangeUnion (rng0, rng1: Figure.Range; ): Figure.Range =
  BEGIN
    RETURN Figure.Range{MIN(rng0.min, rng1.min), MAX(rng0.max, rng1.max)};
  END RangeUnion;

PROCEDURE SingleGetRange (SELF: Single; ): Figure.Box =
  VAR box, curBox: Figure.Box;
  BEGIN
    IF SELF.auto # DirSet{} THEN
      box := NARROW(SELF.figures.get(0), Figure.T).getRange();
      FOR i := 1 TO SELF.figures.size() - 1 DO
        curBox := NARROW(SELF.figures.get(i), Figure.T).getRange();
        box.x := RangeUnion(box.x, curBox.x);
        box.y := RangeUnion(box.y, curBox.y);
      END;
    END;
    IF Dir.Horizontal IN SELF.auto THEN
      box.x := ExtendRange(box.x, SELF.spareX);
    ELSE
      box.x := SELF.range.x;
    END;
    IF Dir.Vertical IN SELF.auto THEN
      box.y := ExtendRange(box.y, SELF.spareY);
    ELSE
      box.y := SELF.range.y;
    END;
    RETURN box;
  END SingleGetRange;

PROCEDURE SingleDraw (SELF: Single; x, y: LONGREAL; ) =
  VAR
    box := SELF.getRange();
    chr := PL.GetCharacterHeight();
    bnd := PL.GetBoundaries();

  BEGIN
    x := x + chr.ht * SELF.marginLeft;
    y := y + chr.ht * SELF.marginBottom;
    PL.SetVPAbsolute(x, x + (bnd.xmax - bnd.xmin) * SELF.sizeX, y,
                     y + (bnd.ymax - bnd.ymin) * SELF.sizeY);
    PL.SetFGColorDiscr(SELF.color);
    PL.SetWindow(box.x.min, box.x.max, box.y.min, box.y.max);
    PL.DrawBox(SELF.boxTilesX, 0.0D0, 0, SELF.boxTilesY, 0.0D0, 0);
    FOR i := 0 TO SELF.figures.size() - 1 DO
      NARROW(SELF.figures.get(i), Figure.T).draw();
    END;
  END SingleDraw;



PROCEDURE MatrixInit
  (SELF: Matrix; READONLY frames: ARRAY OF ARRAY OF T; ): Matrix =
  BEGIN
    SELF.subFrames :=
      NEW(REF ARRAY OF ARRAY OF T, NUMBER(frames), NUMBER(frames[0]));
    SELF.subFrames^ := frames;
    RETURN SELF;
  END MatrixInit;

PROCEDURE MatrixDraw (SELF: Matrix; x, y: LONGREAL; ) =
  VAR
    xc := NEW(REF ARRAY OF LONGREAL, NUMBER(SELF.subFrames^));
    yc := NEW(REF ARRAY OF LONGREAL, NUMBER(SELF.subFrames[0]));

  BEGIN
    FOR column := FIRST(SELF.subFrames^) TO LAST(SELF.subFrames^) DO
      xc[column] := x;
      x := x + ColumnWidth(SELF.subFrames[column]);
    END;
    FOR row := FIRST(SELF.subFrames[0]) TO LAST(SELF.subFrames[0]) DO
      yc[row] := y;
      y := y + RowHeight(SELF.subFrames^, row);
    END;

    FOR column := FIRST(SELF.subFrames^) TO LAST(SELF.subFrames^) DO
      FOR row := FIRST(SELF.subFrames[0]) TO LAST(SELF.subFrames[0]) DO
        SELF.subFrames[column, row].draw(xc[column], yc[row]);
      END;
    END;
  END MatrixDraw;


PROCEDURE RowHeight
  (READONLY matrix: ARRAY OF ARRAY OF T; row: CARDINAL; ): LONGREAL =
  VAR size := 0.0D0;
  BEGIN
    FOR i := FIRST(matrix) TO LAST(matrix) DO
      size := MAX(size, matrix[i, row].getSizeY());
    END;
    RETURN size;
  END RowHeight;

PROCEDURE ColumnWidth (READONLY column: ARRAY OF T; ): LONGREAL =
  VAR size := 0.0D0;
  BEGIN
    FOR i := FIRST(column) TO LAST(column) DO
      size := MAX(size, column[i].getSizeX());
    END;
    RETURN size;
  END ColumnWidth;

(* if the size is not stored we will run this computation multiple times *)
PROCEDURE MatrixGetSizeX (SELF: Matrix; ): LONGREAL =
  VAR size := 0.0D0;
  BEGIN
    FOR i := FIRST(SELF.subFrames^) TO LAST(SELF.subFrames^) DO
      size := size + ColumnWidth(SELF.subFrames[i]);
    END;
    RETURN size;
  END MatrixGetSizeX;

PROCEDURE MatrixGetSizeY (SELF: Matrix; ): LONGREAL =
  VAR size := 0.0D0;
  BEGIN
    FOR i := FIRST(SELF.subFrames^) TO LAST(SELF.subFrames^) DO
      size := size + RowHeight(SELF.subFrames^, i);
    END;
    RETURN size;
  END MatrixGetSizeY;



BEGIN
END LongRealPLPlotFrame.
