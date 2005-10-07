MODULE LongRealPLPlotFrame;

IMPORT LongRealPLPlot AS PL,
       (*LongRealPLPlotStream AS Stream,*)
       LongRealPLPlotFigure AS Figure;
IMPORT RefSet, RefSeq;


REVEAL
  T = Public BRANDED OBJECT
        stream: RefSet.T;        (* Stream.T; *)(*UNUSED*)
      END;

  Single =
    SinglePublic BRANDED OBJECT
      id     : CARDINAL;
      figures: RefSeq.T;         (* Figure.T *)
      color  : CARDINAL;
      box    : Figure.Box;
      aspect : LONGREAL;
      customAspect: BOOLEAN;     (* if true then respect aspect, otherwise
                                    let the default aspect unchanged *)
      spareH, spareV: LONGREAL := 0.1D0;
      auto                     := DirSet{Dir.Horizontal, Dir.Vertical};
    OVERRIDES
      init := SingleInit;
      draw := SingleDraw;
      (* add := SingleAdd; <* OBSOLETE *> *)
      setBox    := SingleSetBox;
      setRangeH := SingleSetRangeH;
      setRangeV := SingleSetRangeV;
      getBox    := SingleGetBox;
      setAspect := SingleSetAspect;
    END;

  Matrix = MatrixPublic BRANDED OBJECT
             subFrames: REF ARRAY OF ARRAY OF Single;
           OVERRIDES
             init := MatrixInit;
             draw := MatrixDraw;
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
    SELF.customAspect := FALSE;
    RETURN SELF;
  END SingleInit;

<* OBSOLETE *>
PROCEDURE SingleAdd (SELF: Single; figure: Figure.T; ) =
  BEGIN
    SELF.figures.addhi(figure);
  END SingleAdd;

PROCEDURE SingleSetBox (SELF: Single; box: Figure.Box; ) =
  BEGIN
    SELF.box := box;
    SELF.auto := DirSet{};
  END SingleSetBox;

PROCEDURE SingleSetRangeH (SELF: Single; rng: Figure.Range; ) =
  BEGIN
    SELF.box.x := rng;
    SELF.auto := SELF.auto - DirSet{Dir.Horizontal};
  END SingleSetRangeH;

PROCEDURE SingleSetRangeV (SELF: Single; rng: Figure.Range; ) =
  BEGIN
    SELF.box.y := rng;
    SELF.auto := SELF.auto - DirSet{Dir.Vertical};
  END SingleSetRangeV;

PROCEDURE SingleSetAspect (SELF: Single; aspect: LONGREAL; ) =
  BEGIN
    SELF.aspect := aspect;
    SELF.customAspect := TRUE;
  END SingleSetAspect;

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

PROCEDURE SingleGetBox (SELF: Single; ): Figure.Box =
  VAR box, curBox: Figure.Box;
  BEGIN
    IF SELF.auto # DirSet{} THEN
      box := NARROW(SELF.figures.get(0), Figure.T).getBox();
      FOR i := 1 TO SELF.figures.size() - 1 DO
        curBox := NARROW(SELF.figures.get(i), Figure.T).getBox();
        box.x := RangeUnion(box.x, curBox.x);
        box.y := RangeUnion(box.y, curBox.y);
      END;
    END;
    IF Dir.Horizontal IN SELF.auto THEN
      box.x := ExtendRange(box.x, SELF.spareH);
    ELSE
      box.x := SELF.box.x;
    END;
    IF Dir.Vertical IN SELF.auto THEN
      box.y := ExtendRange(box.y, SELF.spareV);
    ELSE
      box.y := SELF.box.y;
    END;
    RETURN box;
  END SingleGetBox;

PROCEDURE SingleDraw (SELF: Single; ) =
  VAR box := SELF.getBox();
  CONST
    flags = PL.DirTileSet{PL.DirTile.LowerBorder, PL.DirTile.UpperBorder,
                          PL.DirTile.LabelMajorConv, PL.DirTile.TicksMajor,
                          PL.DirTile.TicksMinor};
  BEGIN
    PL.SetFGColorDiscr(SELF.color);
    PL.SetStandardVP();
    IF SELF.customAspect THEN PL.SetVPAspect(SELF.aspect); END;
    PL.SetWindow(box.x.min, box.x.max, box.y.min, box.y.max);
    PL.DrawBox(
      flags, 0.0D0, 0, flags + PL.DirTileSet{PL.DirTile.LabelBaseParallel},
      0.0D0, 0);
    (* PL.SetEnvironment(box.x.min, box.x.max, box.y.min, box.y.max); *)
    FOR i := 0 TO SELF.figures.size() - 1 DO
      NARROW(SELF.figures.get(i), Figure.T).draw();
    END;
  END SingleDraw;


PROCEDURE MatrixInit
  (SELF: Matrix; READONLY frames: ARRAY OF ARRAY OF Single; ): Matrix =
  BEGIN
    SELF.subFrames :=
      NEW(REF ARRAY OF ARRAY OF Single, NUMBER(frames), NUMBER(frames[0]));
    SELF.subFrames^ := frames;
    RETURN SELF;
  END MatrixInit;

PROCEDURE MatrixDraw (SELF: Matrix; ) =
  VAR page: CARDINAL := 0;
  BEGIN
    PL.SetSubWindows(NUMBER(SELF.subFrames^), NUMBER(SELF.subFrames[0]));
    FOR column := FIRST(SELF.subFrames^) TO LAST(SELF.subFrames^) DO
      FOR row := FIRST(SELF.subFrames[0]) TO LAST(SELF.subFrames[0]) DO
        INC(page);
        PL.AdvanceSubPage(page);
        SELF.subFrames[column, row].draw();
      END;
    END;
  END MatrixDraw;



BEGIN
END LongRealPLPlotFrame.
