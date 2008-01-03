MODULE LongRealPLPlotFigure;

IMPORT LongRealPLPlot          AS PL,
       LongRealPLPlotLineStyle AS LineStyle,
       IEEESpecial;

REVEAL
  PointSet = T BRANDED OBJECT
               x, y : REF PL.FloatVector;
               color: CARDINAL;
             OVERRIDES
               getRange := PointSetGetRange;
             END;

  Lines = LinesPublic BRANDED OBJECT
            style: LineStyle.T;
          OVERRIDES
            init            := LinesInit;
            initEquidistant := LinesInitEquidistant;
            draw            := LinesDraw;
          END;

  Points = PointsPublic BRANDED OBJECT
             code: INTEGER;
           OVERRIDES
             init := PointsInit;
             draw := PointsDraw;
           END;



(* copied from arithmetic/VectorFast.mg *)
PROCEDURE Max (READONLY x: PL.FloatVector; ): LONGREAL =
  VAR max := IEEESpecial.LongNegInf;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO max := MAX(max, x[i]); END;
    RETURN max;
  END Max;

PROCEDURE Min (READONLY x: PL.FloatVector; ): LONGREAL =
  VAR min := IEEESpecial.LongPosInf;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO min := MIN(min, x[i]); END;
    RETURN min;
  END Min;


PROCEDURE PointSetGetRange (SELF: PointSet; ): Box =
  BEGIN
    RETURN Box{Range{Min(SELF.x^), Max(SELF.x^)},
               Range{Min(SELF.y^), Max(SELF.y^)}};
  END PointSetGetRange;

PROCEDURE LinesInit (SELF : Lines;
                     x, y : REF PL.FloatVector;
                     color: CARDINAL;
                     style: LineStyle.T;        ): Lines =
  BEGIN
    <* ASSERT NUMBER(x^) = NUMBER(y^) *>
    SELF.x := x;
    SELF.y := y;
    SELF.color := color;
    IF style = NIL THEN
      SELF.style := LineStyle.Continuous;
    ELSE
      SELF.style := style;
    END;
    RETURN SELF;
  END LinesInit;

PROCEDURE LinesInitEquidistant (SELF    : Lines;
                                y       : REF PL.FloatVector;
                                from, by: PL.Float;
                                color   : CARDINAL;
                                style   : LineStyle.T;        ): Lines =
  VAR x := NEW(REF PL.FloatVector, NUMBER(y^));
  BEGIN
    (* similar to arithmetic/VectorFast.ArithSeq *)
    FOR i := FIRST(x^) TO LAST(x^) DO x[i] := from; from := from + by; END;
    RETURN LinesInit(SELF, x, y, color, style);
  END LinesInitEquidistant;

PROCEDURE LinesDraw (SELF: Lines; ) =
  BEGIN
    SELF.style.apply();
    PL.SetFGColorDiscr(SELF.color);
    PL.PlotLines(SELF.x^, SELF.y^);
  END LinesDraw;


PROCEDURE PointsInit (SELF : Points;
                      x, y : REF PL.FloatVector;
                      code : INTEGER;
                      color: CARDINAL;           ): Points =
  BEGIN
    <* ASSERT NUMBER(x^) = NUMBER(y^) *>
    SELF.x := x;
    SELF.y := y;
    SELF.code := code;
    SELF.color := color;
    RETURN SELF;
  END PointsInit;

PROCEDURE PointsDraw (SELF: Points; ) =
  BEGIN
    PL.SetFGColorDiscr(SELF.color);
    PL.PlotPoints(SELF.x^, SELF.y^, SELF.code);
  END PointsDraw;


BEGIN
END LongRealPLPlotFigure.
