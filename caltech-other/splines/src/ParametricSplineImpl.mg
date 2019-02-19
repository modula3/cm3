(* $Id$ *)

GENERIC MODULE ParametricSplineImpl(BaseSpline);
IMPORT ParametricSpline, ParametricSplineRep;
FROM ParametricSpline IMPORT PlotStyle, Coord;
IMPORT Math;
IMPORT Debug,Fmt;

REVEAL
  T = ParametricSpline.T BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
    getParametricPoint := GetParametricPoint;
    gnuPlotFormat := GnuPlotFormat;
  END;

PROCEDURE FormatCoord(c : Coord) : TEXT = 
  BEGIN 
    RETURN "(" & Fmt.LongReal(c.x) & "," & Fmt.LongReal(c.y) & ")" 
  END FormatCoord;

PROCEDURE Init(self : T; READONLY coords : ARRAY OF Coord) : ParametricSpline.T =

  PROCEDURE ComputeWeight(READONLY a, b : Coord) : LONGREAL =
    CONST MinW = 1.0d-8; (* a bit of a hack... *)
    VAR
      w : LONGREAL;
    BEGIN
      (* compute the square root of the chord length *)
      w := Math.pow((a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y),0.25d0);

      (* the following guarantees strictly increasing parameter values *)
      IF w = 0.0d0 THEN w := MinW END;
      
      RETURN w

    END ComputeWeight;

  VAR
    wTot := 0.0d0;
    px, py := NEW(REF ARRAY OF Coord,NUMBER(coords));
  BEGIN
    <* ASSERT NUMBER(coords) > 1 *> (* is this enough? *)
    FOR i := FIRST(coords) TO LAST(coords) - 1 DO
      wTot := wTot + ComputeWeight(coords[i],coords[i+1])
    END;

    VAR
      cur := 0.0d0;
    BEGIN
      px[0].x := cur; px[0].y := coords[0].x;
      Debug.Out("Parametric px[0] at " & FormatCoord(px[0]));
      py[0].x := cur; py[0].y := coords[0].y;
      Debug.Out("Parametric py[0] at " & FormatCoord(px[0]));

      FOR i := FIRST(coords) + 1 TO LAST(coords) DO
        cur := cur + ComputeWeight(coords[i-1],coords[i])/wTot;

        px[i].x := cur; px[i].y := coords[i].x;
        Debug.Out("Parametric px["&Fmt.Int(i)&"] at " & FormatCoord(px[i]));
        py[i].x := cur; py[i].y := coords[i].y;
        Debug.Out("Parametric py["&Fmt.Int(i)&"] at " & FormatCoord(py[i]));

      END
    END;

    (* build underlying spline functions *)

    self.xSpline := NEW(BaseSpline.T).init(px^);
    self.ySpline := NEW(BaseSpline.T).init(py^);
    
    RETURN self
  END Init;

PROCEDURE GetParametricPoint(self : T; param : LONGREAL) : Coord =
  BEGIN
    (*<* ASSERT 0.0d0-EPS <= param AND param <= 1.0d0+EPS *>*)
    (* Ok: spline is only valid from -1 to 1, but there's no real objection
       to evaluating outside.  Needed for num. derivatives anyhow. *)
    RETURN Coord { self.xSpline.eval(param), self.ySpline.eval(param) }
  END GetParametricPoint;

PROCEDURE GnuPlotFormat(self : T; 
                        steps : CARDINAL; plotStyle : PlotStyle) : TEXT =
  VAR 
    res := "";
  BEGIN
    (* eval at steps + 1 points *)
    FOR i := 0 TO steps DO
      VAR
        p := FLOAT(i,LONGREAL)/FLOAT(steps,LONGREAL);
        pt := self.getParametricPoint(p);
      BEGIN
        IF plotStyle = PlotStyle.Parametric THEN
          res := res & Fmt.LongReal(pt.x) & " " & Fmt.LongReal(pt.y) & "\n"
        ELSIF plotStyle = PlotStyle.X THEN
          res := res & Fmt.LongReal(p) & " " & Fmt.LongReal(pt.x) & "\n"
        ELSIF plotStyle = PlotStyle.Y THEN
          res := res & Fmt.LongReal(p) & " " & Fmt.LongReal(pt.y) & "\n"
        ELSE
          <* ASSERT FALSE *>
        END
      END
    END;
    RETURN res
  END GnuPlotFormat;

BEGIN END ParametricSplineImpl.
