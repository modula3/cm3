(* Copyright (C)-ip992, Digital Equipment Corporation                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 15 13:54:11 PST 1996 by heydon                   *)
<* PRAGMA LL *>
<* PRAGMA SPEC *>

MODULE StarAnim EXPORTS Main;

IMPORT R2;
IMPORT VBT, Trestle, TrestleComm, DblBufferVBT, Rect, Region, Point, Path;
IMPORT   PaintOp, VBTClass, BorderedVBT, ButtonVBT, HVSplit, TextVBT, Axis;
IMPORT   TextureVBT, Font;
IMPORT Time;

<* FATAL TrestleComm.Failure *>

CONST
  Lo = -1.2; Hi = 1.2;     (* t \in [Lo, Hi] *)
  BelowMidFrac = 0.25;     (* fraction of "sz" origin is below window center *)
  OverMidFrac = 0.25;      (* fraction of "sz" "xTip" is right of center *)
  CurveDeltaFrac = 0.0001; (* fraction of "sz" used for delta-T for curve *)
  PathDeltaFrac = 0.5 * CurveDeltaFrac; (* fraction of "sz" for path *)
  WidthFrac = 0.006;       (* fraction of "sz" used for line width *)
  OuterBeamLen = 0.6;      (* length of outer portion of beam *)
  MinSize = 100;           (* minimum size of child in both dimensions *)
  PrefSize = 300;          (* preferred size of child in both dimensions *)
  Duration = 6.0;          (* animation duration (in seconds) *)

VAR (* CONST after init *)
  PathColor := PaintOp.FromRGB(1.0, 0.0, 0.0);

TYPE
  T = VBT.Leaf OBJECT
    <* SPEC PROTECT SELF *>
    sz: REAL;        (* size used to scale elements of drawing *)
    width: CARDINAL; (* line width for strokes (a function of "sz") *)
    origin: Point.T; (* origin point in Trestle coordinates *)
    xTip: R2.T;      (* vector for x-axis *)
  METHODS
    <* LL.sup = SELF *>
    toPoint(READONLY p: R2.T): Point.T := ToPoint;
    (* Return the Trestle point corresponding to "p". *)

    <* LL.sup = SELF *>
    fPoint(t: REAL): Point.T := FPoint;
    (* Return "toPoint(f(t))". *)

    <* LL.sup < SELF *>
    paintBackground(READONLY clip := Rect.Full) := PaintBackground;
    (* Paint the animation's background clipped to "clip"; this method does
       not "sync" the resulting graphics. *)

    <* LL.sup < SELF *>
    paintBeam(t: REAL; READONLY clip := Rect.Full) := PaintBeam;
    (* Paint the beam at time "t" clipped to "clip". *)

    <* LL.sup < SELF *>
    paintPath(t, tLast: REAL; READONLY clip := Rect.Full) := PaintPath;
    (* Paint the portion of the path traced by the beam for times in the
       closed interval "[tLast, t]". *)
  OVERRIDES
    <* LL.sup = mu.SELF *>
    reshape := Reshape;
    repaint := Repaint;
    shape   := Shape;
  END;

PROCEDURE F(t: REAL): R2.T =
  VAR t2 := t * t; BEGIN RETURN R2.T{t, t2 * t2} END F;

PROCEDURE Inner(t: REAL; y: REAL := 1.0): R2.T =
(* Return the point along the beam at time "t" with y-coordinate "y" relative
   to a coordinate system whose origin is "F(t)" and and whose x unit vector
   is along the tangent to the curve at time "t". *)
  VAR
    p := F(t);
    q := R2.Plus(p, R2.Normalize(R2.T{1.0, 4.0 * t * t * t}));
  BEGIN
    RETURN R2.Rel(R2.T{0.0, y}, p, q)
  END Inner;

PROCEDURE ToPoint(v: T; READONLY p: R2.T): Point.T =
  <* LL.sup = v *>
  BEGIN
    RETURN R2.ToPoint(R2.Rel(p, R2.Origin, v.xTip), v.origin)
  END ToPoint;

PROCEDURE FPoint(v: T; t: REAL): Point.T =
  <* LL.sup = v *>
  BEGIN
    RETURN v.toPoint(F(t))
  END FPoint;

PROCEDURE TypeR(v: T; READONLY clip: Rect.T; READONLY p: Point.T;
  fnt: Font.T; txt: TEXT) =
  <* LL.sup < v *>
  BEGIN
    VBT.PaintText(v, clip, Point.MoveH(p, -VBT.TextWidth(v, txt, fnt)),
      fnt, txt)
  END TypeR;

CONST
  Prefix = "-*-";
  Middle = "-normal-*-*-";
  Suffix = "0-*-*-*-*-*-*";

VAR
  LabelFnt := Font.FromName(ARRAY OF TEXT{
    Prefix & "times-medium-i" & Middle & "14" & Suffix});
  SubFnt := Font.FromName(ARRAY OF TEXT{
    Prefix & "times-medium-r" & Middle & "10" & Suffix});

PROCEDURE PaintLabel(v: T; READONLY clip: Rect.T) =
  <* LL.sup < v *>
  VAR p: Point.T; BEGIN
    p := Point.MoveV(v.fPoint(Lo), -15);
    TypeR(v, clip, p, LabelFnt, "y ");
    VBT.PaintText(v, clip, p, LabelFnt, "= x");
    VAR bbox := VBT.BoundingBox(v, "= x", LabelFnt); BEGIN
      VBT.PaintText(v, clip, fnt := SubFnt, t := "4",
        pt := Point.MoveV(Point.Add(p, Rect.NorthEast(bbox)), 8))
    END
  END PaintLabel;

PROCEDURE PaintBackground(v: T; READONLY clip := Rect.Full) =
  <* LL.sup < v *>
  VAR path := NEW(Path.T); width: CARDINAL; BEGIN
    VBT.PaintTint(v, clip, PaintOp.Bg);
    PaintLabel(v, clip);
    LOCK v DO
      width := v.width;
      VAR step := v.sz * CurveDeltaFrac; t := Lo; BEGIN
        Path.MoveTo(path, v.fPoint(t));
        LOOP
          t := t + step;
          IF t >= Hi THEN EXIT END;
          Path.LineTo(path, v.fPoint(t))
        END;
        Path.LineTo(path, v.fPoint(Hi))
      END
    END;
    VBT.Stroke(v, clip, path, width, end := VBT.EndStyle.Butt)
  END PaintBackground;

PROCEDURE PaintBeam(v: T; t: REAL; READONLY clip := Rect.Full) =
  <* LL.sup < v *>
  VAR path := NEW(Path.T); width: CARDINAL; BEGIN
    LOCK v DO
      width := v.width;
      Path.MoveTo(path, v.toPoint(Inner(t)));
      Path.LineTo(path, v.toPoint(Inner(t, -OuterBeamLen)))
    END;
    VBT.Stroke(v, clip, path, width, end := VBT.EndStyle.Butt)
  END PaintBeam;

PROCEDURE PaintPath(v: T; endT, startT: REAL; READONLY clip := Rect.Full) =
  <* LL.sup < v *>
  VAR path: Path.T; width: CARDINAL; BEGIN
    IF startT < endT THEN
      path := NEW(Path.T);
      LOCK v DO
    	width := v.width;
    	VAR
          step := v.sz * PathDeltaFrac;
          t := MAX(startT - step, Lo);
        BEGIN
    	  Path.MoveTo(path, v.toPoint(Inner(t)));
    	  LOOP
    	    t := t + step;
    	    IF t >= endT THEN EXIT END;
    	    Path.LineTo(path, v.toPoint(Inner(t)))
    	  END;
    	  Path.LineTo(path, v.toPoint(Inner(endT)))
    	END
      END;
      VBT.Stroke(v, clip, path, width, end := VBT.EndStyle.Butt,
    	join := VBT.JoinStyle.Bevel, op := PathColor)
    END
  END PaintPath;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) =
  <* LL.sup = VBT.mu.v *>
  BEGIN
    IF cd.new = Rect.Empty THEN RETURN END;
    LOCK v DO
      v.sz := FLOAT(MIN(Rect.HorSize(cd.new), Rect.VerSize(cd.new)));
      v.width := ROUND(v.sz * WidthFrac);
      v.origin := Point.MoveV(Rect.Middle(cd.new), ROUND(v.sz * BelowMidFrac));
      v.xTip := R2.FromPoint(
        Point.MoveH(v.origin, ROUND(v.sz * OverMidFrac)), v.origin)
    END;
    v.repaint(Region.FromRect(cd.new))
  END Reshape;

PROCEDURE Repaint(v: T; READONLY rgn: Region.T) =
  <* LL.sup = VBT.mu.v *>
  BEGIN
    WITH clip = rgn.r DO
      v.paintBackground(clip);
      v.paintBeam(Lo, clip)
    END;
    VBT.Sync(v)
  END Repaint;

PROCEDURE Shape(<*UNUSED*> v: T; <*UNUSED*> ax: Axis.T;
    n: CARDINAL): VBT.SizeRange =
  <* LL.sup = VBT.mu.v *>
  VAR pref: CARDINAL := PrefSize; BEGIN
    IF n # 0 THEN pref := n END;
    RETURN VBT.SizeRange{lo := MinSize, pref := pref, hi := 99999}
  END Shape;

PROCEDURE QuitProc(<*UNUSED*> b: ButtonVBT.T;
    <*UNUSED*> READONLY cd: VBT.MouseRec) =
(* "Quit" button callback procedure. *)
  <* LL.sup = VBT.mu *>
  BEGIN Trestle.Delete(v) END QuitProc;

PROCEDURE WarpT(t: REAL): REAL =
(* Linearly interpolate times in the closed interval "[0, Duration]" to times
   in the closed interval "[Lo, Hi]". *)
  BEGIN RETURN Lo + (Hi - Lo) * (t / Duration) END WarpT;

PROCEDURE RunProc(v: ButtonVBT.T; <*UNUSED*> READONLY cd: VBT.MouseRec) =
(* "Run" button callback procedure. *)
  <* LL.sup = VBT.mu *>
  VAR ch: T := VBT.GetProp(v, TYPECODE(T)); BEGIN
    ch.paintBackground();
    VAR start := Time.Now(); t, tLast := Lo; BEGIN
      WHILE t < Hi DO
        ch.paintPath(t, tLast);
        DblBufferVBT.Save(ch);
        ch.paintBeam(t);
        VBT.Sync(ch);
        DblBufferVBT.Restore(ch);
        tLast := t;
        t := WarpT(FLOAT(Time.Now() - start))
      END;
      ch.paintPath(Hi, tLast);
      ch.paintBeam(Hi)
    END;
    VBT.Sync(ch)
  END RunProc;

PROCEDURE NewButton(nm: TEXT; action: ButtonVBT.Proc;
  ref: REFANY := NIL): VBT.T =
(* Return a new button with label "nm", action procedure "action", and
   embedded reference "ref". The new button will have a black border. *)
  BEGIN
    RETURN BorderedVBT.New(ButtonVBT.New(TextVBT.New(nm), action, ref))
  END NewButton;

TYPE
  Bar = TextureVBT.T OBJECT
    ax: Axis.T;    (* axis of the bar *)
    sz: CARDINAL;  (* width in pixels *)
  OVERRIDES
    shape := BarShape
  END;

PROCEDURE BarShape(v: Bar; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange =
  <* LL.sup = VBT.mu.v *>
  BEGIN
    IF ax = v.ax
      THEN RETURN VBT.SizeRange{lo := v.sz, pref := v.sz, hi := v.sz + 1}
      ELSE RETURN VBT.DefaultShape
    END
  END BarShape;

VAR ch := NEW(T); v: VBT.T; BEGIN
  LOCK VBT.mu DO
    v := HVSplit.Cons(Axis.T.Ver,
      BorderedVBT.New(
        ButtonVBT.MenuBar(
          NewButton("Quit", QuitProc),
          NewButton("Run",  RunProc, ch)),
        size := 1.0,
        op := PaintOp.Bg),
      NEW(Bar, ax := Axis.T.Ver, sz := 2).init(),
      NEW(DblBufferVBT.T).init(ch))
  END;
  Trestle.Install(v, applName := "Star Animation");
  Trestle.AwaitDelete(v);
END StarAnim.
