(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 13:34:30 PST 1994 by kalsow    *)

MODULE SilArc;

IMPORT Point, Rect, Math;
IMPORT WinDef, WinGDI;
IMPORT SilWindow, SilPen, SilObject, SilRd, SilWr;

REVEAL
  T = Tx BRANDED "SilArc" OBJECT
    center : Point.T;
    s0, s1 : Point.T;
    width  : INTEGER;
    radius : INTEGER;
  OVERRIDES
    init     := Init;
    draw     := Draw;
    clone    := Clone;
    write    := Write;
    setWidth := SetWidth;
  END;
  
PROCEDURE Init (t: T;  READONLY c,r,p0: Point.T;  width: INTEGER): T =
  VAR dxt, dyt, rt: INTEGER;
  BEGIN
    t.width  := width;
    t.state  := SilObject.Selected;
    t.next   := NIL;
    t.radius := Dist (r, c);
    t.center := c;

    t.s0 := r;
    t.bbox.north := c.v - t.radius;  (*default bounding box*)
    t.bbox.south := c.v + t.radius;
    t.bbox.west  := c.h - t.radius;
    t.bbox.east  := c.h + t.radius;
    (* The actual bounding box is tricky, since it depends on the
       quadrants in which the starting and ending points of the arc lie. *)

    dxt := p0.h - c.h;
    dyt := p0.v - c.v;
    rt := Dist (p0, c);
    t.s1.h := c.h + (t.radius * dxt) DIV rt;  (*s1 is on the arc *)
    t.s1.v := c.v + (t.radius * dyt) DIV rt;

    CASE Quadrant (t.s0, c) * 4 + Quadrant (t.s1, c) OF <*NOWARN*>

    | 0 => (* s0=NE, s1=NE *)
           IF t.s0.v > t.s1.v THEN (*small arc segment*)
             t.bbox.west := t.s1.h;  t.bbox.north := t.s1.v;
             t.bbox.east := t.s0.h;  t.bbox.south := t.s0.v;
           END;

    | 1 => (* s0=NE, s1=NW *)
           t.bbox.west  := t.s1.h;
           t.bbox.east  := t.s0.h;
           t.bbox.south := MAX (t.s0.v, t.s1.v);

    | 2 => (* s0=NE, s1=SW *)
           t.bbox.south := t.s1.v;
           t.bbox.east  := t.s0.h;

    | 3 => (* s0=NE, s1=SE *)
           t.bbox.east := MAX (t.s0.h, t.s1.h);

    | 4 => (* s0=NW, s1=NE *)
           t.bbox.north := MIN (t.s0.v, t.s1.v);

    | 5 => (* s0=NW, s1=NW *)
           IF t.s0.v < t.s1.v THEN
             t.bbox.west  := t.s1.h;  t.bbox.east  := t.s0.h;
             t.bbox.north := t.s0.v;  t.bbox.south := t.s1.v;
           END;

    | 6 => (* s0=NW, s1=SW *)
           t.bbox.north := t.s0.v;
           t.bbox.south := t.s1.v;
           t.bbox.east  := MAX (t.s0.h, t.s1.h);

    | 7 => (* s0=NW, s1=SE *)
           t.bbox.north := t.s0.v;
           t.bbox.east  := t.s1.h;

    | 8 => (* s0=SW, s1=NE *)
           t.bbox.west  := t.s0.h;
           t.bbox.north := t.s1.v;

    | 9 => (* s0=SW, s1=NW *)
           t.bbox.west := MIN (t.s0.h,t.s1.h);

    | 10 => (* s0=SW, s1=SW *)
           IF t.s0.v < t.s1.v THEN
             t.bbox.west := t.s0.h;  t.bbox.north := t.s0.v;
             t.bbox.east := t.s1.h;  t.bbox.south := t.s1.v;
           END;

    | 11 => (* s0=SW, s1=SE *)
           t.bbox.north := MIN (t.s0.v, t.s1.v);
           t.bbox.west  := t.s0.h;
           t.bbox.east  := t.s1.h;

    | 12 => (* s0=SE, s1=NE *)
           t.bbox.north := t.s1.v;
           t.bbox.west  := MIN (t.s0.h, t.s1.h);
           t.bbox.south := t.s0.v;

    | 13 => (* s0=SE, s1=NW *)
           t.bbox.west  := t.s1.h;
           t.bbox.south := t.s0.v;

    | 14 => (* s0=SE, s1=SW *)
           t.bbox.south := MAX (t.s0.v, t.s1.v);

    | 15 => (* s0=SE, s1=SE *)
           IF t.s1.v < t.s0.v THEN
             t.bbox.north := t.s1.v;  t.bbox.west := t.s0.h;
             t.bbox.south := t.s0.v;  t.bbox.east := t.s1.h;
           END;

    END; (*CASE*)

    (* Make the bounding box big enough to include the pen stroke *)
    t.bbox := Rect.Inset (t.bbox, -width);

    (* Make center, s0, and s1 relative to the bounding box, so that
       macros work. *)
    t.center.h := t.center.h - t.bbox.west;
    t.center.v := t.center.v - t.bbox.north;
    t.s0.h := t.s0.h - t.bbox.west;
    t.s0.v := t.s0.v - t.bbox.north;
    t.s1.h := t.s1.h - t.bbox.west;
    t.s1.v := t.s1.v - t.bbox.north;

    RETURN t;
  END Init;
  
PROCEDURE Write (t: T;  wr: SilWr.T) =
  BEGIN
    IF t.state >= SilObject.Visible THEN
      wr.putText ("a ");
      SilObject.WriteBBox (t, wr);
      wr.putInt (t.center.h);
      wr.putInt (t.center.v);
      wr.putInt (t.s0.h);
      wr.putInt (t.s0.v);
      wr.putInt (t.s1.h);
      wr.putInt (t.s1.v);
      wr.putInt (t.width);
      wr.putInt (t.radius);
      wr.endLine ();
    END;
  END Write;
  
PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR t := NEW (T);
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 12 *>
    SilObject.ReadBBox (t, rd);
    t.center.h := rd.ints[4];
    t.center.v := rd.ints[5];
    t.s0.h     := rd.ints[6];
    t.s0.v     := rd.ints[7];
    t.s1.h     := rd.ints[8];
    t.s1.v     := rd.ints[9];
    t.width    := rd.ints[10];
    t.radius   := rd.ints[11];
    RETURN t;
  END Read;
  
PROCEDURE Clone (t: T): SilObject.T =
  VAR a := NEW (T);
  BEGIN
    a.center := t.center;
    a.s0     := t.s0;
    a.s1     := t.s1;
    a.width  := t.width;
    a.radius := t.radius;
    RETURN a;
  END Clone;
  
PROCEDURE Draw (t: T;  READONLY p: Point.T;  sel: BOOLEAN;  w: SilWindow.T) =
  VAR pen: WinDef.HGDIOBJ;  h, v: INTEGER;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      pen := SilPen.Push (t, sel, t.width, w);
      h := t.bbox.west + p.h;
      v := t.bbox.north + p.v;
      EVAL WinGDI.Arc (w.dc,
             t.center.h - t.radius + h, t.center.v - t.radius + v,
             t.center.h + t.radius + h, t.center.v + t.radius + v,
             t.s0.h + h, t.s0.v + v,
             t.s1.h + h, t.s1.v + v);
      SilPen.Pop (pen, w);
    END;
  END Draw;

PROCEDURE SetWidth (t: T;  width: INTEGER;  sel: BOOLEAN): BOOLEAN =
  VAR delta: INTEGER;
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      delta := t.width - width;
      t.bbox := Rect.Inset (t.bbox, delta);
      DEC (t.center.h, delta);  DEC (t.center.v, delta);
      DEC (t.s0.h, delta);      DEC (t.s0.v, delta);
      DEC (t.s1.h, delta);      DEC (t.s1.v, delta);
      t.width := width;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END SetWidth;

(*---------------------------------------------------- internal utilities ---*)

PROCEDURE Dist (READONLY a, b: Point.T): INTEGER =
  BEGIN
    RETURN ROUND (Math.sqrt (FLOAT (Point.DistSquare (a, b), LONGREAL)));
  END Dist;
  
PROCEDURE Quadrant (READONLY p, origin: Point.T): [0..3] =
  (* 0=NE, 1=NW, 2=SW, 3=SE *)
  TYPE  Z   = ARRAY BOOLEAN OF [0..3];
  CONST Map = ARRAY BOOLEAN OF Z { Z { 3, 0 }, Z { 2, 1 } };
  BEGIN
    RETURN Map [p.h < origin.h, p.v < origin.v];
  END Quadrant;

BEGIN
  SilWindow.RegisterReader ('a', 'a', Read);
END SilArc.
