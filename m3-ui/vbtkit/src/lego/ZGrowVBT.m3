(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jan 30 15:22:35 PST 1995 by kalsow     *)
(*      modified on Wed Jul  7 02:17:53 PDT 1993 by mhb        *)
(*      modified on Wed Aug  5 21:48:47 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:07:58 PDT 1992 by muller     *)
(*      modified on Fri Mar 27 02:59:20 1992 by steveg         *)

MODULE ZGrowVBT;

IMPORT Axis, BtnVBTClass, Cursor, FeedbackVBT, Point, Rect,
       VBT, VBTClass, ZMoveVBT, ZSplitUtils;

TYPE
  Sides = RECORD north, west, south, east: BOOLEAN; END;

REVEAL
  T = Public BRANDED OBJECT
        zChild : VBT.T;
        firstDown: Point.T;
        sides: Sides;
        rect   : Rect.T;
        stuck  : BOOLEAN;
        shape  : ARRAY Axis.T OF VBT.SizeRange;
      OVERRIDES
        init   := Init;
        mouse  := Mouse;
        pre    := Pre;
        during := During;
      END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    GetResources();
    EVAL ZMoveVBT.T.init (v, f);
    RETURN v
  END Init;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    v.firstDown := cd.cp.pt;
    ZMoveVBT.T.mouse(v, cd);
  END Mouse;

PROCEDURE Pre (v: T) =
  BEGIN
    ZMoveVBT.T.pre(v);
    IF NOT v.ready THEN RETURN END;
    v.zChild := ZSplitUtils.FindZChild(v);
    v.rect := VBT.Domain(v.zChild);
    v.shape := VBTClass.GetShapes(v.zChild, FALSE);
    FindCloseSides(v, v.firstDown, v.sides);
    ChangeSides(v, v.firstDown);
    OrientCursor(v, v.sides);
    ZMoveVBT.MoveAndHighlight(v, v.rect);
  END Pre;

PROCEDURE During (v: T; READONLY cd: VBT.PositionRec) =
  VAR pt := cd.cp.pt; sides: Sides;
  BEGIN
    IF Rect.Member(pt, VBT.Domain(VBT.Parent(v.zChild))) THEN
      (* only grow a child inside of ZSplit's domain *)
      IF FindStuckSides(v, pt) THEN
        ChangeSides(v, pt);
        sides := v.sides;
      ELSE
        FindCloseSides(v, pt, sides)
      END;
      OrientCursor(v, sides)
    END;
    ZMoveVBT.MoveAndHighlight(v, v.rect);
  END During;

PROCEDURE FindStuckSides (v: T; READONLY pt: Point.T): BOOLEAN =
  BEGIN
    WITH s = v.sides,
         r = v.rect   DO
      (* aliases; v.sides can change! *)
      IF pt.h < r.west THEN
        s.west := TRUE;
        s.east := FALSE;
      ELSIF pt.h > r.east THEN
        s.west := FALSE;
        s.east := TRUE;
      END;
      IF pt.v < r.north THEN
        s.north := TRUE;
        s.south := FALSE;
      ELSIF pt.v > r.south THEN
        s.north := FALSE;
        s.south := TRUE;
      END;
      RETURN s.east OR s.west OR s.north OR s.south
    END
  END FindStuckSides;

PROCEDURE FindCloseSides (            v : T;
                          READONLY    pt: Point.T;
                          VAR (*OUT*) s : Sides    ) =
  CONST C = 4;
  VAR
    dW    := pt.h - v.rect.west;
    dE    := v.rect.east - pt.h;
    dN    := pt.v - v.rect.north;
    dS    := v.rect.south - pt.v;
    hFuzz := ROUND(VBT.MMToPixels(v, 1.0, Axis.T.Hor));
    vFuzz := ROUND(VBT.MMToPixels(v, 1.0, Axis.T.Ver));
  BEGIN
    s.west := dW <= hFuzz + MIN(dE, MIN(dN, dS));
    s.east := dE <= hFuzz + MIN(dW, MIN(dN, dS));
    s.north := dN <= vFuzz + MIN(dW, MIN(dE, dS));
    s.south := dS <= vFuzz + MIN(dW, MIN(dE, dN));
    IF s.north THEN
      s.west := (dN < C*vFuzz AND dW < C*hFuzz);
      s.east := (dN < C*vFuzz AND dE < C*hFuzz);
    ELSIF s.south THEN
      s.west := (dS < C*vFuzz AND dW < C*hFuzz);
      s.east := (dS < C*vFuzz AND dE < C*hFuzz);
    END;
    IF s.north AND s.south THEN s.south := FALSE END;
    IF s.west AND s.east THEN s.east := FALSE END;
  END FindCloseSides;

PROCEDURE ChangeSides (v: T; READONLY pt: Point.T) =
  VAR
    min, max: INTEGER;
    dom               := VBT.Domain(v.zChild);
  BEGIN
    WITH sides = v.sides,
         rect  = v.rect   DO
      (* aliases; both v.sides and v.rect can change! *)
      min := v.shape[Axis.T.Hor].lo;
      max := v.shape[Axis.T.Hor].hi - 1;
      IF sides.west THEN
        rect.east := dom.east;
        rect.west := pt.h;
        IF Rect.HorSize(rect) > max THEN
          rect.west := rect.east - max;
        ELSIF Rect.HorSize(rect) < min THEN
          sides.west := FALSE;
          rect.west := rect.east - min;
        END
      ELSIF sides.east THEN
        rect.west := dom.west;
        rect.east := pt.h;
        IF Rect.HorSize(rect) > max THEN
          rect.east := rect.west + max;
        ELSIF Rect.HorSize(rect) < min THEN
          sides.east := FALSE;
          rect.east := rect.west + min;
        END
      END;
      min := v.shape[Axis.T.Ver].lo;
      max := v.shape[Axis.T.Ver].hi - 1;
      IF sides.north THEN
        rect.south := dom.south;
        rect.north := pt.v;
        IF Rect.VerSize(rect) > max THEN
          rect.north := rect.south - max;
        ELSIF Rect.VerSize(rect) < min THEN
          sides.north := FALSE;
          rect.north := rect.south - min;
        END
      ELSIF sides.south THEN
        rect.north := dom.north;
        rect.south := pt.v;
        IF Rect.VerSize(rect) > max THEN
          rect.south := rect.north + max;
        ELSIF Rect.VerSize(rect) < min THEN
          sides.south := FALSE;
          rect.south := rect.north + min;
        END
      END
    END
  END ChangeSides;


PROCEDURE OrientCursor (v: T; READONLY s: Sides) =
  VAR cursor: Cursor.T;
  BEGIN
    IF s.north AND s.east THEN
      cursor := MagnetNE
    ELSIF s.north AND s.west THEN
      cursor := MagnetNW
    ELSIF s.south AND s.west THEN
      cursor := MagnetSW
    ELSIF s.south AND s.east THEN
      cursor := MagnetSE
    ELSIF s.north THEN
      cursor := MagnetN
    ELSIF s.west THEN
      cursor := MagnetW
    ELSIF s.south THEN
      cursor := MagnetS
    ELSIF s.east THEN
      cursor := MagnetE
    ELSE
      cursor := MagnetNE;
    END;
    VBT.SetCursor(v, cursor)
  END OrientCursor;

VAR
  rsrcMu                 := NEW(MUTEX);
  rsrcInit               := FALSE;
  MagnetW, MagnetE, MagnetN, MagnetS:     Cursor.T;
  MagnetNW, MagnetNE, MagnetSW, MagnetSE: Cursor.T;

PROCEDURE GetResources () =
  BEGIN
    LOCK rsrcMu DO
      IF rsrcInit THEN RETURN END;
      MagnetW := Cursor.FromName(ARRAY OF TEXT{"XC_left_side"});
      MagnetE := Cursor.FromName(ARRAY OF TEXT{"XC_right_side"});
      MagnetN := Cursor.FromName(ARRAY OF TEXT{"XC_top_side"});
      MagnetS :=
        Cursor.FromName(ARRAY OF TEXT{"XC_bottom_side"});
      MagnetNW :=
        Cursor.FromName(ARRAY OF TEXT{"XC_top_left_corner"});
      MagnetNE :=
        Cursor.FromName(ARRAY OF TEXT{"XC_top_right_corner"});
      MagnetSW :=
        Cursor.FromName(ARRAY OF TEXT{"XC_bottom_left_corner"});
      MagnetSE :=
        Cursor.FromName(ARRAY OF TEXT{"XC_bottom_right_corner"});
      rsrcInit := TRUE;
    END
  END GetResources;


BEGIN
END ZGrowVBT.
