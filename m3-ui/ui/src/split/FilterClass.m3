(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Mar 16 17:50:53 PST 1995 by msm      *)
(*      modified on Mon Feb 24 13:53:18 PST 1992 by muller   *)
(*      modified on Sun Nov 10 17:57:38 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE FilterClass EXPORTS FilterClass;

IMPORT Filter, Point, Rect, VBT, VBTClass, Axis;

REVEAL Filter.T = Public BRANDED OBJECT
  OVERRIDES
    beChild := BeChild;
    succ := Succ;
    pred := Succ; (*sic*)
    nth := Nth;
    index := Index;
    locate := Locate;
    replace := Replace;
    insert := InsertAfter;
    move := NoOp;
    misc := MiscCode;
    key := KeyCode;
    read := Read;
    write := Write;
    reshape := Reshape;
    shape := Shape;
    axisOrder := AxisOrder;
    init := Be
  END;

PROCEDURE Be(v: Filter.T; ch: VBT.T): Filter.T RAISES {} =
  BEGIN
    LOCK v DO v.ch := NIL END;
    EVAL Filter.Replace(v, ch);
    RETURN v
  END Be;

PROCEDURE Locate (                       v : Filter.T;
                  <*UNUSED*> READONLY    pt: Point.T;
                             VAR (*out*) r : Rect.T    ): VBT.T RAISES {} =
  BEGIN                          (* LL = VBT.mu *)
    r := Rect.Full;
    RETURN v.ch
  END Locate;

PROCEDURE BeChild(v: Filter.T; ch: VBT.T) RAISES {} =
  BEGIN
    VBT.Split.beChild(v, ch);
    v.ch := ch
  END BeChild;

PROCEDURE Succ(v: Filter.T; ch: VBT.T): VBT.T RAISES {} =
  BEGIN
    IF ch = NIL THEN RETURN v.ch ELSE RETURN NIL END
  END Succ;

PROCEDURE Nth(v: Filter.T; n: CARDINAL): VBT.T RAISES {} =
  BEGIN
    IF n=0 THEN RETURN v.ch ELSE RETURN NIL END
  END Nth;

PROCEDURE Index(v: Filter.T; ch: VBT.T): CARDINAL RAISES {} =
  BEGIN
    IF ch = v.ch THEN RETURN 0 ELSE RETURN 1 END
  END Index;

PROCEDURE MiscCode(v: Filter.T; READONLY cd: VBT.MiscRec) RAISES {} =
  BEGIN
    IF v.ch # NIL THEN VBTClass.Misc(v.ch, cd) END
  END MiscCode;

PROCEDURE KeyCode(v: Filter.T; READONLY cd: VBT.KeyRec) RAISES {} =
  BEGIN
    IF v.ch # NIL THEN VBTClass.Key(v.ch, cd) END
  END KeyCode;

PROCEDURE Reshape(v: Filter.T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    IF v.ch # NIL THEN VBTClass.Reshape(v.ch, cd.new, cd.saved) END
  END Reshape;
  
PROCEDURE Replace(v: Filter.T; ch, new: VBT.T) RAISES {} =
  BEGIN
    IF ch = NIL THEN ch := v.ch END;
    IF new # NIL THEN 
      LOCK new DO LOCK v DO v.beChild(new) END END
    ELSE 
      LOCK v DO v.ch := NIL END
    END;
    VBTClass.LocateChanged(v);
    IF ch # NIL THEN VBTClass.Detach(ch) END
  END Replace;

PROCEDURE InsertAfter(v: Filter.T; ch, new: VBT.T) RAISES {} =
  BEGIN
    IF ch = NIL THEN ch := v.ch END;
    IF new # NIL THEN 
      LOCK new DO LOCK v DO v.beChild(new) END END
    ELSE 
      LOCK v DO v.ch := NIL END
    END;
    VBTClass.LocateChanged(v);
    IF ch # NIL THEN VBTClass.Detach(ch); VBT.Discard(ch) END
  END InsertAfter;

PROCEDURE NoOp(<*UNUSED*> v: Filter.T; <*UNUSED*> pred, ch: VBT.T) RAISES {} =
  BEGIN END NoOp;

PROCEDURE Shape(v: Filter.T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF v.ch = NIL THEN
      RETURN VBT.DefaultShape
    ELSE
      RETURN VBTClass.GetShape(v.ch, ax, n)
    END
  END Shape;

PROCEDURE AxisOrder(v: Filter.T): Axis.T =
  BEGIN 
    IF v.ch = NIL THEN 
      RETURN Axis.T.Hor
      (* Split.T.axisOrder(v) *)
    ELSE
      RETURN v.ch.axisOrder()
    END
  END AxisOrder;

PROCEDURE Read(
  v: Filter.T; 
  s: VBT.Selection; 
  tc: CARDINAL)
  : VBT.Value RAISES {VBT.Error}  =
  VAR owner: VBT.T; BEGIN 
    LOCK v DO owner := v.ch END;
    IF owner = NIL THEN 
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
    ELSE
      RETURN owner.read(s, tc)
    END
  END Read;

PROCEDURE Write(
  v: Filter.T; 
  s: VBT.Selection; 
  val: VBT.Value;
  tc: CARDINAL) 
  RAISES {VBT.Error} =
  VAR owner: VBT.T; BEGIN 
    LOCK v DO owner := v.ch END;
    IF owner = NIL THEN 
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
    ELSE
      owner.write(s, val, tc)
    END
  END Write;


BEGIN END FilterClass.
