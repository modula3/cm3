(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 09:42:47 PST 1995 by kalsow   *)
(*      modified on Wed Mar 18 15:46:44 PST 1992 by msm      *)
(*      modified on Tue Mar 10 19:07:02 1992 by steveg   *)
(*      modified on Mon Feb 24 13:52:20 PST 1992 by muller   *)
(*      modified on Sun Nov 10 21:30:22 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE AnchorBtnVBT;

IMPORT VBT, Filter, ZSplit, Point, Rect, ButtonVBT, Trestle, Axis,
HighlightVBT, Split, VBTClass, TrestleComm;

FROM VBT IMPORT ClickType;

REVEAL 
  T = Public BRANDED OBJECT
    n: CARDINAL;
    anchorParent: VBT.T := NIL;
    hfudge, vfudge: REAL
  OVERRIDES 
    mouse := Mouse;
    position := Position;
    init := Be
  END;

TYPE
  AnchorRef = REF RECORD activeAnchor: T END;
  
PROCEDURE Be(
  v: T;
  ch: VBT.T; 
  menu: VBT.T;
  n: CARDINAL := 0;
  anchorParent: VBT.T := NIL;
  hfudge, vfudge := 0.0;
  ref: REFANY := NIL): T RAISES {} =
  BEGIN
    v.menu := menu;
    v.n := n;
    v.anchorParent := anchorParent;
    v.hfudge := hfudge;
    v.vfudge := vfudge;
    EVAL ButtonVBT.T.init(v, ch, NIL, ref);
    RETURN v
  END Be; 
  
PROCEDURE New(
  ch: VBT.T; 
  menu: VBT.T;
  n: CARDINAL := 0;
  anchorParent: VBT.T := NIL;
  hfudge, vfudge := 0.0;
  ref: REFANY := NIL): T RAISES {} =
  VAR res := NEW(T);
  BEGIN
    RETURN Be(res, ch, menu, n, anchorParent, hfudge, vfudge, ref)
  END New;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    Filter.T.mouse(v, cd);
    IF cd.clickType = ClickType.FirstDown THEN
       WITH ref = GetAnchorRef(v) DO
         ref.activeAnchor := v;
         Activate(v, ref)
       END
    ELSE
      WITH ref = GetAnchorRef(v) DO
        IF ref.activeAnchor # NIL THEN
          Deactivate(ref.activeAnchor);
          ref.activeAnchor := NIL
        END
      END
    END
  END Mouse;

PROCEDURE GetAnchorRef(v: T): AnchorRef =
 VAR 
   ref: AnchorRef;
   parent: VBT.T;
 BEGIN
   IF v.anchorParent = NIL THEN
     parent := VBT.Parent(v)
   ELSE
     parent := v.anchorParent
   END;
   ref := VBT.GetProp(parent, TYPECODE(AnchorRef));
   IF ref = NIL THEN
     ref := NEW(AnchorRef);
     VBT.PutProp(parent, ref)
   END;
   RETURN ref
  END GetAnchorRef;

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  BEGIN
    Filter.T.position(v, cd);
    IF cd.cp.gone THEN VBT.SetCage(v, VBT.GoneCage); RETURN END;
    VBT.SetCage(v, VBT.InsideCage);
    WITH ref = GetAnchorRef(v) DO
      IF (ref.activeAnchor # NIL)
         AND (ref.activeAnchor # v) THEN
        Deactivate(ref.activeAnchor);
        ref.activeAnchor := v;
        Activate(v, ref)
      END
    END
  END Position;

PROCEDURE GetZSplit(v: T): ZSplit.T =
  VAR m := v.n; z := v.parent;  BEGIN
    LOOP
      IF z = NIL THEN RETURN NIL END;
      IF ISTYPE(z, ZSplit.T) THEN
        IF m = 0 THEN RETURN z ELSE DEC(m) END
      END;
      z := z.parent
    END
  END GetZSplit; 
    
PROCEDURE Activate(v: T; ref: AnchorRef) =
  VAR
    pt := Point.MoveHV(Rect.SouthWest(VBT.Domain(v)), 
      ROUND(VBT.MMToPixels(v, v.hfudge, Axis.T.Hor)),
      ROUND(VBT.MMToPixels(v, v.vfudge, Axis.T.Ver)));
    z := GetZSplit(v);
    dom: Rect.T;
  BEGIN
    v.pre();
    IF v.menu.st # v.st THEN VBTClass.Rescreen(v.menu, v.st) END;
    IF z = NIL THEN
      (* insert menu as top-level window *)
      WITH srec = Trestle.ScreenOf(v, pt) DO
        IF srec.trsl # NIL THEN
          dom := Shift(MinRect(v.menu, srec.q), srec.dom);
          TRY
            Trestle.Attach(v.menu, srec.trsl);
            Trestle.Overlap(v.menu, srec.id, Rect.NorthWest(dom))
          EXCEPT
            TrestleComm.Failure => v.cancel(); ref.activeAnchor := NIL
          END
        END
      END
    ELSE
      (* insert menu in z *)
      dom := Shift(MinRect(v.menu, pt), VBT.Domain(z));
      ZSplit.Insert(z, HighlightVBT.New(v.menu), dom)
    END
  END Activate;

PROCEDURE Shift(READONLY menu, parent: Rect.T): Rect.T =
  (* Shift the menu left until it is entirely contained in parent or until its
     left edge coincides with the left edge of parent, unless it needs
     shifting to the right, in which shift until the left edge of menu is
     visible. Do the same thing vertically. *)
  VAR dh, dv: INTEGER;
  BEGIN
    dh := MAX(MIN(0, parent.east - menu.east), parent.west - menu.west);
    dv := MAX(MIN(0, parent.south - menu.south), parent.north - menu.north);
    RETURN Rect.MoveHV(menu, dh, dv);
  END Shift;

PROCEDURE MinRect(v: VBT.T; READONLY pt: Point.T): Rect.T =
  BEGIN
    RETURN 
      Rect.FromCorner(pt,
        VBTClass.GetShape(v, Axis.T.Hor, 0).lo, 
        VBTClass.GetShape(v, Axis.T.Ver, 0).lo)
  END MinRect;

PROCEDURE Deactivate(v: T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    v.cancel();
    WITH z = GetZSplit(v) DO
      IF z = NIL THEN
        Trestle.Delete(v.menu)
      ELSE
        WITH highlighter = VBT.Parent(v.menu) DO
          Split.Delete(z, highlighter);
          Split.Delete(highlighter, v.menu);
          VBT.Discard(highlighter)
        END
      END
    END
  END Deactivate;
        
PROCEDURE IsActive(v: T): BOOLEAN =
  BEGIN
    IF VBT.Parent(v) = NIL THEN RETURN FALSE END;
    WITH ref = GetAnchorRef(v) DO
      RETURN v = ref.activeAnchor
    END
  END IsActive;

PROCEDURE SetParent(v: T; p: VBT.T) =
  BEGIN 
    IF IsActive(v) THEN Crash() END;
    v.anchorParent := p 
  END SetParent;

PROCEDURE GetParent(v: T): VBT.T =
  BEGIN RETURN v.anchorParent END GetParent;
  
PROCEDURE Set(v: T; n: CARDINAL; 
  hfudge, vfudge: REAL) =
  BEGIN 
    IF IsActive(v) THEN Crash() END;
    v.n := n; v.hfudge := hfudge; v.vfudge := vfudge
  END Set;
  
PROCEDURE Get(v: T; VAR n: CARDINAL; VAR hfudge, vfudge: REAL) =
  BEGIN
    n := v.n; hfudge := v.hfudge; vfudge := v.vfudge
  END Get;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;

BEGIN END AnchorBtnVBT.
    
