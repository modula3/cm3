(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Sep 28 20:42:17 PDT 1995 by mhb        *)
(*      modified on Mon Jan 30 15:22:40 PST 1995 by kalsow     *)
(*      modified on Tue Aug  4 17:24:32 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:08:38 PDT 1992 by muller     *)
(*      modified on Fri Mar 27 02:31:45 1992 by steveg         *)
(*      modified on Mon Dec  9 16:56:38 PST 1991 by meehan     *)

MODULE MarginFeedbackVBT;

IMPORT Axis, FeedbackVBT, Filter, HVSplit, MultiClass,
         Pixmap, PixmapVBT, Point, Rect, Region, Shadow,
         ShadowPaint, Split, VBT, VBTKitResources;

REVEAL
  T = Public BRANDED OBJECT
        marginVBT: VBT.T
      OVERRIDES
        init := Init
      END;

TYPE
  MC = MultiClass.T OBJECT
       OVERRIDES
         replace := Replace;
         succ    := Succ;
         pred    := Succ;
       END;

PROCEDURE Init (v: T; ch, marginVBT: VBT.T): T =
  VAR hv := HVSplit.New(Axis.T.Hor, FALSE, -1, FALSE);
  BEGIN
    EVAL FeedbackVBT.T.init(v, hv);
    MultiClass.Be(v, NEW(MC));
    Split.AddChild(hv, marginVBT);
    IF ch # NIL THEN
      Split.AddChild(hv, ch);
      MultiClass.BeChild(v, ch);
    END;
    v.marginVBT := marginVBT;
    FeedbackVBT.Normal(v);
    RETURN v
  END Init;

TYPE
  Flavor = {Check, Box, Bullet};
  OnOff = BOOLEAN;
  NE = {Normal, Excited};
  
VAR 
  mu := NEW(MUTEX);
  inited := ARRAY Flavor OF BOOLEAN {FALSE, FALSE, FALSE};
  
  pixmaps  : ARRAY Flavor, OnOff, NE OF Pixmap.T;
  styles   : ARRAY Flavor, OnOff, NE OF Shadow.Style;
  textures : ARRAY Flavor, OnOff, NE OF Pixmap.T;
 
TYPE
  TWithPixmaps = T OBJECT
                   flavor: Flavor;
                 OVERRIDES
                   normal  := Normal;
                   excited := Excited;
                 END;

PROCEDURE NewWithPixmaps (ch     : VBT.T;
                          blotVBT: BlotVBT;
                          flavor : Flavor   ): T =
  BEGIN
    RETURN NEW(TWithPixmaps, flavor := flavor).init(ch, blotVBT)
  END NewWithPixmaps;

PROCEDURE Show (v: TWithPixmaps; normalExcited: NE) =
  VAR onOff := FeedbackVBT.GetState(v);
  BEGIN
    LOCK mu DO
      BlotVBTPut(
        v.marginVBT, pixmaps[v.flavor, onOff, normalExcited],
        styles[v.flavor, onOff, normalExcited],
        textures[v.flavor, onOff, normalExcited])
    END
  END Show;

PROCEDURE Normal (v: T) =
  BEGIN
   Show(v, NE.Normal);
  END Normal;

PROCEDURE Excited (v: T) =
  BEGIN
   Show(v, NE.Excited);
  END Excited;


(************************** Check Mark  ****************************)

PROCEDURE NewCheck (ch: VBT.T; shadow: Shadow.T := NIL): T =
  BEGIN
    GetCheckResources();
    RETURN NewWithPixmaps(ch, NewBlotVBT(shadow), Flavor.Check);
  END NewCheck;

PROCEDURE GetCheckResources () =
  BEGIN
    LOCK mu DO
      IF inited [Flavor.Check] THEN RETURN END;
      pixmaps [Flavor.Check, FALSE, NE.Normal] :=
        VBTKitResources.GetPixmap ("checkMarkOff");
      pixmaps [Flavor.Check, TRUE, NE.Normal] :=
        VBTKitResources.GetPixmap ("checkMarkOn");
      pixmaps [Flavor.Check, FALSE, NE.Excited] :=
        VBTKitResources.GetPixmap ("checkMarkOffExcited");
      pixmaps [Flavor.Check, TRUE, NE.Excited] :=
        VBTKitResources.GetPixmap ("checkMarkOnExcited");
      inited [Flavor.Check] := TRUE;
    END
  END GetCheckResources;


(**************************  Box  ****************************)

PROCEDURE NewBox (ch: VBT.T; shadow: Shadow.T := NIL): T =
  BEGIN
    GetBoxResources();
    RETURN
      NewWithPixmaps(
        ch, NewBlotVBT(shadow, Looks.Square, 0.5), Flavor.Box);
  END NewBox;

PROCEDURE GetBoxResources () =
  BEGIN
    LOCK mu DO
      IF inited[Flavor.Box] THEN RETURN END;
      pixmaps[Flavor.Box, FALSE, NE.Normal] :=
        VBTKitResources.GetPixmap("checkOff");
      styles[Flavor.Box, FALSE, NE.Normal] :=
        Shadow.Style.Raised;
      textures[Flavor.Box, FALSE, NE.Normal] := Pixmap.Empty;
      pixmaps[Flavor.Box, TRUE, NE.Normal] :=
        VBTKitResources.GetPixmap("checkOn");
      styles[Flavor.Box, TRUE, NE.Normal] :=
        Shadow.Style.Lowered;
      textures[Flavor.Box, TRUE, NE.Normal] := Pixmap.Solid;
      pixmaps[Flavor.Box, FALSE, NE.Excited] :=
        VBTKitResources.GetPixmap("checkOffExcited");
      styles[Flavor.Box, FALSE, NE.Excited] :=
        Shadow.Style.Raised;
      textures[Flavor.Box, FALSE, NE.Excited] := Pixmap.Gray;
      pixmaps[Flavor.Box, TRUE, NE.Excited] :=
        VBTKitResources.GetPixmap("checkOnExcited");
      styles[Flavor.Box, TRUE, NE.Excited] :=
        Shadow.Style.Lowered;
      textures[Flavor.Box, TRUE, NE.Excited] := Pixmap.Gray;
      inited[Flavor.Box] := TRUE;
    END
  END GetBoxResources;


(**************************  Radio  ****************************)

PROCEDURE NewBullet (ch: VBT.T; shadow: Shadow.T := NIL): T =
  BEGIN
    GetBulletResources ();
    RETURN NewWithPixmaps (
             ch, NewBlotVBT (shadow, Looks.Diamond, 0.25),
             Flavor.Bullet);
  END NewBullet;

PROCEDURE GetBulletResources () =
  BEGIN
    LOCK mu DO
      IF inited[Flavor.Bullet] THEN RETURN END;
      pixmaps[Flavor.Bullet, FALSE, NE.Normal] :=
        VBTKitResources.GetPixmap("radioOff");
      styles[Flavor.Bullet, FALSE, NE.Normal] :=
        Shadow.Style.Raised;
      textures[Flavor.Bullet, FALSE, NE.Normal] := Pixmap.Empty;
      pixmaps[Flavor.Bullet, TRUE, NE.Normal] :=
        VBTKitResources.GetPixmap("radioOn");
      styles[Flavor.Bullet, TRUE, NE.Normal] :=
        Shadow.Style.Lowered;
      textures[Flavor.Bullet, TRUE, NE.Normal] := Pixmap.Solid;
      pixmaps[Flavor.Bullet, FALSE, NE.Excited] :=
        VBTKitResources.GetPixmap("radioOffExcited");
      styles[Flavor.Bullet, FALSE, NE.Excited] :=
        Shadow.Style.Raised;
      textures[Flavor.Bullet, FALSE, NE.Excited] := Pixmap.Gray;
      pixmaps[Flavor.Bullet, TRUE, NE.Excited] :=
        VBTKitResources.GetPixmap("radioOnExcited");
      styles[Flavor.Bullet, TRUE, NE.Excited] :=
        Shadow.Style.Lowered;
      textures[Flavor.Bullet, TRUE, NE.Excited] := Pixmap.Gray;
      inited[Flavor.Bullet] := TRUE;
    END
  END GetBulletResources;


(************************** BlotVBT:  ****************************)

(* When the 2-1/2d look is supported, a BlotVBT displays as a
   2-1/2d square or diamond, whose interior color and style
   (e.g., lowered or raised) can be set dynamically.  Otherwise,
   when the 2-1/2d look is not supported, a pixmap is displayed.
   The shape of VBT is the shape of the pixmap (even when 2-1/2d
   is supported), with all stretch removed. *)

TYPE
  Looks = {Same, Square, Diamond};

TYPE
  BlotVBT = PixmapVBT.T BRANDED OBJECT
        shadow  : Shadow.T;
        looks   : Looks;
        inset   : REAL;
        pm      : Pixmap.T;
        style   : Shadow.Style;
        interior: Pixmap.T
      OVERRIDES
        shape   := BlotVBTShape;
        repaint := BlotVBTRepaint;
      END;

PROCEDURE NewBlotVBT (shadow: Shadow.T := NIL;
                      looks : Looks    := Looks.Same;
                      inset : REAL     := 0.0         ): BlotVBT =
  VAR v := NEW(BlotVBT);
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    v.shadow := shadow;
    v.looks := looks;
    v.inset := inset;
    v.pm := Pixmap.Empty;
    v.style := Shadow.Style.Flat;
    v.interior := Pixmap.Empty;
    EVAL PixmapVBT.T.init(v, v.pm, op:=v.shadow.bgFg, bg:=v.shadow.bg);
    RETURN v;
  END NewBlotVBT;

PROCEDURE BlotVBTPut (v       : BlotVBT;
                      pm      : Pixmap.T;
                      style   : Shadow.Style;
                      interior: Pixmap.T      ) =
  BEGIN
    v.pm := pm;
    v.style := style;
    v.interior := interior;
    PixmapVBT.Put(v, v.pm);
    VBT.Mark(v);
  END BlotVBTPut;

PROCEDURE BlotVBTShape (v: BlotVBT; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR sr := PixmapVBT.T.shape(v, ax, n);
  BEGIN
    sr.hi := sr.lo + 1;
    RETURN sr;
  END BlotVBTShape;

PROCEDURE BlotVBTRepaint (v: BlotVBT; READONLY rgn: Region.T) =
  BEGIN
    IF (v.looks = Looks.Same) OR NOT Shadow.Supported(v.shadow, v) THEN
      PixmapVBT.T.repaint(v, rgn)
    ELSE
      BlotVBTRepaint2 (v, rgn)
    END
  END BlotVBTRepaint;
  
PROCEDURE BlotVBTRepaint2 (v: BlotVBT; READONLY clip: Region.T) =
  VAR dom, inner, outer: Rect.T; a: Rect.Partition;
  BEGIN
    dom := VBT.Domain(v);
    WITH
      dh = ROUND(VBT.MMToPixels(v, v.inset, Axis.T.Hor)),
      dv = ROUND(VBT.MMToPixels(v, v.inset, Axis.T.Ver)),
      bounds = Rect.Change(VBT.PixmapDomain(v, v.pm), dh, -dh, dv, -dv),
      delta = Point.Sub(Rect.Middle(dom), Rect.Middle(bounds)) 
    DO
      (* Now midpoint(v.pm) + delta = midpoint(dom) *)
      outer := Rect.Move(bounds, delta)
    END;
    Rect.Factor(Rect.Meet(dom, clip.r), outer, a, 0, 0);
    FOR i := 0 TO 4 DO
      IF i # 2 THEN
        VBT.PaintTexture(v, a[i], v.shadow.bg, Pixmap.Solid, Point.Origin);
      ELSE (* i = 2 *)
        WITH
          dh = ROUND(VBT.MMToPixels(v, ABS(v.shadow.size), Axis.T.Hor)),
          dv = ROUND(VBT.MMToPixels(v, ABS(v.shadow.size), Axis.T.Ver)) 
        DO
          inner := Rect.Change(outer, dh, -dh, dv, -dv)
        END;
        CASE v.looks OF
        | Looks.Same => <* ASSERT FALSE *>
        | Looks.Square =>
            ShadowPaint.Border(v, clip, v.shadow, v.style, inner, outer);
            VBT.PaintTexture(v, Rect.Meet(clip.r, inner), v.shadow.bgFg,
                             v.interior, Point.Origin);
        | Looks.Diamond =>
            ShadowPaint.Diamond(v, clip, v.shadow, v.style, inner, outer,
                                v.shadow.bgFg, v.interior);
        END
      END
    END
  END BlotVBTRepaint2;


(************************** Multi methods:  ****************************)

PROCEDURE Replace (m: MC; ch, new: VBT.T) =
  <* FATAL Split.NotAChild *>
  VAR hv := Filter.Child(m.vbt);
  BEGIN
    IF ch = NIL THEN
      Split.AddChild(hv, new)
    ELSE
      Split.Replace(hv, ch, new)
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  VAR hv := Filter.Child(m.vbt);
  BEGIN
    IF ch = NIL THEN RETURN Split.Nth(hv, 1) ELSE RETURN NIL END
  END Succ;

BEGIN
END MarginFeedbackVBT.


