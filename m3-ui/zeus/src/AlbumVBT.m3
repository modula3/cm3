(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Apr  1 09:22:31 PST 1993 by mhb      *)
(*      modified on Fri Sep 25 14:13:25 PDT 1992 by steveg   *)

MODULE AlbumVBT;

(* Like a photograph album of VBT snapshots *)

IMPORT Axis, Image, PackSplit, PaintOp, Pixmap, PixmapVBT, Split, 
  TrestleComm, VBT;

REVEAL
  T = TPublic BRANDED OBJECT
        ax        : Axis.T;
        size      : Size;
        pref      : CARDINAL;
        cntEntries: CARDINAL;
      OVERRIDES
        rescreen := RescreenT;
        shape    := ShapeT;
        init     := Init;
        add      := Add;
        clear    := Clear;
      END;

REVEAL TPrivate = PackSplit.T BRANDED OBJECT END;

TYPE
  Size = ARRAY Axis.T OF REAL;

  ImageVBT = PixmapVBT.T OBJECT
                size: Size;
              OVERRIDES
                shape   := ShapePixmap;
              END;

PROCEDURE ShapePixmap (v: ImageVBT; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR pref := ROUND(VBT.MMToPixels(v, v.size[ax], ax));
  BEGIN
    RETURN VBT.SizeRange{pref, pref, pref + 1}
  END ShapePixmap;

CONST
  Gap = 2.0;

PROCEDURE RescreenT (t: T; READONLY cd: VBT.RescreenRec) =
  VAR
    ax   := t.ax;
    gap  := ROUND(VBT.MMToPixels(t, Gap, ax));
    size := ROUND(VBT.MMToPixels(t, t.size[ax], ax));
  BEGIN
    t.pref := t.cntEntries * (gap + size) + gap;
    PackSplit.T.rescreen(t, cd);
  END RescreenT;

PROCEDURE ShapeT (t: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF ax # t.ax THEN
      RETURN PackSplit.T.shape(t, ax, n);
    ELSE
      RETURN VBT.SizeRange{t.pref, t.pref, t.pref + 1};
    END;
  END ShapeT;

<* FATAL Split.NotAChild *>
PROCEDURE Init (t            : T;
                ax           : Axis.T;
                cntEntries   : CARDINAL;
                width, height: REAL       := 30.0): T =
  BEGIN
    t.ax := ax;
    t.cntEntries := cntEntries;
    t.size := Size{width, height};
    RETURN PackSplit.T.init(t, ax, Gap, Gap, Pixmap.Gray, PaintOp.BgFg);
  END Init;

<* FATAL TrestleComm.Failure *>
PROCEDURE Add (t: T; v: VBT.T) =
  VAR
    pmVBT := NEW(ImageVBT, size := t.size).init(
               pm := Image.Unscaled(
                       Image.FromVBT(v, t.size[Axis.T.Hor],
                                     t.size[Axis.T.Ver])),
               op := PaintOp.Copy, bg := PaintOp.Bg);
  BEGIN
    Split.AddChild(t, pmVBT);
    VBT.NewShape(t);
    VBT.Mark(t);
  END Add;

PROCEDURE Clear (t: T) =
  VAR ch := Split.Succ(t, NIL);
  BEGIN
    WHILE ch # NIL DO
      Split.Delete(t, ch);
      VBT.Discard(ch);
      ch := Split.Succ(t, NIL);
    END;
    VBT.NewShape(t);
  END Clear;

BEGIN
END AlbumVBT.
