(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Jan 31 09:52:22 PST 1995 by kalsow   *)
(*      modified on Mon Feb 14 16:10:36 PST 1994 by msm      *)
(*      modified on Wed Jul 28 13:15:28 PDT 1993 by sfreeman *)
(* modified on Thu Apr 29 11:03:49 PDT 1993 by mjordan *)
(* modified on Fri Apr 16 09:07:45 PDT 1993 by steveg *)
(* modified on Mon Feb 24 13:58:32 PST 1992 by muller *)
(* modified on Mon Dec 30 18:11:51 PST 1991 by gnelson *)

<*PRAGMA LL*>

UNSAFE MODULE VBT;

IMPORT Word, Thread, Rect, Point, Axis, Path, Trapezoid, Region, Pixmap,
       Cursor, Font, PaintOp, ScrnPixmap, BatchRep, ScrnFont, ScrnPaintOp,
       Text, VBTClass, VBTRep, TextWr, Cstring, PaintExt, PaintPrivate,
       Pickle, TextRd, PropertyV, PathPrivate, TextIntTbl, Wr, Rd,
       Palette, PlttFrnds, RTParams, MutexRep;

PROCEDURE CopyBytes (src, dst: ADDRESS; n: INTEGER) =
  BEGIN
    EVAL Cstring.memcpy(dst, src, n)
  END CopyBytes;

PROCEDURE Parent (v: T): Split RAISES {} =
  BEGIN
    LOCK v DO RETURN v.parent END
  END Parent;

PROCEDURE Domain (v: T): Rect.T RAISES {} =
  BEGIN
    LOCK v DO RETURN v.domain END
  END Domain;

PROCEDURE ScreenTypeOf (v: T): ScreenType RAISES {} =
  BEGIN
    LOCK v DO RETURN v.st END
  END ScreenTypeOf;

PROCEDURE MMToPixels (v: T; mm: REAL; ax: Axis.T): REAL RAISES {} =
  BEGIN
    LOCK v DO
      IF v.st = NIL THEN RETURN mm ELSE RETURN mm * v.st.res[ax] END
    END
  END MMToPixels;

PROCEDURE SetCage (v: T; READONLY cg: Cage) RAISES {} =
  BEGIN
    LOCK v DO VBTClass.SetCage(v, cg) END
  END SetCage;

PROCEDURE Outside (READONLY cp: CursorPosition; READONLY cg: Cage): BOOLEAN
  RAISES {} =
  BEGIN
    RETURN NOT ((cp.gone IN cg.inOut)
                  AND ((cg.screen = AllScreens) OR (cg.screen = cp.screen))
                  AND Rect.Member(cp.pt, cg.rect))
  END Outside;

PROCEDURE CageFromRect (READONLY r: Rect.T; READONLY cp: CursorPosition):
  Cage =
  BEGIN
    RETURN Cage{r, InOut{cp.gone}, cp.screen}
  END CageFromRect;

PROCEDURE CageFromPosition (READONLY cp: CursorPosition;
                            trackOutside, trackOffScreen: BOOLEAN := FALSE):
  Cage =
  BEGIN
    IF NOT cp.gone OR trackOutside AND NOT cp.offScreen OR trackOffScreen THEN
      RETURN Cage{Rect.FromPoint(cp.pt), InOut{cp.gone}, cp.screen}
    ELSIF cp.offScreen AND trackOutside THEN
      RETURN Cage{Rect.Full, InOut{FALSE, TRUE}, cp.screen}
    ELSE
      RETURN GoneCage
    END
  END CageFromPosition;

PROCEDURE SetCursor (v: T; cs: Cursor.T) RAISES {} =
  BEGIN
    LOCK v DO VBTClass.SetCursor(v, cs) END
  END SetCursor;

REVEAL
  Value = Value_Public BRANDED OBJECT
            tc : INTEGER;
            txt: TEXT
          OVERRIDES
            toRef := ToRefDefault
          END;

PROCEDURE FromRef (v: REFANY): Value RAISES {} =
  <*FATAL Wr.Failure, Pickle.Error, Thread.Alerted *>
  VAR
    res           := NEW(Value);
    wr : TextWr.T;
  BEGIN
    res.tc := TYPECODE(v);
    IF v = NIL OR res.tc = TYPECODE(TEXT) THEN
      res.txt := v
    ELSE
      wr := TextWr.New();
      Pickle.Write(wr, v);
      res.txt := TextWr.ToText(wr)
    END;
    RETURN res
  END FromRef;

PROCEDURE ToRefDefault (v: Value): REFANY RAISES {Error} =
  <*FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted *>
  BEGIN
    IF v.txt = NIL OR v.tc = TYPECODE(TEXT) THEN RETURN v.txt END;
    TRY
      RETURN Pickle.Read(TextRd.New(v.txt))
    EXCEPT
      Pickle.Error => RAISE Error(ErrorCode.WrongType)
    END;
  END ToRefDefault;

PROCEDURE Ready (<*UNUSED*> v: Value): BOOLEAN =
  BEGIN
    RETURN TRUE
  END Ready;

PROCEDURE Read (v: T; s: Selection; t: TimeStamp; tc: INTEGER := -1): Value
  RAISES {Error} =
  BEGIN
    IF s = KBFocus THEN RAISE Error(ErrorCode.Unreadable) END;
    IF tc = -1 THEN tc := TYPECODE(TEXT) END;
    WITH p = Parent(v) DO
      IF p = NIL THEN RAISE Error(ErrorCode.Uninstalled) END;
      RETURN p.readUp(v, v, s, t, tc)
    END;
  END Read;

PROCEDURE Write (v  : T;
                 s  : Selection;
                 t  : TimeStamp;
                 val: Value;
                 tc : INTEGER     := -1) RAISES {Error} =
  BEGIN
    IF s = KBFocus THEN RAISE Error(ErrorCode.Unwritable) END;
    IF tc = -1 THEN tc := TYPECODE(TEXT) END;
    WITH p = Parent(v) DO
      IF p = NIL THEN RAISE Error(ErrorCode.Uninstalled) END;
      p.writeUp(v, v, s, t, val, tc)
    END;
  END Write;

PROCEDURE Acquire (v: T; s: Selection; t: TimeStamp) RAISES {Error} =
  BEGIN
    LOCK v DO VBTClass.Acquire(v, s, t) END
  END Acquire;

PROCEDURE Release (v: T; s: Selection) RAISES {} =
  BEGIN
    LOCK v DO VBTClass.Release(v, s) END
  END Release;

PROCEDURE Put (         v     : T;
                        s     : Selection;
                        t     : TimeStamp;
                        type  : MiscCodeType;
               READONLY detail: MiscCodeDetail) RAISES {Error} =
  BEGIN
    LOCK v DO VBTClass.Put(v, s, t, type, detail) END
  END Put;

PROCEDURE Forge (v: T; type: MiscCodeType; READONLY detail: MiscCodeDetail)
  RAISES {Error} =
  BEGIN
    LOCK v DO VBTClass.Forge(v, type, detail) END
  END Forge;

PROCEDURE ForceRepaint (v: T; READONLY rgn: Region.T) RAISES {} =
  BEGIN
    LOCK v DO VBTClass.ForceRepaint(v, rgn) END
  END ForceRepaint;

CONST
  BigScrollArea = 100000;
  (* To prevent clients from queuing up lots of scrolling commands, we
     force the batch after any scrolling command larger than this. *)

  CoveredProps = VBTRep.AllProps
                   - VBTRep.Props{VBTRep.Prop.Covered, VBTRep.Prop.OnQ,
                                  VBTRep.Prop.ExcessBegins};

PROCEDURE Scroll (         v      : Leaf;
                  READONLY clp    : Rect.T;
                  READONLY dlta   : Point.T;
                           paintOp            := PaintOp.Copy) RAISES {} =
  VAR
    clip: Rect.T;
    p   : PaintPrivate.ScrollPtr;
  CONST
    bsize = ADRSIZE(PaintPrivate.ScrollRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    IF Rect.HorSize(clp) * Rect.VerSize(clp) > BigScrollArea THEN
      Sync(v)
    END;
    LOOP
      LOCK v DO
        IF NOT (VBTRep.Prop.Reshaping IN v.props) THEN
          clip := Rect.Meet(clp, Rect.Move(v.domain, dlta))
        ELSE
          clip := clp
        END;
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR po: ScrnPaintOp.T := NIL;
        BEGIN
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF po # NIL AND po # PlttFrnds.noOp THEN
            DEC(v.remaining, bsize);
            WITH b  = v.batch,
                 ss = b.scrollSource DO
              p := b.next;
              INC(b.next, bsize);
              p.command := PaintPrivate.PaintCommand.ScrollCom;
              p.clip := clip;
              p.op := po.id;
              p.delta := dlta;
              ss := Rect.Join(ss, Rect.Sub(clip, dlta))
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveOp(st, paintOp) END
      END
    END
  END Scroll;

PROCEDURE PaintTint (v: Leaf; READONLY clp: Rect.T; paintOp: PaintOp.T)
  RAISES {} =
  VAR p: PaintPrivate.TintPtr;
  CONST
    bsize = ADRSIZE(PaintPrivate.TintRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    IF Rect.IsEmpty(clp) THEN RETURN END;
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR po: ScrnPaintOp.T := NIL;
        BEGIN
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF po # NIL AND po # PlttFrnds.noOp THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.command := PaintPrivate.PaintCommand.TintCom;
              p.clip := clp;
              p.op := po.id
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveOp(st, paintOp) END
      END
    END
  END PaintTint;

PROCEDURE PolyTint (         v      : Leaf;
                    READONLY clp    : ARRAY OF Rect.T;
                             paintOp: PaintOp.T        ) RAISES {} =
  VAR
    pAdr, endP: ADDRESS;
    i         : CARDINAL;
  CONST
    bsize1 = ADRSIZE(PaintPrivate.TintRec);
    size1  = bsize1 DIV ADRSIZE(Word.T);
    bsize2 = ADRSIZE(PaintPrivate.CommandRec);
  BEGIN
    LOOP
      LOCK v DO
        IF v.st = NIL THEN RETURN END;
        VAR po: ScrnPaintOp.T := NIL;
        BEGIN
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF po # NIL AND po # PlttFrnds.noOp THEN
            i := 0;
            WHILE i # NUMBER(clp) DO
              IF Rect.IsEmpty(clp[i]) THEN
                INC(i)
              ELSE
                IF v.remaining < bsize1 THEN
                  IF v.st = NIL THEN RETURN END;
                  VBTRep.NewBatch(v, size1)
                END;
                DEC(v.remaining, bsize1);
                pAdr := v.batch.next;
                WITH p = LOOPHOLE(pAdr, PaintPrivate.TintPtr) DO
                  p.command := PaintPrivate.PaintCommand.TintCom;
                  p.clip := clp[i];
                  p.op := po.id
                END;
                INC(i);
                INC(pAdr, bsize1);
                WHILE i # NUMBER(clp) AND v.remaining >= bsize2 DO
                  WITH nbsize = MIN(
                                  NUMBER(clp) - i, v.remaining DIV bsize2)
                                  * bsize2 DO
                    DEC(v.remaining, nbsize);
                    endP := pAdr + nbsize
                  END;
                  WHILE pAdr # endP DO
                    IF Rect.IsEmpty(clp[i]) THEN
                      DEC(endP, bsize2);
                      INC(v.remaining, bsize2)
                    ELSE
                      WITH comP = LOOPHOLE(pAdr, PaintPrivate.RepeatPtr) DO
                        comP.command :=
                          PaintPrivate.PaintCommand.RepeatCom;
                        comP.clip := clp[i]
                      END;
                      INC(pAdr, bsize2)
                    END;
                    INC(i)
                  END
                END;
	        v.batch.next := pAdr
              END
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveOp(st, paintOp) END
      END
    END
  END PolyTint;

PROCEDURE PaintTexture (         v      : Leaf;
                        READONLY clp    : Rect.T;
                                 paintOp: PaintOp.T;
                                 src    : Pixmap.T;
                        READONLY dlta   : Point.T    ) RAISES {} =
  VAR p: PaintPrivate.TexturePtr;
  CONST
    bsize = ADRSIZE(PaintPrivate.PixmapRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    IF Rect.IsEmpty(clp) THEN RETURN END;
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF pm # NIL AND pm # PlttFrnds.noPixmap AND po # NIL
               AND po # PlttFrnds.noOp THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.command := PaintPrivate.PaintCommand.TextureCom;
              p.clip := clp;
              p.delta := dlta;
              p.pm := pm.id;
              p.op := po.id
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolveOp(st, paintOp);
          EVAL Palette.ResolvePixmap(st, src)
        END
      END
    END
  END PaintTexture;

PROCEDURE PolyTexture (         v      : Leaf;
                       READONLY clp    : ARRAY OF Rect.T;
                                paintOp: PaintOp.T;
                                src    : Pixmap.T;
                       READONLY dlta   : Point.T          ) RAISES {} =
  VAR
    pAdr, endP: ADDRESS;
    i         : CARDINAL;
  CONST
    bsize1 = ADRSIZE(PaintPrivate.PixmapRec);
    size1  = bsize1 DIV ADRSIZE(Word.T);
    bsize2 = ADRSIZE(PaintPrivate.CommandRec);
  BEGIN
    LOOP
      LOCK v DO
        IF v.st = NIL THEN RETURN END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF pm # NIL AND pm # PlttFrnds.noPixmap AND po # NIL
               AND po # PlttFrnds.noOp THEN
            i := 0;
            WHILE i # NUMBER(clp) DO
              IF Rect.IsEmpty(clp[i]) THEN
                INC(i)
              ELSE
                IF v.remaining < bsize1 THEN
                  IF v.st = NIL THEN RETURN END;
                  VBTRep.NewBatch(v, size1)
                END;
                DEC(v.remaining, bsize1);
                pAdr := v.batch.next;
                WITH p = LOOPHOLE(pAdr, PaintPrivate.TexturePtr) DO
                  p.command := PaintPrivate.PaintCommand.TextureCom;
                  p.clip := clp[i];
                  p.delta := dlta;
                  p.pm := pm.id;
                  p.op := po.id
                END;
                INC(i);
                INC(pAdr, bsize1);
                WHILE i # NUMBER(clp) AND v.remaining >= bsize2 DO
                  WITH nbsize = MIN(
                                  NUMBER(clp) - i, v.remaining DIV bsize2)
                                  * bsize2 DO
                    DEC(v.remaining, nbsize);
                    endP := pAdr + nbsize
                  END;
                  WHILE pAdr # endP DO
                    IF Rect.IsEmpty(clp[i]) THEN
                      DEC(endP, bsize2);
                      INC(v.remaining, bsize2)
                    ELSE
                      WITH comP = LOOPHOLE(pAdr, PaintPrivate.RepeatPtr) DO
                        comP.command :=
                          PaintPrivate.PaintCommand.RepeatCom;
                        comP.clip := clp[i]
                      END;
                      INC(pAdr, bsize2)
                    END;
                    INC(i)
                  END
                END;
                v.batch.next := pAdr
              END
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolveOp(st, paintOp);
          EVAL Palette.ResolvePixmap(st, src)
        END
      END
    END
  END PolyTexture;

PROCEDURE PaintRegion (         v    : Leaf;
                       READONLY rgn  : Region.T;
                                op   : PaintOp.T;
                                src  : Pixmap.T;
                       READONLY delta: Point.T    ) RAISES {} =
  BEGIN
    WITH list = Region.ToRects(rgn) DO
      PolyTexture(v, list^, op, src, delta)
    END
  END PaintRegion;

PROCEDURE PaintPixmap (         v      : Leaf;
                       READONLY clp    : Rect.T;
                                paintOp: PaintOp.T;
                                src    : Pixmap.T;
                       READONLY dlta   : Point.T    ) RAISES {} =
  VAR
    p  : PaintPrivate.PixmapPtr;
    clpp: Rect.T;
  CONST
    bsize = ADRSIZE(PaintPrivate.PixmapRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF pm # NIL AND pm # PlttFrnds.noPixmap AND po # NIL
               AND po # PlttFrnds.noOp THEN
            clpp := Rect.Meet(clp, Rect.Move(pm.bounds, dlta));
            IF NOT Rect.IsEmpty(clpp) THEN
              DEC(v.remaining, bsize);
              WITH b = v.batch DO
                p := b.next;
                INC(b.next, bsize);
                p.command := PaintPrivate.PaintCommand.PixmapCom;
                p.clip := clpp;
                p.pm := pm.id;
                p.delta := dlta;
                p.op := po.id
              END;
              IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END
            END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolveOp(st, paintOp);
          EVAL Palette.ResolvePixmap(st, src)
        END
      END
    END
  END PaintPixmap;

PROCEDURE PixmapDomain (v: T; pix: Pixmap.T): Rect.T =
  BEGIN
    LOOP
      LOCK v DO
        WITH st = v.st DO
          IF st = NIL THEN RETURN Rect.Empty END;
          VAR pm: ScrnPixmap.T := NIL;
          BEGIN
            IF pix.pm < NUMBER(st.pixmaps^) THEN
              pm := st.pixmaps[pix.pm]
            END;
            IF pm # NIL AND pm # PlttFrnds.noPixmap THEN
              RETURN pm.bounds
            END
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolvePixmap(st, pix) END
      END
    END
  END PixmapDomain;

PROCEDURE PaintScrnPixmap (         v   : Leaf;
                           READONLY clp : Rect.T;
                                    op  : PaintOp.T      := PaintOp.Copy;
                                    src : ScrnPixmap.T;
                           READONLY dlta: Point.T                         ) =
  VAR
    p  : PaintPrivate.PixmapPtr;
    clpp: Rect.T;
  CONST
    bsize = ADRSIZE(PaintPrivate.PixmapRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR po: ScrnPaintOp.T := NIL;
        BEGIN
          IF op.op < NUMBER(v.st.ops^) THEN po := v.st.ops[op.op] END;
          IF po # NIL AND po # PlttFrnds.noOp THEN
            clpp := Rect.Meet(clp, Rect.Move(src.bounds, dlta));
            IF NOT Rect.IsEmpty(clp) THEN
              DEC(v.remaining, bsize);
              WITH b = v.batch DO
                p := b.next;
                INC(b.next, bsize);
                p.command := PaintPrivate.PaintCommand.PixmapCom;
                p.clip := clpp;
                p.delta := dlta;
                p.pm := src.id;
                p.op := po.id
              END;
              IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END
            END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveOp(st, op) END
      END
    END
  END PaintScrnPixmap;

PROCEDURE PaintText (         v      : Leaf;
                     READONLY clp    : Rect.T;
                     READONLY rfpt   : Point.T;
                              fntP   : Font.T;
                              t      : Text.T;
                              paintOp: PaintOp.T;
                     READONLY dl := ARRAY OF Displacement{}) RAISES {} =
  VAR
    len := Text.Length(t);
    buf : ARRAY [0..127] OF CHAR;
    rbuf: REF ARRAY OF CHAR;
  BEGIN
    IF (len <= NUMBER(buf)) THEN
      Text.SetChars (buf, t);
      PaintSub(v, clp, rfpt, fntP, SUBARRAY(buf, 0, len), paintOp, dl)
    ELSE
      rbuf := NEW (REF ARRAY OF CHAR, len);
      Text.SetChars (rbuf^, t);
      PaintSub(v, clp, rfpt, fntP, rbuf^, paintOp, dl)
    END;
  END PaintText;

PROCEDURE PaintSub (         v      : Leaf;
                    READONLY clp    : Rect.T;
                    READONLY rfpt   : Point.T;
                             fntP   : Font.T;
                    READONLY t      : ARRAY OF CHAR;
                             paintOp: PaintOp.T       := PaintOp.BgFg;
                    READONLY dl := ARRAY OF Displacement{}) RAISES {} =
  VAR
    p          : PaintPrivate.TextPtr;
    size, bsize: INTEGER;
    dstAdr     : ADDRESS;
    ndl                               := NUMBER(dl);
    dlsize                            := ADRSIZE(Displacement) * ndl;
    txtsize                           := ADRSIZE(CHAR) * NUMBER(t);
    valid                             := TRUE;
  BEGIN
    bsize := ADRSIZE(PaintPrivate.TextRec) + dlsize + txtsize;
    size := (bsize + ADRSIZE(Word.T) - 1) DIV ADRSIZE(Word.T);
    bsize := ADRSIZE(Word.T) * size;
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          sf: ScrnFont.T    := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF fntP.fnt < NUMBER(v.st.fonts^) THEN
            sf := v.st.fonts[fntP.fnt]
          END;
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF sf # NIL AND sf # PlttFrnds.noFont AND po # NIL
               AND po # PlttFrnds.noOp THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch,
                 bb = Rect.Move(
                        ScrnFont.BoundingBoxSubValid(t, sf, valid), rfpt) DO
              p := b.next;
              INC(b.next, bsize);
              p.command := PaintPrivate.PaintCommand.TextCom;
              IF NOT Rect.Subset(bb, clp) THEN
                p.props := PaintPrivate.Props{PaintPrivate.Prop.Clipped}
              ELSE
                p.props := PaintPrivate.Props{}
              END;
              p.clip := Rect.Meet(bb, clp);
              p.refpt := rfpt;
              p.byteOrder := PaintPrivate.HostByteOrder;
              p.fnt := sf.id;
              p.xftFnt := sf.xftFont; (* copy the xft font*)
              p.txtsz := NUMBER(t);
              p.dlsz := NUMBER(dl);
              p.op := po.id;
              p.szOfRec := size
            END;
            dstAdr := p + ADRSIZE(p^);
            (* Copy in the displacement list: *)
            IF dlsize > 0 THEN
              CopyBytes(ADR(dl[0]), dstAdr, dlsize);
              dstAdr := dstAdr + dlsize;
            END;
            IF txtsize > 0 THEN CopyBytes(ADR(t[0]), dstAdr, txtsize) END;
            IF NOT valid THEN
              WITH m    = sf.metrics,
                   fc   = m.firstChar,
                   lc   = m.lastChar,
                   dc   = m.defaultChar,
                   dcOk = fc <= dc AND dc <= lc DO
                VAR
                  chA: UNTRACED REF ARRAY [0 .. 999999] OF CHAR := dstAdr;
                  dlA: UNTRACED REF ARRAY [0 .. 999999] OF Displacement := dstAdr
                                                                             - dlsize;
                  j, k, l, n          := 0;
                  ch        : INTEGER;
                BEGIN
                  IF dcOk THEN
                    FOR i := 0 TO txtsize - 1 DO
                      ch := ORD(chA[i]);
                      IF ch < fc OR lc < ch THEN
                        chA[i] := VAL(dc, CHAR)
                      END
                    END
                  ELSE
                    WHILE j < txtsize DO
                      k := j;
                      LOOP
                        ch := ORD(chA[k]);
                        IF ch < fc OR lc < ch THEN EXIT END;
                        INC(k);
                        IF k = txtsize THEN EXIT END
                      END;
                      IF l # 0 AND j # k THEN
                        CopyBytes(ADR(chA[j]), ADR(chA[j - l]), k - j)
                      END;
                      WHILE n < ndl AND dlA[n].index <= k DO
                        IF l # 0 THEN DEC(dlA[n].index, l) END;
                        INC(n)
                      END;
                      INC(l);
                      j := k + 1
                    END;
                    IF l # 0 THEN
                      WHILE n < ndl DO
                        IF l # 0 THEN DEC(dlA[n].index, l) END;
                        INC(n)
                      END;
                      DEC(p.txtsz, l)
                    END
                  END
                END
              END
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolveFont(st, fntP);
          EVAL Palette.ResolveOp(st, paintOp);
        END
      END
    END
  END PaintSub;

PROCEDURE BoundingBox (v: Leaf; txt: TEXT; fnt: Font.T): Rect.T =
  BEGIN
    LOOP
      LOCK v DO
        IF v.st = NIL THEN RETURN ScrnFont.BoundingBox(txt, NIL) END;
        VAR sf: ScrnFont.T := NIL;
        BEGIN
          IF fnt.fnt < NUMBER(v.st.fonts^) THEN
            sf := v.st.fonts[fnt.fnt]
          END;
          IF sf # NIL AND sf # PlttFrnds.noFont THEN
            RETURN ScrnFont.BoundingBox(txt, sf)
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveFont(st, fnt) END
      END
    END
  END BoundingBox;

PROCEDURE TextWidth (v: Leaf; txt: TEXT; fnt: Font.T): INTEGER =
  BEGIN
    LOOP
      LOCK v DO
        IF v.st = NIL THEN RETURN 0 END;
        VAR sf: ScrnFont.T := NIL;
        BEGIN
          IF fnt.fnt < NUMBER(v.st.fonts^) THEN
            sf := v.st.fonts[fnt.fnt]
          END;
          IF sf # NIL AND sf # PlttFrnds.noFont THEN
            RETURN ScrnFont.TextWidth(txt, sf)
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveFont(st, fnt) END
      END
    END
  END TextWidth;

(* PROCEDURE PaintPatch( v: Leaf; READONLY clip: Rect.T; hl, hr, vlo, vhi,
   start: INTEGER; READONLY deltaArray: ARRAY OF DeltaPair; op: PaintOp.T
   := PaintOp.BgFg; src: Pixmap.T := Pixmap.Solid; READONLY delta: Point.T
   := Point.Origin) = BEGIN Crash() END PaintPatch; *)

PROCEDURE Fill (         v    : Leaf;
                READONLY clip : Rect.T;
                         path : Path.T;
                         wind : WindingCondition;
                         op   : PaintOp.T          := PaintOp.BgFg;
                         src  : Pixmap.T           := Pixmap.Solid;
                READONLY delta: Point.T            := Point.Origin  )
  RAISES {} =
  VAR
    p          : PaintExt.FillPtr;
    size, bsize: INTEGER;
    dstAdr     : ADDRESS;
    l                             := PathPrivate.Freeze(path);
    pathsize                      := path.next - path.start;
  BEGIN
    IF pathsize = 0 THEN PathPrivate.Thaw(l); RETURN END;
    bsize := ADRSIZE(PaintExt.FillRec) + pathsize;
    size := bsize DIV ADRSIZE(Word.T);
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF op.op < NUMBER(v.st.ops^) THEN po := v.st.ops[op.op] END;
          IF po # NIL AND po # PlttFrnds.noOp AND pm # NIL
               AND pm # PlttFrnds.noPixmap THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.ext.command := PaintPrivate.PaintCommand.ExtensionCom;
              p.ext.clip := clip;
              p.ext.op := po.id;
              p.ext.szOfRec := size;
              p.ext.delta := Point.Origin;
              p.ext.pm := pm.id;
              p.ext.fnt := 0;
              p.ext.subCommand := PaintExt.FillCommand;
              p.delta := delta;
              p.wind := wind;
              p.path.curveCount := path.curveCount
            END;
            dstAdr := p + ADRSIZE(p^);
            CopyBytes(path.start, dstAdr, pathsize);
            PathPrivate.Thaw(l);
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolvePixmap(st, src);
          EVAL Palette.ResolveOp(st, op)
        END
      END
    END
  END Fill;

PROCEDURE Stroke (         v    : Leaf;
                  READONLY clip : Rect.T;
                           path : Path.T;
                           width: CARDINAL  := 1;
                           end              := EndStyle.Round;
                           join             := JoinStyle.Round;
                           op   : PaintOp.T := PaintOp.BgFg;
                           src  : Pixmap.T  := Pixmap.Solid;
                  READONLY delta: Point.T   := Point.Origin     )
  RAISES {} =
  VAR
    p          : PaintExt.StrokePtr;
    size, bsize: INTEGER;
    dstAdr     : ADDRESS;
    l                               := PathPrivate.Freeze(path);
    pathsize                        := path.next - path.start;
  BEGIN
    IF pathsize = 0 THEN PathPrivate.Thaw(l); RETURN END;
    LOOP
      bsize := ADRSIZE(PaintExt.StrokeRec) + pathsize;
      size := bsize DIV ADRSIZE(Word.T);
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF op.op < NUMBER(v.st.ops^) THEN po := v.st.ops[op.op] END;
          IF po # NIL AND po # PlttFrnds.noOp AND pm # NIL
               AND pm # PlttFrnds.noPixmap THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.ext.command := PaintPrivate.PaintCommand.ExtensionCom;
              p.ext.clip := clip;
              p.ext.op := po.id;
              p.ext.szOfRec := size;
              p.ext.delta := Point.Origin;
              p.ext.pm := pm.id;
              p.ext.fnt := 0;
              p.ext.subCommand := PaintExt.StrokeCommand;
              p.delta := delta;
              p.width := width;
              p.end := end;
              p.join := join;
              p.path.curveCount := path.curveCount
            END;
            dstAdr := p + ADRSIZE(p^);
            CopyBytes(path.start, dstAdr, pathsize);
            PathPrivate.Thaw(l);
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolvePixmap(st, src);
          EVAL Palette.ResolveOp(st, op)
        END
      END
    END
  END Stroke;

PROCEDURE Line (         v     : Leaf;
                READONLY clip  : Rect.T;
                         p0, p1: Point.T;
                         width : CARDINAL  := 1;
                         end               := EndStyle.Round;
                         op    : PaintOp.T := PaintOp.BgFg;
                         src   : Pixmap.T  := Pixmap.Solid;
                READONLY delta : Point.T   := Point.Origin    ) RAISES {} =
  CONST
    bsize = ADRSIZE(PaintExt.LineRec);
    size  = bsize DIV ADRSIZE(Word.T);
  VAR p: PaintExt.LinePtr;
  BEGIN
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF op.op < NUMBER(v.st.ops^) THEN po := v.st.ops[op.op] END;
          IF po # NIL AND po # PlttFrnds.noOp AND pm # NIL
               AND pm # PlttFrnds.noPixmap THEN
            DEC(v.remaining, bsize);
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.ext.command := PaintPrivate.PaintCommand.ExtensionCom;
              p.ext.clip := clip;
              p.ext.op := po.id;
              p.ext.szOfRec := size;
              p.ext.delta := Point.Origin;
              p.ext.pm := pm.id;
              p.ext.fnt := 0;
              p.ext.subCommand := PaintExt.LineCommand;
              p.delta := delta;
              p.width := width;
              p.end := end;
              p.p := p0;
              p.q := p1
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolvePixmap(st, src);
          EVAL Palette.ResolveOp(st, op)
        END
      END
    END
  END Line;

PROCEDURE PaintTrapezoid (         v      : Leaf;
                          READONLY clp    : Rect.T;
                          READONLY trp    : Trapezoid.T;
                                   paintOp: PaintOp.T     := PaintOp.BgFg;
                                   src    : Pixmap.T      := Pixmap.Solid;
                          READONLY dlta   : Point.T       := Point.Origin  )
  RAISES {} =
  VAR
    p     : PaintPrivate.TrapPtr;
    pmP   : PaintPrivate.Pixmap;
    lo, hi: INTEGER;
  CONST
    bsize = ADRSIZE(PaintPrivate.TrapRec);
    size  = bsize DIV ADRSIZE(Word.T);
  BEGIN
    lo := MAX(trp.vlo, clp.north);
    hi := MIN(trp.vhi, clp.south);
    IF lo >= hi THEN
      RETURN
    ELSIF (trp.m1.n = 0) OR (trp.m2.n = 0) THEN
      Crash()
    END;
    LOOP
      LOCK v DO
        IF v.remaining < bsize THEN
          IF v.st = NIL THEN RETURN END;
          VBTRep.NewBatch(v, size)
        END;
        VAR
          pm: ScrnPixmap.T  := NIL;
          po: ScrnPaintOp.T := NIL;
        BEGIN
          IF src.pm < NUMBER(v.st.pixmaps^) THEN
            pm := v.st.pixmaps[src.pm]
          END;
          IF paintOp.op < NUMBER(v.st.ops^) THEN
            po := v.st.ops[paintOp.op]
          END;
          IF po # NIL AND po # PlttFrnds.noOp AND pm # NIL
               AND pm # PlttFrnds.noPixmap THEN
            DEC(v.remaining, bsize);
            pmP := pm.id;
            WITH b = v.batch DO
              p := b.next;
              INC(b.next, bsize);
              p.command := PaintPrivate.PaintCommand.TrapCom;
              p.clip.west := clp.west;
              p.clip.east := clp.east;
              p.clip.north := lo;
              p.clip.south := hi;
              p.delta := dlta;
              p.op := po.id;
              p.p1 := trp.p1;
              p.p2 := trp.p2;
              p.m1 := trp.m1;
              p.m2 := trp.m2;
              p.pm := pmP;
            END;
            IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
            EXIT
          END
        END
      END;
      VAR st: ScreenType;
      BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN
          EVAL Palette.ResolvePixmap(st, src);
          EVAL Palette.ResolveOp(st, paintOp)
        END
      END
    END
  END PaintTrapezoid;

PROCEDURE BeginGroup (v: Leaf; sizeHint: INTEGER := 0) =
  BEGIN
    LOCK v DO
      IF v.remaining < sizeHint OR v.batch = NIL THEN
        IF v.st = NIL THEN RETURN END;
        VBTRep.NewBatch(v, sizeHint DIV BYTESIZE(Word.T))
      END;
      INC(v.batch.excessBegins);
      v.props := v.props + VBTRep.Props{VBTRep.Prop.ExcessBegins}
    END
  END BeginGroup;

PROCEDURE EndGroup (v: Leaf) =
  BEGIN
    LOCK v DO
      IF v.batch = NIL THEN RETURN END;
      WITH ba = v.batch DO
        DEC(ba.excessBegins);
        IF ba.excessBegins < 0 THEN
          VBTRep.ForceBatch(v)
        ELSIF ba.excessBegins = 0 THEN
          v.props := v.props - VBTRep.Props{VBTRep.Prop.ExcessBegins};
          IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END
        END
      END
    END
  END EndGroup;

PROCEDURE Sync (v: Leaf; wait := TRUE) =
  BEGIN
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      WITH p = v.parent DO IF p # NIL THEN p.sync(v, wait) END END
    END
  END Sync;

PROCEDURE Capture (v: T; READONLY clip: Rect.T; VAR (*out*) br: Region.T):
  ScrnPixmap.T RAISES {} =
  VAR bad: Region.T;
  BEGIN
    LOCK v DO
      bad := VBTClass.GetBadRegion(v);
      IF v.parent = NIL THEN
        br := Region.FromRect(clip);
        RETURN NIL
      ELSIF Rect.Subset(clip, v.domain) AND Region.IsEmpty(bad) THEN
        RETURN v.parent.capture(v, clip, br)
      ELSE
        WITH res = v.parent.capture(v, Rect.Meet(clip, v.domain), br) DO
          br := Region.Join(Region.Join(br, bad),
                            Region.Difference(Region.FromRect(clip),
                                              Region.FromRect(v.domain)));
          RETURN res
        END
      END
    END
  END Capture;

TYPE
  Mutex = MUTEX OBJECT
    holder: Thread.T := NIL;
  OVERRIDES
    acquire := PedanticAcquire;
    release := PedanticRelease;
  END;

PROCEDURE PedanticAcquire(m: Mutex) =
  BEGIN
    MUTEX.acquire(m);
    m.holder := Thread.Self();
  END PedanticAcquire;

PROCEDURE PedanticRelease(m: Mutex) =
  BEGIN
    m.holder := NIL;
    MUTEX.release(m);
  END PedanticRelease;

VAR pedantic := RTParams.IsPresent("CheckShape");

PROCEDURE NewShape (v: T) RAISES {} =
  BEGIN
    IF pedantic AND v.st # NIL
         AND NARROW(mu, Mutex).holder # Thread.Self() THEN
      Crash()
    END;
    LOCK v DO
      v.props := v.props + VBTRep.Props{VBTRep.Prop.HasNewShape};
      IF (v.parent # NIL) AND NOT (VBTRep.Prop.BlockNewShape IN v.props) THEN
        v.props := v.props + VBTRep.Props{VBTRep.Prop.BlockNewShape};
        v.parent.newShape(v)
      END
    END
  END NewShape;

PROCEDURE PutProp (v: T; ref: REFANY) RAISES {} =
  BEGIN
    LOCK v DO PropertyV.Put(v.propset, ref) END
  END PutProp;

PROCEDURE GetProp (v: T; tc: INTEGER): REFANY RAISES {} =
  BEGIN
    LOCK v DO RETURN PropertyV.Get(v.propset, tc) END
  END GetProp;

PROCEDURE RemProp (v: T; tc: INTEGER) RAISES {} =
  BEGIN
    LOCK v DO PropertyV.Remove(v.propset, tc) END
  END RemProp;

PROCEDURE Mark (v: T) RAISES {} =
  BEGIN
    LOCK v DO VBTRep.Mark(v) END
  END Mark;

PROCEDURE IsMarked (v: T): BOOLEAN RAISES {} =
  BEGIN
    LOCK v DO RETURN VBTRep.Prop.Marked IN v.props END
  END IsMarked;

PROCEDURE Unmark (v: T) RAISES {} =
  BEGIN
    LOCK v DO v.props := v.props - VBTRep.Props{VBTRep.Prop.Marked} END
  END Unmark;

PROCEDURE Discard (v: T) RAISES {} =
  BEGIN
    v.discard()
  END Discard;

REVEAL
  Leaf = T BRANDED OBJECT
         OVERRIDES
           reshape   := ReshapeDefault;
           repaint   := RepaintDefault;
           rescreen  := RescreenDefault;
           mouse     := MouseDefault;
           key       := KeyCodeDefault;
           position  := PositionDefault;
           misc      := MiscCodeDefault;
           shape     := ShapeDefault;
           read      := ReadDefault;
           write     := WriteDefault;
           redisplay := RedisplayDefault;
           discard   := DiscardDefault;
         END;

PROCEDURE MouseDefault (<*UNUSED*> v: T; <*UNUSED*> READONLY cd: MouseRec)
  RAISES {} =
  BEGIN
  END MouseDefault;

PROCEDURE PositionDefault (<*UNUSED*>          v : T;
                           <*UNUSED*> READONLY cd: PositionRec) RAISES {} =
  BEGIN
  END PositionDefault;

PROCEDURE ReadDefault (<*UNUSED*> v : T;
                       <*UNUSED*> s : Selection;
                       <*UNUSED*> tc: CARDINAL   ): Value RAISES {Error} =
  BEGIN
    RAISE Error(ErrorCode.Unreadable)
  END ReadDefault;

PROCEDURE WriteDefault (<*UNUSED*> v  : T;
                        <*UNUSED*> s  : Selection;
                        <*UNUSED*> val: Value;
                        <*UNUSED*> tc : CARDINAL   ) RAISES {Error} =
  BEGIN
    RAISE Error(ErrorCode.Unwritable)
  END WriteDefault;

PROCEDURE KeyCodeDefault (<*UNUSED*> v: T; <*UNUSED*> READONLY cd: KeyRec)
  RAISES {} =
  BEGIN
  END KeyCodeDefault;

PROCEDURE MiscCodeDefault (<*UNUSED*> v: T; <*UNUSED*> READONLY cd: MiscRec)
  RAISES {} =
  BEGIN
  END MiscCodeDefault;

PROCEDURE ReshapeDefault (v: T; <*UNUSED*> READONLY cd: ReshapeRec)
  RAISES {} =
  BEGIN
    VBTClass.Repaint(v, Region.FromRect(v.domain))
  END ReshapeDefault;

PROCEDURE RepaintDefault (<*UNUSED*>          v  : T;
                          <*UNUSED*> READONLY rgn: Region.T) RAISES {} =
  BEGIN
  END RepaintDefault;

PROCEDURE RescreenDefault (v: T; READONLY cdP: RescreenRec) RAISES {} =
  VAR cd: ReshapeRec;
  BEGIN                          (* LL = v's share of VBT.mu *)
    NewShape(v);
    cd.new := Rect.Empty;
    cd.saved := Rect.Empty;
    cd.prev := cdP.prev;
    cd.marked := cdP.marked;
    v.reshape(cd)
  END RescreenDefault;

PROCEDURE RedisplayDefault (v: T) RAISES {} =
  VAR cd: ReshapeRec;
  BEGIN
    cd.new := v.domain;
    cd.prev := v.domain;
    cd.saved := Rect.Empty;
    cd.marked := TRUE;
    v.reshape(cd)
  END RedisplayDefault;

PROCEDURE DiscardDefault (<*UNUSED*> v: T) RAISES {} =
  BEGIN
  END DiscardDefault;

PROCEDURE ShapeDefault (<*UNUSED*> v : T;
                        <*UNUSED*> ax: Axis.T;
                        <*UNUSED*> n : CARDINAL): SizeRange RAISES {} =
  BEGIN
    RETURN DefaultShape
  END ShapeDefault;

PROCEDURE GetSelection (name: TEXT): Selection =
  BEGIN
    RETURN Selection{GetAtom(name, sel)}
  END GetSelection;

PROCEDURE GetMiscCodeType (name: TEXT): MiscCodeType =
  BEGIN
    RETURN MiscCodeType{GetAtom(name, mct)}
  END GetMiscCodeType;

PROCEDURE SelectionName (s: Selection): TEXT =
  BEGIN
    RETURN AtomName(s.sel, sel)
  END SelectionName;

PROCEDURE MiscCodeTypeName (type: MiscCodeType): TEXT =
  BEGIN
    RETURN AtomName(type.typ, mct)
  END MiscCodeTypeName;

TYPE
  TextSeq = REF ARRAY OF TEXT;
  AtomTable = RECORD
                cnt: CARDINAL;
                tbl: TextIntTbl.T;
                nm : TextSeq
              END;

PROCEDURE GetAtom (nm: TEXT; VAR tbl: AtomTable): CARDINAL =
  VAR res: INTEGER;
  BEGIN
    LOCK atomMu DO
      IF tbl.tbl.get(nm, res) THEN RETURN res END;
      res := tbl.cnt;
      INC(tbl.cnt);
      IF tbl.cnt > NUMBER(tbl.nm^) THEN Extend(tbl.nm) END;
      tbl.nm[res] := nm;
      EVAL tbl.tbl.put(nm, res);
      RETURN res
    END
  END GetAtom;

PROCEDURE AtomName (atm: CARDINAL; READONLY tbl: AtomTable): TEXT =
  BEGIN
    LOCK atomMu DO
      IF atm >= tbl.cnt THEN RETURN NIL ELSE RETURN tbl.nm[atm] END
    END
  END AtomName;

PROCEDURE Extend (VAR seq: TextSeq) =
  VAR
    new: TextSeq;
    n            := NUMBER(seq^);
  BEGIN
    new := NEW(TextSeq, MAX(6, 2 * n));
    SUBARRAY(new^, 0, n) := seq^;
    seq := new
  END Extend;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

VAR
  atomMu := NEW(MUTEX);
  sel, mct := AtomTable{0, NEW(TextIntTbl.Default).init(), NEW(TextSeq, 0)};

BEGIN
  <* ASSERT BITSIZE (ModifiersAsInt) = BITSIZE (Modifiers) *>
  IF pedantic
    THEN mu := NEW(Mutex);
    ELSE mu := NEW(MUTEX);
  END;
  NilSel := GetSelection("NilSel");
  Forgery := GetSelection("Forgery");
  KBFocus := GetSelection("KBFocus");
  Target := GetSelection("Target");
  Source := GetSelection("Source");
  Deleted := GetMiscCodeType("Deleted");
  Disconnected := GetMiscCodeType("Disconnected");
  TakeSelection := GetMiscCodeType("TakeSelection");
  Lost := GetMiscCodeType("Lost");
  TrestleInternal := GetMiscCodeType("TrestleInternal");
  Moved := GetMiscCodeType("Moved");
END VBT.
