(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul  8 17:10:22 PDT 1994 by msm     *)

<* PRAGMA LL *>

UNSAFE MODULE JoinScreen;

IMPORT VBT, Palette, ScrnPixmap, ScrnFont, ScrnPaintOp, VBTClass,
       ScreenType, PaintOp, Font, Cursor, Pixmap, VBTRep, PlttFrnds,
       JoinFont, JoinCursor, JoinPaintOp, JoinPixmap, JoinCMap, Batch,
       BatchUtil, PaintPrivate, Axis, MouseSplit;

TYPE PC = PaintPrivate.PaintCommand;

REVEAL
  T = Public BRANDED OBJECT
        (* sts contains the list of screens that this joined screen is
           shared over.  this might be helpful in making some of the
           oracles make intelligent decisions based on the screentypes
           involved in the join. *)
        sts: ScreenArray := NIL;
        mu: MUTEX;
      OVERRIDES
        opApply      := JoinPaintOp.Apply;
        pixmapApply  := JoinPixmap.Apply;
        cursorApply  := JoinCursor.Apply;
        fontApply    := JoinFont.Apply;
        eval         := EvalResources;
        init         := Init;
        addScreen    := AddScreen;
        removeScreen := RemoveScreen;
        succ         := Succ;
      END;                      (* object *)

TYPE 
  ScreenArray = REF ARRAY OF RECORD st: ScreenType.T; cnt: INTEGER END;

PROCEDURE Succ (self: T; st: VBT.ScreenType; VAR hint: INTEGER):
  VBT.ScreenType =
  BEGIN
    LOCK self.mu DO
      IF self.sts = NIL THEN RETURN NIL END;
      IF st = NIL THEN hint := 0; RETURN self.sts[0].st END;
      IF hint < 0 OR hint > LAST(self.sts^) OR self.sts[hint].st # st THEN
        hint := 0;
        WHILE hint < NUMBER(self.sts^) AND self.sts[hint].st # st DO
          INC(hint)
        END
      END;
      INC(hint);
      IF hint < NUMBER(self.sts^) THEN
        RETURN self.sts[hint].st
      ELSE
        RETURN NIL
      END
    END
  END Succ;

PROCEDURE RemoveScreen (self: T; st: VBT.ScreenType): BOOLEAN =
  VAR
    deleted          := FALSE;
    n      : INTEGER;
    i                := 0;
  BEGIN
    IF self.sts = NIL OR st = NIL THEN RETURN FALSE END;
    LOCK self.mu DO
      LOOP
        IF i = NUMBER(self.sts^) THEN EXIT END;
        IF self.sts[i].st = st THEN
          DEC(self.sts[i].cnt);
          IF self.sts[i].cnt = 0 THEN
            n := NUMBER(self.sts^) - i - 1;
            SUBARRAY(self.sts^, i, n) := SUBARRAY(self.sts^, i + 1, n);
            self.sts[LAST(self.sts^)].st := NIL;
            self.sts[LAST(self.sts^)].cnt := 0;
            deleted := TRUE;
            IF i = 0 THEN SetParamsFromScreenType(self, self.sts[0].st) END
          END;
          EXIT
        END;
        INC(i)
      END
    END;
    RETURN deleted
  END RemoveScreen;

PROCEDURE SetParamsFromScreenType (self: T; st: VBT.ScreenType) =
  BEGIN
    IF st = NIL THEN RETURN END;
    self.depth := st.depth;
    self.color := st.color;
    self.res := st.res;
    self.bg := st.bg;
    self.fg := st.fg;
    IF self.bits # self THEN
      SetParamsFromScreenType(self.bits, st.bits)
    END
  END SetParamsFromScreenType;

PROCEDURE AddScreen (self: T; st: VBT.ScreenType): BOOLEAN =
  VAR postNil, i, n: INTEGER;
  BEGIN
    IF st = NIL THEN RETURN FALSE END;
    LOCK self.mu DO
      IF self.sts = NIL THEN
        self.sts := NEW(ScreenArray, 2);
        FOR i := 0 TO LAST(self.sts^) DO self.sts[i].st := NIL END;
        self.sts[0].st := st;
        self.sts[0].cnt := 1;
        SetParamsFromScreenType(self, st)
      ELSE
        postNil := NUMBER(self.sts^);
        i := 0;
        WHILE postNil > 0 AND self.sts[postNil - 1].st = NIL DO
          DEC(postNil)
        END;
        LOOP
          IF i = postNil THEN EXIT END;
          WITH sti = self.sts[i].st DO
            IF sti = st THEN INC(self.sts[i].cnt); RETURN FALSE END;
            IF sti.depth < st.depth THEN EXIT END;
            IF sti.depth = st.depth THEN
              IF st.color AND NOT sti.color THEN EXIT END;
              IF st.color = sti.color THEN
                IF sti.res[Axis.T.Hor] < st.res[Axis.T.Hor] THEN EXIT END;
                IF sti.res[Axis.T.Hor] = st.res[Axis.T.Hor] THEN
                  IF sti.res[Axis.T.Ver] < st.res[Axis.T.Ver] THEN EXIT END
                END
              END
            END
          END;
          INC(i)
        END;
        IF postNil = NUMBER(self.sts^) THEN
          VAR new := NEW(ScreenArray, postNil * 2);
          BEGIN
            FOR j := postNil + 1 TO LAST(new^) DO
              new[j].st := NIL;
              new[j].cnt := 0
            END;
            SUBARRAY(new^, 0, postNil) := self.sts^;
            self.sts := new
          END
        END;
        n := postNil - i;
        SUBARRAY(self.sts^, i + 1, n) := SUBARRAY(self.sts^, i, n);
        self.sts[i].st := st;
        self.sts[i].cnt := 1;
        IF i = 0 THEN SetParamsFromScreenType(self, st) END
      END
    END;
    RETURN TRUE
  END AddScreen;

PROCEDURE EvalResources(st: T) =
  BEGIN
    FOR j := 0 TO LAST(st.ops^) DO
      IF st.ops[j] # NIL AND st.ops[j] # PlttFrnds.noOp THEN
        EVAL st.opApply(NIL, PaintOp.T{j})
      END
    END;
    FOR j := 0 TO LAST(st.fonts^) DO
      IF st.fonts[j] # NIL AND st.fonts[j] # PlttFrnds.noFont THEN
        EVAL st.fontApply(NIL, Font.T{j})
      END
    END;
    FOR j := 0 TO LAST(st.pixmaps^) DO
      IF st.pixmaps[j] # NIL AND st.pixmaps[j] # PlttFrnds.noPixmap THEN
        EVAL st.pixmapApply(NIL, Pixmap.T{j})
      END
    END;
    FOR j := 0 TO LAST(st.cursors^) DO
      IF st.cursors[j] # NIL AND st.cursors[j] # PlttFrnds.noCursor THEN
        EVAL st.cursorApply(NIL, Cursor.T{j})
      END
    END
  END EvalResources;

PROCEDURE New(): T =
  BEGIN
    RETURN NEW(T, bits := NIL).init();
  END New;

PROCEDURE Init (st: T): T =
  BEGIN
    st.mu := NEW(MUTEX);
    st.op := JoinPaintOp.New(st);
    st.cursor := JoinCursor.New(st);
    st.pixmap := JoinPixmap.New(st);
    st.font := JoinFont.New(st);
    st.cmap := JoinCMap.New(st);
    st.depth := 1;
    st.color := FALSE;
    st.res := ARRAY Axis.T OF REAL{2.8, ..};
    st.bg := 0;
    st.fg := 1;
    IF st.bits = NIL THEN
      VAR bits := NEW(T);
      BEGIN
        bits.bits := bits;
        EVAL bits.init();
        st.bits := bits
      END
    END;
    Palette.Init(st);
    RETURN st
  END Init;

PROCEDURE MungeBatch (ba: Batch.T; st, cst: ScreenType.T) =
  VAR
    cptr: PaintPrivate.CommandPtr             := BatchUtil.Succ(ba, NIL);
    ptr := LOOPHOLE(ADR(cptr), UNTRACED REF PaintPrivate.PaintPtr);
    pxm := LOOPHOLE(ADR(cptr), UNTRACED REF PaintPrivate.PixmapPtr);
    txt := LOOPHOLE(ADR(cptr), UNTRACED REF PaintPrivate.TextPtr);
    trp := LOOPHOLE(ADR(cptr), UNTRACED REF PaintPrivate.TrapPtr);
    ext := LOOPHOLE(ADR(cptr), UNTRACED REF PaintPrivate.ExtensionPtr);
    cmd: PaintPrivate.PaintCommand;
    no                                          := 2 * NUMBER(st.ops^);
    np                                          := 2 * NUMBER(st.pixmaps^);
    nf                                          := 2 * NUMBER(st.fonts^);
    ncf                                         := 2 * NUMBER(cst.fonts^);
    x  : INTEGER;
    op : ScrnPaintOp.T;
    pm : ScrnPixmap.T;
    fn, cfn: ScrnFont.T;
  BEGIN
    WHILE cptr # NIL DO
      cmd := cptr.command;
      IF cmd # PC.RepeatCom THEN
        x := ptr^.op;
        IF x > 0 AND x MOD 2 = 1 AND x < no THEN
          op := st.ops[x DIV 2];
          IF op # NIL THEN ptr^.op := op.id END
        END;
        pm := NIL;
        CASE cmd OF
          PC.TextureCom, PC.PixmapCom =>
            x := pxm^.pm;
            IF x > 0 AND x MOD 2 = 1 AND x < np THEN
              pm := st.pixmaps[x DIV 2]
            ELSIF x MOD 2 = 0 THEN
              pm := JoinPixmap.Resolve(st, cst, x)
            END;
            IF pm # NIL THEN pxm^.pm := pm.id END
        | PC.TrapCom =>
            x := trp^.pm;
            IF x > 0 AND x MOD 2 = 1 AND x < np THEN
              pm := st.pixmaps[x DIV 2]
            ELSIF x MOD 2 = 0 THEN
              pm := JoinPixmap.Resolve(st, cst, x)
            END;
            IF pm # NIL THEN trp^.pm := pm.id END
        | PC.TextCom =>
            x := txt^.fnt;
            IF x > 0 AND x MOD 2 = 1 AND x < nf THEN
              fn := st.fonts[x DIV 2];
              IF fn # NIL THEN
                txt^.fnt := fn.id;
                IF x < ncf THEN
                  cfn := cst.fonts[x DIV 2];
                  IF cfn # NIL AND cfn.metrics.fprint # fn.metrics.fprint THEN
                    txt^.props := PaintPrivate.Props{PaintPrivate.Prop.Clipped,
                                                     PaintPrivate.Prop.FontSub}
                  END
                END
              END
            END
        | PC.ExtensionCom =>
            x := ext^.fnt;
            IF x > 0 AND x MOD 2 = 1 AND x < nf THEN
              fn := st.fonts[x DIV 2];
              IF fn # NIL THEN ext^.fnt := fn.id END
            END;
            x := ext^.pm;
            IF x > 0 AND x MOD 2 = 1 AND x < np THEN
              pm := st.pixmaps[x DIV 2]
            ELSIF x MOD 2 = 0 THEN
              pm := JoinPixmap.Resolve(st, cst, x)
            END;
            IF pm # NIL THEN ext^.pm := pm.id END
        ELSE                     (* skip *)
        END
      END;
      cptr := BatchUtil.Succ(ba, cptr)
    END
  END MungeBatch;
  
PROCEDURE PaintBatch (v, ch: VBT.T; ba: Batch.T) =
  VAR
    pst      : ScreenType.T;
  BEGIN                          (* LL = ch *)
    LOCK v DO
      pst := v.st;
      IF ch.st # pst THEN
        TYPECASE ch.st OF
          NULL =>                (* skip *)
        | T (st) => BatchUtil.Tighten(ba); MungeBatch(ba, pst, st)
        ELSE (*skip*)
        END
      END
    END;
    VBTClass.PaintBatch(v, ba);
  END PaintBatch;

REVEAL VBT.Split <: MouseSplit.Public;

PROCEDURE SetCursor (v: VBT.Split; ch: VBT.T) =
  VAR
    pst: VBT.ScreenType;
    cs                := ch.getcursor();
  BEGIN                          (* LL = ch *)
    LOCK v DO
      pst := v.st;
      IF ch.st # pst THEN
        TYPECASE ch.st OF
          NULL =>                (* skip *)
        | T  =>
            IF cs.id > 0 AND cs.id MOD 2 = 1 AND pst # NIL
                 AND cs.id DIV 2 < NUMBER(pst.cursors^) THEN
              cs := pst.cursors[cs.id DIV 2]
            END
        ELSE
        END
      END;
      IF cs # v.effectiveCursor THEN
        v.effectiveCursor := cs;
        IF v.parent # NIL THEN v.parent.setcursor(v) END
      END                        (* IF *)
    END
  END SetCursor;

BEGIN END JoinScreen.
