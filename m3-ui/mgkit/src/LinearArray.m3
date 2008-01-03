(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Thu Feb  4 09:00:51 PST 1993 by steveg   *)

<*PRAGMA LL*>

MODULE LinearArray;

IMPORT
  Animate, Axis, MG, MGPublic, MGV, PaintOp, R2, Thread, VBT;

<* FATAL Thread.Alerted *>

REVEAL
  T = TPublic BRANDED OBJECT
  OVERRIDES
    init := InitT;
    setNextPrev := SetNextPrevT;
    setNextPrevLink := SetNextPrevLinkT;
  END;

PROCEDURE InitT (t: T; v: V; graphic: MG.T): T =
  BEGIN
    EVAL MG.Group.init(t);
    IF t.id # MG.NoID THEN MGPublic.Register(v, t.id, t); END;
    t.graphic := graphic;
    t.addBefore(v, graphic, NIL);
    MG.TranslateToLocked(graphic, v, R2.Origin, TRUE);
    MG.SetPosLocked(t, R2.Origin, v);
    t.setVisible(v, 0.0);
    RETURN t;
  END InitT;

PROCEDURE SetNextPrevT (t: T; v: V; np: NP; nextPrev: T) =
  BEGIN
    IF np = NP.Next THEN
      IF t.next # NIL THEN t.remove(v, t.next); END;
      t.next := nextPrev;
      t.addBefore(v, nextPrev, NIL);
    ELSE
      t.prev := nextPrev;
    END;
  END SetNextPrevT;

PROCEDURE SetNextPrevLinkT (t: T; v: V; np: NP; READONLY link: LinkerRec) =
  VAR old: LinkerRec;
  BEGIN
    IF np = NP.Next THEN
      old := t.linkToNext;
      t.linkToNext := link;
    ELSE
      old := t.linkToPrev;
      t.linkToPrev := link;
    END;
    IF old.from # NIL THEN
      old.fromT.remove(v, old.from);
      old.toT.remove(v, old.to);
    END;
    IF link.toT # NIL THEN link.toT.addAfter(v, link.to, NIL); END;
    IF link.fromT # NIL THEN link.fromT.addAfter(v, link.from, NIL); END;
  END SetNextPrevLinkT;

TYPE LRUD = {Left, Right, Up, Down, None};

REVEAL
  V = VPublic BRANDED OBJECT
        (* set by init *)
        labelDirection: LRUD;
        alignDirection: LRUD;
        listDirection : LRUD;
      OVERRIDES
        init      := InitV;
        shape     := ShapeV;
        clear     := ClearV;
        setLabel  := SetLabelV;
        align     := AlignV;
        insert    := InsertV;
        delete    := DeleteV;
      END;

(* Basic picture: (all parts optional)


             aligned slot

   v.head -> v.head.next -> ...  -> v.tail.prev -> v.tail
     |                                                |
   v.headLabel                                     v.tailLabel


   v.headLabel and v.head stay still

   Each element is a group containing (in painting order - low to high)
   linkToNext, linkToPrev, graphic, next

   If no list or a one element list, then labels will be adjacent. 

   each element is allocated a "slot" of size v.width x v.height.
   adjacent slots are separated by v.dx or v.dy

*)

PROCEDURE InitV (v: V; width, height: REAL): V =
  BEGIN
    EVAL MGV.V.init(v);
    v.width := width;
    v.height := height;
    IF v.linker = NIL THEN v.linker := linkerDefault; END;
    CASE v.axis OF
    | Axis.T.Hor =>
        v.listDirection := LRUD.Right;
        CASE v.alignment OF
        | Alignment.None =>
            v.alignDirection := LRUD.None;
            v.labelDirection := LRUD.Down;
        | Alignment.AboveLeft, Alignment.AboveRight =>
            v.alignDirection := LRUD.Up;
            v.labelDirection := LRUD.Down;
        | Alignment.BelowLeft, Alignment.BelowRight =>
            v.alignDirection := LRUD.Down;
            v.labelDirection := LRUD.Up;
        END;
    | Axis.T.Ver =>
        v.listDirection := LRUD.Down;
        CASE v.alignment OF
        | Alignment.None =>
            v.alignDirection := LRUD.None;
            v.labelDirection := LRUD.Right;
        | Alignment.AboveLeft, Alignment.BelowLeft =>
            v.alignDirection := LRUD.Left;
            v.labelDirection := LRUD.Right;
        | Alignment.AboveRight, Alignment.BelowRight =>
            v.alignDirection := LRUD.Right;
            v.labelDirection := LRUD.Left;
        END;
    END;
    RETURN v;
  END InitV;

PROCEDURE ShapeV (v: V; axis: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR inter, size, pref: REAL;
  BEGIN
    IF axis = Axis.T.Hor THEN
      inter := v.dx;
      size := v.width;
    ELSE
      inter := v.dy;
      size := v.width;
    END;
    IF axis = v.axis THEN
      pref := FLOAT(MAX(2, v.cntElems)) * (inter + size) - inter;
    ELSE
      IF v.alignDirection = LRUD.None THEN
        pref := size + inter + size;
      ELSE
        pref := 3.0 * size + inter + inter;
      END;
      pref := pref + 2.0 * v.border[axis];
    END;
    WITH p = ROUND(pref) DO
      RETURN VBT.SizeRange{0, p, MAX(VBT.DefaultShape.hi, p + 1)};
    END;
  END ShapeV;

PROCEDURE ClearV (v: V) =
  BEGIN
    MG.ResetLookupsLocked(v);
    TYPECASE v OF
    | Buffer(buf) =>
        FOR i := 0 TO LAST(buf.slots^) DO
          buf.displayList.remove(buf, buf.slots[i]);
          buf.slots[i] := EmptySlot(buf, i);
        END;
        buf.headIndex := 0;
        buf.tailIndex := 0;
    ELSE
      IF v.head # NIL THEN v.displayList.remove(v, v.head); END;
    END;
    v.head := NIL;
    v.tail := NIL;
    v.cntElems := 0;
    AdjustLabels(v);
    WHILE v.aligned # NIL DO
      v.displayList.remove(v, v.aligned);
      v.aligned := v.aligned.next;
    END;
  END ClearV;

TYPE
  FromOrigin = Animate.Linear OBJECT
               OVERRIDES
                 length := FOLength;
                 doStep := FODoStep;
               END;

PROCEDURE FOLength (<* UNUSED *> fo: FromOrigin;
                    <* UNUSED *> v : MG.V;
                    <* UNUSED *> mg: MG.T        ): INTEGER =
  BEGIN
    RETURN 1
  END FOLength;

PROCEDURE FODoStep (fo      : FromOrigin;
                    time    : REAL;
                    timePrev: REAL;
                    v       : MG.V;
                    mg      : MG.T        ) =
  BEGIN
    IF timePrev = 0.0 AND time # 0.0 THEN
      MG.RTranslateLocked(mg, v, fo.vector);
    END;
    IF time = 1.0 AND timePrev # 1.0 THEN
      mg.setVisible(v, 1.0);
    END;
  END FODoStep;

CONST
  Epsilon = 0.01;

PROCEDURE LinearAnimation (v: V; dest: R2.T; t: T) =
  VAR
    pos               := MG.PosLocked(t, v);
    vector: R2.T;
    a     : Animate.T;
  BEGIN
    IF pos = R2.Origin THEN
      a := NEW(FromOrigin, vector := dest).init();
    ELSE
      vector := R2.Sub(dest, pos);
      IF ABS(vector[0]) < Epsilon AND ABS(vector[1]) < Epsilon THEN
        RETURN
      END;
      a := NEW(Animate.Linear, vector := vector).init()
    END;

    IF v.animations = NIL THEN
      v.animations := NEW(Animate.Group).init()
    END;
    v.animations.add(v, NEW(Animate.Composite, t := a, mg := t));
  END LinearAnimation;

(* returns center of v.head's slot *)
PROCEDURE HeadSlot (v: V): R2.T =
  VAR pos: R2.T;
  BEGIN
    pos[0] := v.nw[0] + v.border[Axis.T.Hor] + v.width / 2.0;
    pos[1] := v.nw[1] - v.border[Axis.T.Ver] - v.height / 2.0;

    CASE v.alignDirection OF
    | LRUD.None =>
    | LRUD.Down, LRUD.Up => pos[1] := pos[1] - v.dy - v.height;
    | LRUD.Left, LRUD.Right => pos[0] := pos[0] + v.dx + v.width;
    END;
    RETURN pos;
  END HeadSlot;

PROCEDURE Slot(v: V; i: INTEGER): R2.T =
  VAR pos := HeadSlot(v);
  BEGIN
    IF v.axis = Axis.T.Hor THEN
      pos[0] := pos[0] + FLOAT(i) * (v.dx + v.width);
    ELSE
      pos[1] := pos[1] - FLOAT(i) * (v.dy + v.height);
    END;
    RETURN pos;
  END Slot;

PROCEDURE LabelSlot (v: V; i: INTEGER): R2.T =
  VAR pos := Slot(v, i);
  BEGIN
    CASE v.labelDirection OF
    | LRUD.Right => pos[0] := pos[0] + (v.dx + v.width);
    | LRUD.Left => pos[0] := pos[0] - (v.dx + v.width);
    | LRUD.Down => pos[1] := pos[1] - (v.dy + v.height);
    | LRUD.Up => pos[1] := pos[1] + (v.dy + v.height);
    ELSE
    END;
    RETURN pos;
  END LabelSlot;

(* returns center of v.headLabel's slot *)
PROCEDURE HeadLabelSlot (v: V): R2.T =
  BEGIN
    TYPECASE v OF
    | Buffer (buf) => RETURN LabelSlot(v, HeadIndex(buf))
    ELSE
      RETURN LabelSlot(v, 0);
    END;
  END HeadLabelSlot;

(* returns center of v.tailLabel's slot *)
PROCEDURE TailLabelSlot (v: V): R2.T =
  BEGIN
    TYPECASE v OF
    | Buffer (buf) =>
        IF buf.head = buf.tail THEN
          RETURN LabelSlot(v, buf.headIndex + 1);
        ELSE
          RETURN LabelSlot(buf, TailIndex(buf));
        END;
    ELSE
      IF v.head = v.tail THEN
        RETURN LabelSlot(v, 1);
      ELSE
        RETURN LabelSlot(v, v.cntElems - 1);
      END;
    END;
  END TailLabelSlot;

(* returns the center of the "next" slot in the given direction.  "t" must
   be in the list. *)
PROCEDURE DerivedSlot (v: V; t: T; direction: LRUD): R2.T =
  VAR pos: R2.T;
  BEGIN
    IF t = NIL THEN
      RETURN HeadSlot(v);
    ELSE
      pos := MG.PosLocked(t, v);
    END;
    CASE direction OF
    | LRUD.Left => pos[0] := pos[0] - v.dx - v.width;
    | LRUD.Right => pos[0] := pos[0] + v.dx + v.width;
    | LRUD.Up => pos[1] := pos[1] + v.dy + v.height;
    | LRUD.Down => pos[1] := pos[1] - v.dy - v.height;
    ELSE
    END;
    RETURN pos;
  END DerivedSlot;

PROCEDURE AdjustLabels (v: V; fixLabelLinks := TRUE) =
  VAR
  BEGIN
    IF fixLabelLinks THEN
      FixLabelLink(v, HT.Head);
      FixLabelLink(v, HT.Tail);
    END;
    IF v.headLabel # NIL THEN
      LinearAnimation(v, HeadLabelSlot(v), v.headLabel);
    END;
    IF v.tailLabel # NIL THEN
      LinearAnimation(v, TailLabelSlot(v), v.tailLabel);
    END;
  END AdjustLabels;

PROCEDURE FixLabelLink (v: V; ht: HT) =
  VAR from, to: T;
  BEGIN
    IF v.labelLinks THEN
      CASE ht OF
      | HT.Head => from := v.headLabel; to := v.head;
      | HT.Tail => from := v.tailLabel; to := v.tail; 
      END;
      SetLabelLink(v, from, to);
    END;
  END FixLabelLink;

PROCEDURE SetLabelLink (v: V; from, to: T) =
  VAR linker: LinkerRec;
  BEGIN
    IF from # NIL AND to # NIL THEN
      linker := v.linker.new(v, from, to, NP.Label);
      from.setNextPrevLink(v, NP.Next, linker);
    ELSIF from # NIL THEN
      from.setNextPrevLink(v, NP.Next, LinkerRec{fromT := from, toT := to,
                                                 from := NIL, to := NIL});
    END;
  END SetLabelLink;

PROCEDURE SetNextLink (v: V; from, to: T) =
  VAR linker: LinkerRec;
  BEGIN
    IF from # NIL AND to # NIL THEN
      linker := v.linker.new(v, from, to, NP.Next);
      from.setNextPrevLink(v, NP.Next, linker);
    ELSIF from # NIL THEN
      from.setNextPrevLink(v, NP.Next, LinkerRec{fromT := from, toT := to,
                                                 from := NIL, to := NIL});
    END;
  END SetNextLink;

PROCEDURE SetPrevLink (v: V; from, to: T) =
  VAR linker: LinkerRec;
  BEGIN
    IF from # NIL AND to # NIL THEN
      linker := v.linker.new(v, to, from, NP.Prev);
      (* reverse the order to linker to new so that the
         "from" link comes from the "prev" node and is thus
         painted before (under) the "prev" and "next" elems *)
      from.setNextPrevLink(v, NP.Prev, linker);
    ELSIF from # NIL THEN
      from.setNextPrevLink(v, NP.Prev, LinkerRec{fromT := from, toT := to,
                                                 from := NIL, to := NIL});
    END;
  END SetPrevLink;

PROCEDURE SetLabelV (v: V; ht: HT; graphic: MG.T) =
  VAR
    label       := NEW(T).init(v, graphic);
    old: T;
  BEGIN
    CASE ht OF
    | HT.Head => old := v.headLabel; v.headLabel := label;
    | HT.Tail => old := v.tailLabel; v.tailLabel := label;
    END;

    IF old # NIL THEN v.displayList.remove(v, old) END;
    v.displayList.addBefore(v, label, NIL);

    FixLabelLink(v, ht);
    AdjustLabels(v, FALSE);
  END SetLabelV;

PROCEDURE AlignV (v: V; t, dest: T) =
  BEGIN
    Unalign(v, t);
    v.displayList.addBefore(v, t, NIL);
    t.setNextPrev(v, NP.Next, v.aligned);
    v.aligned := t;
    LinearAnimation(v, DerivedSlot(v, dest, v.alignDirection), t);
  END AlignV;

(* If t is in the aligned list, then remove t from the list and the display
   list *)
PROCEDURE Unalign (v: V; t: T) =
  VAR aligned: T;
  BEGIN
    IF t = v.aligned THEN
      v.aligned := t.next;
      t.setNextPrev(v, NP.Next, NIL);
      v.displayList.remove(v, t);
    ELSIF v.aligned # NIL THEN
      aligned := v.aligned;
      WHILE aligned.next # NIL DO
        IF t = aligned.next THEN
          aligned.setNextPrev(v, NP.Next, t.next);
          t.setNextPrev(v, NP.Next, NIL);
          v.displayList.remove(v, t);
          RETURN;
        END;
        aligned := aligned.next;
      END;
    END;
  END Unalign;

PROCEDURE InsertV (v: V; t, prev: T) =
  BEGIN
    Unalign(v, t);
    IF prev = NIL THEN
      LinearAnimation(v, HeadSlot(v), t);
      IF v.head = NIL THEN
        (* empty list *)
        v.tail := t;
        FixLabelLink(v, HT.Tail);
      ELSE
        LinearAnimation(v, DerivedSlot(v, t, v.listDirection), v.head);
        t.setNextPrev(v, NP.Next, v.head);
        v.head.setNextPrev(v, NP.Prev, t);
        v.displayList.remove(v, v.head);
      END;
      v.displayList.addBefore(v, t, NIL);
      v.head := t;
      FixLabelLink(v, HT.Head);
    ELSE
      LinearAnimation(v, DerivedSlot(v, prev, v.listDirection), t);
      t.setNextPrev(v, NP.Next, prev.next);
      t.setNextPrev(v, NP.Prev, prev);
      prev.setNextPrev(v, NP.Next, t);
      IF t.next # NIL THEN
        t.next.setNextPrev(v, NP.Prev, t);
        LinearAnimation(v, DerivedSlot(v, t, v.listDirection), t.next);
      END;
      IF v.tail = prev THEN
        v.tail := t;
        FixLabelLink(v, HT.Tail);
      END;
    END;
    INC(v.cntElems);
    AdjustLabels(v, FALSE);
  END InsertV;

PROCEDURE DeleteV (v: V; t: T) =
  BEGIN
    Unalign(v, t);
    IF t = v.head THEN
      v.displayList.remove(v, t);
      IF t = v.tail THEN
        v.head := NIL;
        v.tail := NIL;
        FixLabelLink(v, HT.Head);
        FixLabelLink(v, HT.Tail);
      ELSE
        LinearAnimation(v, HeadSlot(v), t.next);
        (* t.next known to exist *)
        v.displayList.addBefore(v, t.next, NIL);
        v.head := t.next;
        FixLabelLink(v, HT.Head);
      END;
    ELSIF t = v.tail THEN
      (* t.prev know to exist, since t not head *)
      t.prev.setNextPrev(v, NP.Next, NIL);
      v.tail := t.prev;
      FixLabelLink(v, HT.Tail);
    ELSE
      (* t.next and t.prev known to exist *)
      LinearAnimation(v, DerivedSlot(v, t, LRUD.None), t.next);
      t.next.setNextPrev(v, NP.Prev, t.prev);
      t.prev.setNextPrev(v, NP.Next, t.next);
    END;
    t.setNextPrev(v, NP.Next, NIL);
    t.setNextPrev(v, NP.Prev, NIL);
    DEC(v.cntElems);
    AdjustLabels(v, FALSE);
  END DeleteV;

PROCEDURE Align(v: V; t, dest: T) =
  BEGIN
    LOCK v.mu DO
      v.align(t, dest);
    END;
    MGV.Animation(v);
  END Align;

PROCEDURE Clear (v:V) =
  BEGIN
    LOCK v.mu DO v.clear(); END;
    MGV.Animation(v);
    VBT.NewShape(v);
  END Clear;

PROCEDURE Delete(v: V; t: T) =
  BEGIN
    LOCK v.mu DO
      v.delete(t);
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END Delete;

PROCEDURE PreInsert (v: V; t, prev: T) =
  BEGIN
    IF t.visible = 0.0 THEN
      t.setVisible(v, 1.0);
      MG.TranslateToLocked(t, v, DerivedSlot(v, prev, v.listDirection));
    END;
  END PreInsert;

PROCEDURE Insert(v: V; t, prev: T) =
  BEGIN
    LOCK v.mu DO
      PreInsert(v, t, prev);
      v.insert(t, prev);
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END Insert;

PROCEDURE Link (v: V; from, to: T) =
  BEGIN
    LOCK v.mu DO SetNextLink(v, from, to); END;
    MGV.Animation(v);
  END Link;

REVEAL
  List = V BRANDED OBJECT
         OVERRIDES
           insert := InsertList;
           delete := DeleteList;
         END;

PROCEDURE InsertList(v: V; t, prev: T) =
  BEGIN
    V.insert(v, t, prev);
    SetNextLink(v, prev, t);
    SetNextLink(v, t, t.next);
  END InsertList;

PROCEDURE DeleteList(v: V; t: T) =
  BEGIN
    SetNextLink(v, t.prev, t.next);
    SetNextLink(v, t, NIL);
    V.delete(v, t);
  END DeleteList;

REVEAL 
  DoublyLinkedList = DoublyLinkedListPublic BRANDED OBJECT 
  OVERRIDES
    insert := InsertDList;
    delete := DeleteDList;
    init := InitDList;
  END;

PROCEDURE InsertDList(v: DoublyLinkedList; t, prev: T) =
  BEGIN
    V.insert(v, t, prev);
    SetNextLink(v, prev, t);
    SetNextLink(v, t, t.next);
    SetPrevLink(v, t, prev);
    SetPrevLink(v, t.next, t);
  END InsertDList;

PROCEDURE DeleteDList(v: DoublyLinkedList; t: T) =
  BEGIN
    SetNextLink(v, t.prev, t.next);
    SetPrevLink(v, t.next, t.prev);
    SetNextLink(v, t, NIL);
    SetPrevLink(v, t, NIL);
    V.delete(v, t);
  END DeleteDList;

PROCEDURE InitDList (v: V; width, height: REAL): V =
  VAR dl := NARROW(v, DoublyLinkedList);
  BEGIN
    IF v.linker = NIL THEN v.linker := LinkerDList; END;
    EVAL V.init(v, width, height);
    IF dl.nextLinkColor = NIL THEN dl.nextLinkColor := PaintOp.bgFg; END;
    IF dl.prevLinkColor = NIL THEN dl.prevLinkColor := PaintOp.bgFg; END;
    RETURN v;
  END InitDList;

VAR
  LinkerDList := NEW(Linker, new := LinkerNewDList);

PROCEDURE LinkerNewDList (<* UNUSED *> l       : Linker;
                                       vv      : V;
                                       from, to: T;
                                       type    : NP      ): LinkerRec =
  VAR
    link   : MG.Line;
    toPos                        := MG.PosLocked(to, v);
    fromPos                      := MG.PosLocked(from, v);
    color  : PaintOp.ColorScheme;
    v                            := NARROW(vv, DoublyLinkedList);
  BEGIN
    CASE type OF
    | NP.Next =>
        IF v.axis = Axis.T.Hor THEN
          toPos[1] := toPos[1] + v.height / 6.0;
          fromPos[1] := fromPos[1] + v.height / 6.0
        ELSE
          toPos[0] := toPos[0] + v.width / 6.0;
          fromPos[0] := fromPos[0] + v.width / 6.0;
        END;
        color := v.nextLinkColor;
    | NP.Prev =>
        IF v.axis = Axis.T.Hor THEN
          toPos[1] := toPos[1] - v.height / 6.0;
          fromPos[1] := fromPos[1] - v.height / 6.0
        ELSE
          toPos[0] := toPos[0] - v.width / 6.0;
          fromPos[0] := fromPos[0] - v.width / 6.0;
        END;
        color := v.prevLinkColor;
    | NP.Label => color := PaintOp.bgFg;
    END;
    link := NEW(MG.Line, weight := 2.0, color := color).init(
              to := toPos, from := fromPos);
    RETURN
      LinkerRec{
        toT := to, fromT := from, to :=
        NEW(MG.LineEnd, line := link, controlsFrom := FALSE).init(),
        from := NEW(MG.LineEnd, line := link, controlsFrom := TRUE).init()}
  END LinkerNewDList;

PROCEDURE Push(v: QSB; t: T) =
  BEGIN
    LOCK v.mu DO
      v.push(t);
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END Push;

PROCEDURE Pop(v: QSB) =
  BEGIN
    LOCK v.mu DO
      EVAL v.pop();
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END Pop;

REVEAL Queue = QSB BRANDED OBJECT 
  OVERRIDES
    push := PushQueue;
    pop := PopQueue;
  END;

PROCEDURE PushQueue (v: QSB; t: T) =
  BEGIN
    PreInsert(v, t, NIL);
    v.insert(t, NIL);
  END PushQueue;

PROCEDURE PopQueue(v: QSB): T =
  VAR t := v.tail;
  BEGIN
    v.delete(t);
    RETURN t;
  END PopQueue;

REVEAL Stack = QSB BRANDED OBJECT 
  OVERRIDES
    push := PushStack;
    pop := PopStack;
  END;

PROCEDURE PushStack (v: QSB; t: T) =
  BEGIN
    PreInsert(v, t, NIL);
    v.insert(t, NIL);
  END PushStack;

PROCEDURE PopStack (v: QSB): T =
  VAR t := v.head;
  BEGIN
    v.delete(t);
    RETURN t;
  END PopStack;

REVEAL
  Buffer = BufferPublic BRANDED OBJECT
           OVERRIDES
             init     := InitBuffer;
             grow     := GrowBufferM;
             push     := PushBuffer;
             pop      := PopBuffer;
           END;

PROCEDURE EmptySlot (v: Buffer; i: INTEGER): MG.T =
  VAR pos: R2.T;
  BEGIN
    IF v.emptyColor = NIL THEN RETURN NIL END;

    pos[0] := v.nw[0] + v.border[Axis.T.Hor];
    pos[1] := v.nw[1] - v.border[Axis.T.Ver];
    CASE v.alignDirection OF
    | LRUD.None =>
    | LRUD.Down, LRUD.Up => pos[1] := pos[1] - v.dy - v.height;
    | LRUD.Left, LRUD.Right => pos[0] := pos[0] + v.dx + v.width;
    END;
    IF v.axis = Axis.T.Hor THEN
      pos[0] := pos[0] + FLOAT(i) * (v.dx + v.width);
    ELSE
      pos[1] := pos[1] - FLOAT(i) * (v.dy + v.height);
    END;
    RETURN NEW(MG.Rectangle, color := v.emptyColor).init(
             pos, R2.T{pos[0] + v.width, pos[1] - v.height}, v);
  END EmptySlot;

<* INLINE *> PROCEDURE TailIndex (v: Buffer): INTEGER =
  BEGIN
    IF v.pushSide = HT.Tail THEN
      VAR i := v.tailIndex;
      BEGIN
        DEC(i);
        IF i < 0 THEN i := LAST(v.slots^); END;
        RETURN i;
      END;
    ELSE
      RETURN v.tailIndex;
    END;
  END TailIndex;

<* INLINE *> PROCEDURE HeadIndex (v: Buffer): INTEGER =
  BEGIN
    IF v.pushSide = HT.Head THEN
      RETURN (v.headIndex + 1) MOD NUMBER(v.slots^)
    ELSE
      RETURN v.headIndex;
    END;
  END HeadIndex;

PROCEDURE InitBuffer (v                : Buffer;
                      width, height    : REAL;
                      size             : CARDINAL;
                      pushSide, popSide: HT        ): Buffer =
  BEGIN
    EVAL V.init(v, width, height);
    LOCK v.mu DO
      v.pushSide := pushSide;
      v.popSide := popSide;
      v.headIndex := 0;
      v.tailIndex := 0;
      v.slots := NEW(REF ARRAY OF MG.T, MAX(1, size));
      FOR i := 0 TO LAST(v.slots^) DO
        v.slots[i] := EmptySlot(v, i);
      END;
    END;
    RETURN v;
  END InitBuffer;

PROCEDURE GrowBufferM (v: Buffer; newSize: CARDINAL) =
  VAR
    cnt                    := NUMBER(v.slots^);
    new: REF ARRAY OF MG.T;
    t  : T;
  BEGIN
    IF newSize <= cnt THEN RETURN END;
    new := NEW(REF ARRAY OF MG.T, newSize);
    IF v.tailIndex >= v.headIndex THEN
      SUBARRAY(new^, 0, cnt) := v.slots^;
      FOR i := cnt TO newSize - 1 DO
        new[i] := EmptySlot(v, i);
      END;
    ELSE
      SUBARRAY(new^, v.headIndex, cnt - v.headIndex) :=
        SUBARRAY(v.slots^, v.headIndex, cnt - v.headIndex);
      FOR i := 0 TO TailIndex(v) DO
        t := v.slots[i];
        MG.TranslateToLocked(t, v, Slot(v, cnt + i));
        new[cnt + i] := t;
      END;
      FOR i := cnt + TailIndex(v) + 1 TO newSize DO
        new[i] := EmptySlot(v, cnt + i);
      END;
      v.tailIndex := v.tailIndex + cnt;
    END;
    v.slots := new;
  END GrowBufferM;

PROCEDURE PushBuffer (v: Buffer; t: T) =
  VAR i, dest: INTEGER;
  BEGIN
    IF v.pushSide = HT.Head THEN
      dest := v.headIndex;
      v.head := t;
      IF v.tail = NIL THEN v.tail := t END;
      i := dest - 1;
      IF i < 0 THEN i := LAST(v.slots^); END;
      v.headIndex := i;
    ELSE
      dest := v.tailIndex;
      v.tail := t;
      IF v.head = NIL THEN v.head := t END;
      v.tailIndex := (dest + 1) MOD NUMBER(v.slots^);
    END;
    v.displayList.remove(v, v.slots[dest]);
    v.slots[dest] := t;
    v.displayList.addAfter(v, t, NIL);
    LinearAnimation(v, Slot(v, dest), t);
    AdjustLabels(v);
  END PushBuffer;

PROCEDURE PopBuffer (v: Buffer): T =
  VAR
    i  : INTEGER;
    res: T;
  BEGIN
    IF v.popSide = HT.Head THEN
      i := HeadIndex(v);
      res := v.slots[i];
      v.slots[i] := EmptySlot(v, i);
      v.headIndex := (v.headIndex + 1) MOD NUMBER(v.slots^);
    ELSE
      i := TailIndex(v);
      res := v.slots[i];
      v.slots[i] := EmptySlot(v, i);
      DEC(v.tailIndex);
      IF v.tailIndex < 0 THEN v.tailIndex := LAST(v.slots^); END;
    END;
    IF v.tailIndex = v.headIndex THEN
      v.head := NIL;
      v.tail := NIL;
    ELSE
      v.tail := v.slots[TailIndex(v)];
      v.head := v.slots[HeadIndex(v)];
    END;
    v.displayList.remove(v, res);
    AdjustLabels(v);
    RETURN res;
  END PopBuffer;

PROCEDURE GrowBuffer(v: Buffer; size: INTEGER) =
  BEGIN
    LOCK v.mu DO
      v.grow(size);
    END;
    AdjustLabels(v);
    MGV.Animation(v);
  END GrowBuffer;
    
PROCEDURE LinkerNewDefault (<* UNUSED *> l       : Linker;
                                         v       : V;
                                         from, to: T;
                            <* UNUSED *> type    : NP      ): LinkerRec =
  VAR link: MG.Line;
  BEGIN
    link := NEW(MG.Line, weight := 2.0).init(
              to := MG.PosLocked(to, v), from := MG.PosLocked(from, v));
    RETURN
      LinkerRec{
        toT := to, fromT := from, to :=
        NEW(MG.LineEnd, line := link, controlsFrom := FALSE).init(),
        from := NEW(MG.LineEnd, line := link, controlsFrom := TRUE).init()}
  END LinkerNewDefault;

BEGIN
  linkerDefault := NEW(Linker, new := LinkerNewDefault);
END LinearArray.
