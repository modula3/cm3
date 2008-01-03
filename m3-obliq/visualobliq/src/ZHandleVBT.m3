(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Thu Jul  7 08:58:55 PDT 1994 by bharat *)
(*      modified on Wed Nov 17 16:06:29 PST 1993 by mhb    *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

MODULE ZHandleVBT;

IMPORT Attributes, Axis, Dialog, FeedbackVBT, Filter, FlexVBT, FormsVBT, 
       Fmt, HighlightVBT,
       KnobsVBT, NodeVBT, PaintOp, Pixmap, Point, Rect, Region, SourceVBT,
       Split, Stdio, Text, Thread, VBT, VBTClass, VBTColors, Wr, ZGrowVBT,
       ZMoveVBT, ZSplit, ZSplitUtils;

TYPE Style = {Grow, Move, None};

<* FATAL Split.NotAChild *>
<* FATAL FormsVBT.Error *>
<* FATAL FormsVBT.Unimplemented *>
<* FATAL Wr.Failure *>
<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED "VO-ZHandleVBT" OBJECT
        active                     := Style.None;
        handler : Filter.T;
        knobs   : KnobsVBT.T;
        mover   : ZMoveVBT.T;
        grower  : ZGrowVBT.T;
        handleOn                   := FALSE;
        top     : BOOLEAN;
        child   : VBT.T;
        hl      : HighlightVBT.T;
      OVERRIDES
        init := Init;

        (* Since it is a ZSplit it decides which child is going to get the
           events *)

        on           := ZOn;
        off          := ZOff;
        getchild     := GetChild;
        getDomain    := GetDomain;
        replaceChild := ReplaceChild;
      END;

  Selection = PublicSelect BRANDED "VO-Selection" OBJECT
                size          : CARDINAL;
                selectedObject: REF ARRAY OF T;
                selected      : REF ARRAY OF BOOLEAN;
                singleMode    := TRUE;
                dialog        : Dialog.T;
              OVERRIDES
                init             := InitSelection;
                on               := On;
                off              := Off;
                getSelectionSize := GetSelectionSize;
                getSelection     := GetSelection;
                inSingleMode     := InSingleMode;
                alignSelectedObjects := AlignSelectedObjects;
                shapeSelectedObjects := ShapeSelectedObjects;
                distributeSelectedObjects := DistributeSelectedObjects;
              END;


(*******  ZHandleVBT procedure & method definitions ********)


PROCEDURE Init (v: T; ch: VBT.T; selection: Selection := NIL): T =
  VAR f1, f2: FeedbackVBT.T;
  BEGIN
    v.child := ch;
    v.selection := selection;
    v.knobs := NEW(KnobsVBT.T).init(ch);
    f1 := NEW(Feedback).init(v.knobs);
    v.mover := NEW(Mover, zhandle := v).init(f1);
    f2 := NEW(FeedbackVBT.T).init(v.mover);
    v.grower := NEW(Grower, zhandle := v).init(f2);
    v.handler := NEW(Handler, zhandle := v).init(v.grower);
    RETURN ZSplit.T.init(v, v.handler)
  END Init;


PROCEDURE NewSelection(n : T; s : Selection) =
  BEGIN
    n.selection := s;
  END NewSelection;

PROCEDURE ZOn (v: T; singlemode: BOOLEAN) =
  BEGIN
    KnobsVBT.SetSingleMode(v.knobs, singlemode);
    KnobsVBT.Add(v.knobs);
    VBT.Mark(v.knobs);
    v.handleOn := TRUE
  END ZOn;

PROCEDURE ZOff (v: T) =
  BEGIN
    KnobsVBT.Remove(v.knobs);
    v.handleOn := FALSE;
  END ZOff;

PROCEDURE GetChild (v: T): VBT.T =
  BEGIN
    RETURN v.child;
  END GetChild;

PROCEDURE GetDomain (v: T): Rect.T =
  BEGIN
    RETURN VBT.Domain(v);
  END GetDomain;

PROCEDURE ReplaceChild (v: T; fv: VBT.T) =
  BEGIN
    EVAL Filter.Replace(v.knobs, fv);
    v.child := fv;
  END ReplaceChild;


PROCEDURE GetGridSize(v: T) : INTEGER =
  (* fetches gridsize from selection's dialog *)
  BEGIN
    RETURN v.selection.dialog.grid;
  END GetGridSize;
(****************************************************************************)
TYPE
  (* This regulates the turning on/off of the handle and the passing of
     events *)
  Handler = Filter.T OBJECT
              zhandle         : T;
              startpt, endpt  : Point.T;
              controlledChange: BOOLEAN;
              changeInProgress: BOOLEAN;
              chordCancel     : BOOLEAN;
              offclick        : BOOLEAN;
              
            OVERRIDES
              mouse    := Mouse;
              position := Position;
            END;

PROCEDURE Mouse (h: Handler; READONLY cd: VBT.MouseRec) =
  VAR
    v                         := h.zhandle;
    par                       := VBT.Parent(v);
    singleMode      : BOOLEAN;
    dom, innerdomain: Rect.T;
    switchedOn      := FALSE;
    switchedOff     := FALSE;
    child           : VBT.T;
    grid            := GetGridSize(v);
    doubleclick := (cd.clickType = VBT.ClickType.LastUp)
                       AND (cd.clickCount = 3);
    newcd := cd;
  BEGIN
    IF doubleclick THEN Attributes.Invoke(Dialog.attributes,
                                          NARROW(v, NodeVBT.T));
      NodeVBT.print("Invoking attributes for " & NARROW(v, NodeVBT.T).name 
         & "\n");
      newcd.clickType := VBT.ClickType.OtherDown; 
      (* simulate a chord cancel *) 
      Filter.T.mouse(h, newcd);
      RETURN;
    END;
    
    singleMode := NOT (VBT.Modifier.Control IN cd.modifiers);

    IF cd.clickType = VBT.ClickType.FirstDown THEN
      h.startpt := cd.cp.pt;
      v.top := Split.Succ(par, NIL) = v;

      (* This is set if v is the top child of the zsplit...  here we assume
         that the parent of the ZHandle is a zsplit *)

      IF NOT v.handleOn THEN     (* turn the handle on *)
        h.offclick := FALSE;
        switchedOn := TRUE;
        IF v.selection # NIL THEN
          v.selection.on(v, singleMode)
        ELSE
          v.on(TRUE)             (* ignore modifier *)
        END
      ELSE
        h.offclick := TRUE       (* the handle was previously on, maybe
                                    offclick *)
      END;

    END;

    IF cd.clickType = VBT.ClickType.FirstDown AND v.handleOn THEN
      IF KnobsVBT.Inside(v.knobs, cd.cp.pt) THEN
        v.active := Style.Grow;
        (* Since grow is starting - if this is a split node we need to set
           size constraints to accomadate the children *)
        IF ISTYPE(v, NodeVBT.SplitNode) THEN
          (* first compute the required minimum size of the split *)

          dom := ZSplit.GetDomain(v);
          innerdomain := dom;
          innerdomain.south := innerdomain.north + 50;
          innerdomain.east := innerdomain.west + 50; (* minimum size *)
          child := Split.Succ(v, NIL); (* Top *)
          WHILE child # h DO     (* h should be bottom *)
            innerdomain := Rect.Join(innerdomain, VBT.Domain(child));
            child := Split.Succ(v, child);
          END;
          (* innerdomain is the minimum sized rectangle *)
          dom := Rect.Join(dom, innerdomain);
          ZSplit.Move(v, dom);

          WITH fv = NARROW(v.child, FormsVBT.T),
               nv = NARROW(v, NodeVBT.T),
               flexvbt = NARROW(FormsVBT.GetVBT(fv, nv.name & "shape"),
                                FlexVBT.T),
               vpixpermm = VBT.MMToPixels(v, 1.0, Axis.T.Ver),
               hpixpermm = VBT.MMToPixels(v, 1.0, Axis.T.Hor)  DO

            FlexVBT.SetRange(
              flexvbt, Axis.T.Hor,
              FlexVBT.SizeRange{
                FLOAT(Rect.HorSize(dom)) / hpixpermm,
                FLOAT(Rect.HorSize(dom) - Rect.HorSize(innerdomain))
                  / hpixpermm, FlexVBT.Infinity});
            FlexVBT.SetRange(
              flexvbt, Axis.T.Ver,
              FlexVBT.SizeRange{
                FLOAT(Rect.VerSize(dom)) / vpixpermm,
                FLOAT(Rect.VerSize(dom) - Rect.VerSize(innerdomain))
                  / vpixpermm, FlexVBT.Infinity});
          END

        END

      ELSE
        v.active := Style.Move;

      END;
      h.chordCancel := FALSE;
      h.changeInProgress := TRUE;
      h.controlledChange := VBT.Modifier.Control IN cd.modifiers;

    ELSIF cd.clickType = VBT.ClickType.OtherDown THEN
      h.chordCancel := TRUE
    END;

    Filter.T.mouse(h, cd);

    IF cd.clickType = VBT.ClickType.LastUp THEN
      h.endpt := cd.cp.pt;
      IF NOT h.chordCancel AND v.top AND h.offclick
           AND Point.DistSquare(h.startpt, h.endpt) < 6 THEN (* Turn off *)
        IF v.selection # NIL THEN
          v.selection.off(v, singleMode)
        ELSE
          v.off()
        END;
        switchedOff := TRUE;
      END;

      (* if this was a successful change and control wasn't down clamp the
         domain *)

      IF h.changeInProgress AND NOT h.chordCancel AND 
        NOT switchedOn AND NOT switchedOff THEN
        (* house-keeping *)
        IF NOT h.controlledChange THEN
          dom := ZSplit.GetDomain(v);
          ZSplit.Move(
            v, Rect.FromCorners(ClampToGrid(Rect.NorthWest(dom), grid),
                                ClampToGrid(Rect.SouthEast(dom), grid)));
        END;
       
      END;
      h.changeInProgress := FALSE;

      v.active := Style.None;
    END
  END Mouse;

PROCEDURE Position (h: Handler; READONLY cd: VBT.PositionRec) =
  VAR 
    nv :=  NARROW(h.zhandle, NodeVBT.T);
    r  := VBT.Domain(nv);
    nw := Rect.NorthWest(r);
    dialog := nv.selection.dialog;
  BEGIN
   
      IF Rect.Member(cd.cp.pt, r) THEN
        WITH wid = Rect.HorSize(r),
             ht = Rect.VerSize(r) DO
          (*
          NodeVBT.print("Pointer entered " & nv.name & 
          ":" & Fmt.Int(nw.h) & "," & Fmt.Int(nw.v) & ":"  &
          Fmt.Int(wid) & "X" & Fmt.Int(ht) & "\n");
          *)
          FormsVBT.PutText(dialog, "currentobject",
                           nv.name & " = " & Fmt.Int(nw.h) & ","
                           & Fmt.Int(nw.v) & " (H:"  &
                           Fmt.Int(wid) & ", V: " & Fmt.Int(ht) &
                           ")", FALSE);
                           
        END          
      ELSE
        (*        NodeVBT.print("Pointer exited " & nv.name & "\n") *)
        FormsVBT.PutText(dialog, "currentobject", "", FALSE);
      END;
       
    Filter.T.position(h, cd)   
  END Position;

TYPE
  Grower = ZGrowVBT.T OBJECT
             zhandle   : T;
             clipregion: Rect.T;
           OVERRIDES
             shape    := Shape;
             mouse    := GrowMouse;
             position := GrowPosition
           END;

PROCEDURE GrowMouse (v: Grower; READONLY cd: VBT.MouseRec) =
  VAR
    newcd         := cd;
    parent: VBT.T;
    grid := GetGridSize(v.zhandle);
  BEGIN
    IF v.zhandle.active = Style.Grow THEN

      IF cd.clickType = VBT.ClickType.FirstDown THEN

        (* get parents domain and set clip region *)

        parent := VBT.Parent(v.zhandle);
        IF ISTYPE(parent, T) THEN
          v.clipregion := NARROW(parent, T).getDomain();
        ELSE                     (* default *)
          v.clipregion := VBT.Domain(parent);
        END;
        (* to compensate for the open interval *)
        DEC(v.clipregion.east);
        DEC(v.clipregion.south);

      END;

      IF NOT (VBT.Modifier.Control IN cd.modifiers) AND grid > 1 THEN
        newcd.cp.pt := Clip(ClampToGrid(cd.cp.pt, grid), v.clipregion);
      END;

      ZGrowVBT.T.mouse(v, newcd)
    ELSE
      Filter.T.mouse(v, cd)
    END
  END GrowMouse;

PROCEDURE GrowPosition (v: Grower; READONLY cd: VBT.PositionRec) =
  VAR newcd := cd;
      grid := GetGridSize(v.zhandle);
  BEGIN
    IF v.zhandle.active = Style.Grow THEN
      (* round off position points to grid points *)
      IF NOT (VBT.Modifier.Control IN cd.modifiers) AND grid > 1 THEN
        newcd.cp.pt := Clip(ClampToGrid(cd.cp.pt, grid), v.clipregion);
        (* Point.Mul(Point.Div(cd.cp.pt, grid), grid) *)
      END;
      ZGrowVBT.T.position(v, newcd)
    ELSE
      Filter.T.position(v, cd)
    END
  END GrowPosition;


TYPE
  Mover = ZMoveVBT.T OBJECT
            zhandle    : T;
            origin     : Point.T;
            chordCancel: BOOLEAN;
            clipregion : Rect.T;
            innerdomain: Rect.T;
            op         : PaintOp.T;
            last       : Point.T;
            rect       : Rect.T;
            beingMoved               := FALSE;
          OVERRIDES
            shape    := Shape;
            mouse    := MoveMouse;
            position := MovePosition;
            during   := During;
            repaint  := Repaint;
          END;



PROCEDURE ClampToGrid (READONLY p: Point.T; READONLY gridstep: INTEGER):
  Point.T =
  BEGIN
    IF gridstep <= 1 THEN
      RETURN p
    ELSE
      RETURN Point.Mul(Point.Div(Point.Add(p, Point.T{gridstep DIV 2,
                                                      gridstep DIV 2}),
                                 gridstep), gridstep);

    END
  END ClampToGrid;

PROCEDURE Clip (READONLY p: Point.T; READONLY reg: Rect.T): Point.T =
  VAR q := p;
  BEGIN
    IF q.h < reg.west THEN q.h := reg.west END;
    IF q.h > reg.east THEN q.h := reg.east END;
    IF q.v < reg.north THEN q.v := reg.north END;
    IF q.v > reg.south THEN q.v := reg.south END;
    RETURN q;
  END Clip;

PROCEDURE MoveMouse (v: Mover; READONLY cd: VBT.MouseRec) =
  VAR
    newcd := cd;
    grid  := GetGridSize(v.zhandle);
    delta    : Point.T;
    selection          := v.zhandle.selection;
    movee    : T;
    parent   : VBT.T;

  BEGIN

    IF v.zhandle.active = Style.Move THEN
      (* round off position points to grid points *)

      IF NOT (VBT.Modifier.Control IN cd.modifiers) AND grid > 1 THEN
        newcd.cp.pt := ClampToGrid(cd.cp.pt, grid);
      END;

      IF cd.clickType = VBT.ClickType.FirstDown THEN
        v.origin := newcd.cp.pt;
        v.chordCancel := FALSE;


        (* get parents domain and set clip region *)

        parent := VBT.Parent(v.zhandle);
        IF ISTYPE(parent, T) THEN
          v.clipregion := NARROW(parent, T).getDomain();
        ELSE                     (* default *)
          v.clipregion := VBT.Domain(parent);
        END;
        (* to compensate for the open interval *)
        DEC(v.clipregion.east);
        DEC(v.clipregion.south);

        (* now subtract appropriate offsets *)

        v.innerdomain := Rect.Empty;
        WITH bg = Split.Pred(VBT.Parent(ZSplitUtils.FindZChild(v)), NIL) DO
          v.op := VBTColors.Get(bg).transparentSwap
        END;


        FOR i := FIRST(selection.selected^) TO LAST(selection.selected^) DO
          IF selection.selected[i] THEN
            WITH sel = selection.selectedObject[i] DO
              v.innerdomain := Rect.Join(VBT.Domain(sel), v.innerdomain);
              IF sel # v.zhandle THEN
                WITH mvr = NARROW(sel.mover, Mover) DO
                  sel.hl := SourceVBT.GetHighlighter(sel.mover);
                  mvr.op := v.op; (* is this valid ?*)
                  mvr.rect := VBT.Domain(sel);
                  mvr.beingMoved := TRUE;
                END
              END
            END
          END
        END;
        v.last := Point.T{0, 0};
        v.clipregion :=
          Rect.Change(v.clipregion, v.origin.h - v.innerdomain.west,
                      v.origin.h - v.innerdomain.east,
                      v.origin.v - v.innerdomain.north,
                      v.origin.v - v.innerdomain.south);

        TRY                      (* DEBUG MODE *)
          Wr.PutText(
            Stdio.stdout, "Start Move = (" & Fmt.Int(newcd.cp.pt.h) & ","
                            & Fmt.Int(newcd.cp.pt.v) & ")\n");
          Wr.Flush(Stdio.stdout);
        EXCEPT
        ELSE
        END;

      ELSIF cd.clickType = VBT.ClickType.OtherDown THEN
        v.chordCancel := TRUE;
      ELSIF cd.clickType = VBT.ClickType.LastUp THEN
        TRY
          (* remove highlights *)
          FOR i := FIRST(selection.selected^)
              TO LAST(selection.selected^) DO
            IF selection.selected[i] THEN
              WITH sel = selection.selectedObject[i] DO
                IF sel # v.zhandle THEN
                  HighlightVBT.SetRect(sel.hl, Rect.Empty);
                  NARROW(sel.mover, Mover).beingMoved := FALSE;
                END
              END
            END
          END;

          (* DEBUG MODE *)
          IF NOT v.chordCancel THEN
            (* it was a valid move *)
            Wr.PutText(
              Stdio.stdout, "End Move = (" & Fmt.Int(newcd.cp.pt.h) & ","
                              & Fmt.Int(newcd.cp.pt.v) & ")\n");
            Wr.Flush(Stdio.stdout);
            newcd.cp.pt := Clip(newcd.cp.pt, v.clipregion);
            delta := Point.Sub(newcd.cp.pt, v.origin);
            selection := v.zhandle.selection;
            IF NOT selection.inSingleMode() THEN
              FOR i := FIRST(selection.selected^)
                  TO LAST(selection.selected^) DO
                IF selection.selected[i] THEN
                  (* if this is not our zhandle then move it *)
                  IF selection.selectedObject[i] # v.zhandle THEN
                    (* move it by delta *)
                    movee := selection.selectedObject[i];
                    ZSplit.Move(
                      movee, Rect.Add(ZSplit.GetDomain(movee), delta))
                  END
                END
              END
            END;
          ELSE
            Wr.PutText(Stdio.stdout, "Chord Cancel - Move Abandoned\n");
            Wr.Flush(Stdio.stdout);
          END;
        EXCEPT
        ELSE
          Wr.PutText(Stdio.stdout, "Error !#@\n");
          Wr.Flush(Stdio.stdout);
        END;

      END;
      ZMoveVBT.T.mouse(v, newcd);

    ELSE
      Filter.T.mouse(v, cd)
    END
  END MoveMouse;


PROCEDURE Repaint (v: Mover; READONLY badR: Region.T) RAISES {} =
  VAR
    Thickness := ROUND(MAX(VBT.MMToPixels(v, 0.75, Axis.T.Hor),
                           VBT.MMToPixels(v, 0.75, Axis.T.Ver)));
  BEGIN
    ZMoveVBT.T.repaint(v, badR);
    IF v.beingMoved THEN
      WITH newRect = v.rect DO
        HighlightVBT.SetTexture(
          v.zhandle.hl, Pixmap.Gray, Point.Origin, v.op);
        HighlightVBT.SetRect(v.zhandle.hl, newRect, Thickness);
      END
    END;

  END Repaint;


PROCEDURE During (v: Mover; READONLY cd: VBT.PositionRec) =
  VAR
    selection := v.zhandle.selection;
    Thickness := ROUND(MAX(VBT.MMToPixels(v, 0.75, Axis.T.Hor),
                           VBT.MMToPixels(v, 0.75, Axis.T.Ver)));
  BEGIN
    ZMoveVBT.T.during(v, cd);
    IF NOT Point.Equal(cd.cp.pt, v.last) THEN
      IF NOT selection.inSingleMode() THEN
        FOR i := FIRST(selection.selected^) TO LAST(selection.selected^) DO
          IF selection.selected[i] THEN
            (* if this ISTYPE not our zhandle then move its highlight *)
            IF selection.selectedObject[i] # v.zhandle THEN
              WITH newRect = Rect.Move(
                               VBT.Domain(selection.selectedObject[i]),
                               Point.Sub(cd.cp.pt, v.origin)),
                   sel = selection.selectedObject[i] DO
                HighlightVBT.SetTexture(
                  sel.hl, Pixmap.Gray, Point.Origin, v.op);
                HighlightVBT.SetRect(sel.hl, newRect, Thickness);
                v.rect := newRect;
              END
            END
          END
        END
      END
    END;
    v.last := cd.cp.pt;
    (* This should have drawn highlights for all the other selected
       objects *)

  END During;


PROCEDURE MovePosition (v: Mover; READONLY cd: VBT.PositionRec) =
  VAR newcd := cd;
      grid := GetGridSize(v.zhandle);
  BEGIN

    IF v.zhandle.active = Style.Move THEN
      (* round off position points to grid points *)
      IF NOT (VBT.Modifier.Control IN cd.modifiers) AND grid > 1 THEN
        newcd.cp.pt := ClampToGrid(cd.cp.pt, grid);
        (* Point.Mul(Point.Div(cd.cp.pt, grid), grid) *)
      END;
      newcd.cp.pt := Clip(newcd.cp.pt, v.clipregion);

      ZMoveVBT.T.position(v, newcd)
    ELSE
      Filter.T.position(v, cd)
    END
  END MovePosition;

PROCEDURE Shape (v: Filter.T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBTClass.GetShape(Filter.Child(v), ax, n)
  END Shape;

TYPE
  Feedback = FeedbackVBT.T OBJECT
             OVERRIDES
               mouse    := BlockMouse;
               position := BlockPosition;
             END;

PROCEDURE BlockMouse (<* UNUSED *>          v : Feedback;
                      <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END BlockMouse;

PROCEDURE BlockPosition (<* UNUSED *>          v : Feedback;
                         <* UNUSED *> READONLY cd: VBT.PositionRec) =
  BEGIN
  END BlockPosition;

(**********  Selection methods and procedure definitions **************)


PROCEDURE InitSelection (self      : Selection;
                         size      : CARDINAL;
                         singlemode: BOOLEAN;
                         dialog    : FormsVBT.T) =
  BEGIN
    self.dialog := NARROW(dialog, Dialog.T);
    self.selectedObject := NEW(REF ARRAY OF T, size);
    self.selected := NEW(REF ARRAY OF BOOLEAN, size);
    self.size := size;
    FOR i := FIRST(self.selected^) TO LAST(self.selected^) DO
      self.selected[i] := FALSE
    END;
    self.singleMode := singlemode;
  END InitSelection;

PROCEDURE Flush (sel: Selection) =
  BEGIN
    FOR i := FIRST(sel.selected^) TO LAST(sel.selected^) DO
      IF sel.selected[i] THEN
        sel.selected[i] := FALSE;
        sel.selectedObject[i].off() (* hey turn that thing off *)
      END;
    END
  END Flush;

(* Here we implement the policy - override if necessary *)

PROCEDURE On (self: Selection; v: T; singlemode: BOOLEAN) =
  VAR
    par           : VBT.Split;
    other         : T;
    actuallySingle            := TRUE;
    stored                    := FALSE;
  BEGIN
    (* Assert : The ZHandleVBT v is not currently selected *)
    IF (singlemode) THEN
      (* Flush selection *)
      Flush(self);
      self.selected[FIRST(self.selected^)] := TRUE;
      self.selectedObject[FIRST(self.selected^)] := v;
      v.on(TRUE);                (* make it turn its feedback on in single
                                    mode *)
      self.singleMode := TRUE;
    ELSE
      par := VBT.Parent(v);
      FOR i := FIRST(self.selected^) TO LAST(self.selected^) DO

        IF self.selected[i] THEN (* selected ZHandleVBT *)
          other := self.selectedObject[i];
          IF par = VBT.Parent(other) THEN
            (* it is a sibling - so make it gray *)
            actuallySingle := FALSE;
            other.on(FALSE);     (* turn feedback on in multiple mode *)
          ELSE
            other.off();         (* turn feedback off *)
            self.selected[i] := FALSE;
          END
        END;

        IF NOT (stored OR self.selected[i]) THEN
          self.selected[i] := TRUE;
          self.selectedObject[i] := v;
          stored := TRUE;
        END                      (* store new ZHandle in first free slot *)

      END;
      v.on(actuallySingle);
      (* actuallySingle will decide whether it is in multiple mode or
         not *)
      self.singleMode := actuallySingle;
    END;
  END On;

PROCEDURE Off (self: Selection; v: T; singlemode: BOOLEAN) =
  VAR
    multiplicity    := 0;
    last        : T;
  BEGIN
    (* Assert : The ZHandleVBT.T v is currently ON and in the selection
       list *)

    IF (singlemode) THEN
      Flush(self);
      self.singleMode := TRUE;   (* Nothing selected senor *)
    ELSE                         (* turn off only current element *)
      FOR i := FIRST(self.selected^) TO LAST(self.selected^) DO
        IF self.selected[i] THEN
          IF self.selectedObject[i] = v THEN
            self.selected[i] := FALSE
          ELSE
            INC(multiplicity);
            last := self.selectedObject[i];
          END
        END
      END;
      (* Any on handles should currently be in multiple mode *)

      self.singleMode := (multiplicity < 2);
      (* If multiplicity is 1 then it is now single mode and last needs
         to *)
      (* be switched to single mode *)
      IF multiplicity = 1 THEN
        last.on(TRUE);           (* multiple to single *)
      END;
    END;

    v.off();
  END Off;

PROCEDURE GetSelectionSize (self: Selection): CARDINAL =
  VAR ct: CARDINAL := 0;
  BEGIN
    FOR o := FIRST(self.selected^) TO LAST(self.selected^) DO
      IF self.selected[o] THEN INC(ct) END;
    END;
    RETURN ct;
  END GetSelectionSize;


PROCEDURE GetSelection (self: Selection; indx: CARDINAL): T =
  VAR selno := 0;
  BEGIN
    FOR i := FIRST(self.selected^) TO LAST(self.selected^) DO
      IF self.selected[i] THEN INC(selno) END;
      IF selno = indx THEN RETURN self.selectedObject[i]; END
    END;
    RETURN NIL;
  END GetSelection;

PROCEDURE InSingleMode (self: Selection): BOOLEAN =
  BEGIN
    RETURN self.singleMode
  END InSingleMode;

PROCEDURE ComputeCumulative(style: TEXT;
  cumulative: INTEGER; val: INTEGER): INTEGER =
  BEGIN

    (* uninitialized *)
    IF cumulative = -1 THEN RETURN val; END;
  
    IF Text.Equal(style, "alignAvg") OR Text.Equal(style, "useAvg") THEN
     RETURN cumulative + val;
    ELSIF ((Text.Equal(style, "alignMin") OR Text.Equal(style, "useMin"))  AND val < cumulative)
      OR ((Text.Equal(style, "alignMax") OR Text.Equal(style, "useMax")) AND val > cumulative)
     THEN 
      RETURN val;
    ELSE RETURN cumulative  (* This works for First as well *)
    END;   
  END ComputeCumulative;


PROCEDURE ShapeSelectedObjects(self: Selection; mode: TEXT;  shapingStyle: TEXT) =
  VAR
    ct := self.getSelectionSize();
    cumulative : INTEGER := -1;
  BEGIN
    IF ct < 2 THEN 
      Dialog.message(self.dialog, "The Shaping Operation Is Not Currently Applicable");
      RETURN; 
    END; (* Not Applicable *)
    
    (* So far only 2 modes at this pt : EqualWidth and EqualHeight *)

    (* Pass 1 -  compute the cumulative side length*)
    FOR i := FIRST(self.selected^)  TO LAST(self.selected^) DO
      IF self.selected[i] THEN
        WITH rc = ZSplit.GetDomain(self.selectedObject[i]) DO
          IF Text.Equal(mode, "EqualWidth") THEN
            cumulative := ComputeCumulative(shapingStyle, cumulative, rc.east - rc.west);
          ELSE
            cumulative := ComputeCumulative(shapingStyle, cumulative, rc.south - rc.north);
          END (* IF *)
        END (* WITH *)
      END
    END (* FOR *);

    IF Text.Equal(shapingStyle, "useAvg") THEN
      cumulative := cumulative DIV ct;
    END;

    FOR i := FIRST(self.selected^)
      TO LAST(self.selected^) DO
      IF self.selected[i] THEN
        VAR  rect := ZSplit.GetDomain(self.selectedObject[i]);
        BEGIN
          IF Text.Equal(mode, "EqualWidth") THEN
            WITH objwid = rect.east - rect.west,
                 increasedWid = cumulative - objwid,
                 changePerSide = increasedWid DIV 2 DO
              rect.east := rect.east + changePerSide;
              rect.west := rect.west - changePerSide;
            END (* WITH *)
          ELSE (* EqualHt ! *)
            WITH objht = rect.south - rect.north,
                 increasedHt = cumulative - objht,
                 changePerSide = increasedHt DIV 2 DO
              rect.south := rect.south + changePerSide;
              rect.north := rect.north - changePerSide;
            END (* WITH *)
          END (* IF *);
          ZSplit.Move(self.selectedObject[i], rect);
        END (* BEGIN *)
      END (* IF *)
    END (* FOR *)
    
  END ShapeSelectedObjects;
 
PROCEDURE AlignSelectedObjects(self: Selection; mode: TEXT; 
  alignmentStyle: TEXT; dontStretch: BOOLEAN:=FALSE) =
  VAR
    ct := self.getSelectionSize();
    cumulative : INTEGER := -1;
  BEGIN

    IF ct < 2 THEN 
      Dialog.message(self.dialog, "The Alignment Operation Is Not Currently Applicable");
      RETURN; 
    END; (* Not Applicable *)

    (* Pass 1 -  compute the cumulative edge coord*)
  
      FOR i := FIRST(self.selected^)  TO LAST(self.selected^) DO
      IF self.selected[i] THEN
        WITH rc = ZSplit.GetDomain(self.selectedObject[i]) DO 
          IF Text.Equal(mode, "AlignNorth") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative, rc.north);
          ELSIF Text.Equal(mode, "AlignSouth") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative, rc.south);
          ELSIF Text.Equal(mode, "AlignEast") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative, rc.east);
          ELSIF Text.Equal(mode, "AlignWest") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative, rc.west);
          ELSIF Text.Equal(mode, "AlignCenVert") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative,  (rc.east + rc.west) DIV 2);
          ELSIF Text.Equal(mode, "AlignCenHoriz") THEN
            cumulative := ComputeCumulative(alignmentStyle, cumulative,  (rc.north + rc.south) DIV 2);
          END (* IF *)
        END (* WITH *)
      END (* IF *);
    END (* FOR *);

    IF Text.Equal(alignmentStyle, "alignAvg") THEN
      cumulative := cumulative DIV ct;
    END;

      (* Pass 2 - set value to cumulative  *)

           FOR i := FIRST(self.selected^)
             TO LAST(self.selected^) DO
             IF self.selected[i] THEN
              VAR  rect := ZSplit.GetDomain(self.selectedObject[i]);
              BEGIN
                IF Text.Equal(mode, "AlignNorth") THEN
                  IF dontStretch THEN rect.south := rect.south + cumulative - rect.north; END; (* IF *)
                  IF cumulative < rect.south OR dontStretch THEN rect.north := cumulative;  END; (* IF *)
                ELSIF Text.Equal(mode, "AlignSouth") THEN
                  IF dontStretch THEN rect.north := rect.north + cumulative - rect.south; END; (* IF *)
                  IF cumulative > rect.north OR dontStretch THEN rect.south := cumulative; END; (* IF *)
                ELSIF Text.Equal(mode, "AlignEast") THEN
                  IF dontStretch THEN rect.west := rect.west + cumulative - rect.east; END; (* IF *)
                  IF cumulative > rect.west OR dontStretch THEN rect.east := cumulative; END; (* IF *)
                 ELSIF Text.Equal(mode, "AlignWest") THEN
                   IF dontStretch THEN rect.east := rect.east + cumulative - rect.west; END; (* IF *)
                   IF cumulative < rect.east OR dontStretch  THEN  rect.west := cumulative; END; (* IF *)
                 ELSIF Text.Equal(mode, "AlignCenVert") THEN
                   WITH  horizontalShift =cumulative - (rect.east + rect.west) DIV 2 DO
                     rect.east := rect.east + horizontalShift;
                     rect.west := rect.west + horizontalShift;
                   END (* WITH *)
                 ELSIF Text.Equal(mode, "AlignCenHoriz") THEN
                   WITH  verticalShift = cumulative - (rect.north + rect.south) DIV 2 DO
                     rect.north := rect.north + verticalShift;
                     rect.south := rect.south + verticalShift;
                   END (* WITH *)
                 END (* IF *);
                ZSplit.Move(self.selectedObject[i], rect);
              END (* BEGIN *)
            END (* IF *)
           END (* FOR *)

  END AlignSelectedObjects;
    
PROCEDURE DistributeSelectedObjects(self: Selection; mode: TEXT) =
  VAR 
    ct := self.getSelectionSize();
    tmp := NEW(REF ARRAY OF T, ct);
    tmpsize := NEW(REF ARRAY OF INTEGER, ct);
    horizontally := Text.Equal(mode, "DistHoriz");
    sigma_dimension : INTEGER := 0;
    avail_space, max_edge : INTEGER;
    objrect : Rect.T;
    par : VBT.T; 
  BEGIN

    IF ct < 1 THEN 
      Dialog.message(self.dialog, "No Objects Selected !");
      RETURN; 
    END; (* Not Applicable *)

    (* Copy the selected objects to another array and compute sigma-dimension  at 
       the same time *)
     FOR i := 0 TO  ct-1  DO
       WITH  q = FIRST(self.selectedObject^) + i,
             rect = ZSplit.GetDomain(self.selectedObject[q]),
             r = FIRST(tmp^) +   i
        DO
         tmp[r]  := self.selectedObject[q];
         IF horizontally THEN
           tmpsize[r] := rect.east - rect.west + 1;
         ELSE
           tmpsize[r] := rect.south - rect.north + 1;
         END (* IF *);
         sigma_dimension := sigma_dimension + tmpsize[r];
       END (* WITH *)
     END (* FOR *);

     par :=  VBT.Parent(tmp[FIRST(tmp^)]);
     
     IF NOT ISTYPE(par, T) THEN
       Dialog.message(self.dialog, "There is no use in moving a form!");
       RETURN;
     END;
 
    WITH  rpar =  ZSplit.GetDomain(par) DO
       IF horizontally THEN
         avail_space := rpar.east - rpar.west + 1;
         max_edge := rpar.east;
       ELSE
         avail_space := rpar.south - rpar.north + 1;
         max_edge := rpar.south;
       END (* IF *)
    END (* WITH *);
     
    IF avail_space < sigma_dimension THEN 
      Dialog.message(self.dialog, "No space to distribute !");
      RETURN; 
    END; (* Not Applicable *)
 
      

     (* Bubble-sort the array based on tmp_center_coord *)
      WITH leeway = avail_space - sigma_dimension,
           leewayPerWidget = leeway DIV (ct + 1) DO
        FOR swapsNotNeeded := 1 TO ct  DO         
          (* Do a ripple *)
          FOR ix := FIRST (tmp^) TO LAST(tmp^) - swapsNotNeeded DO
            WITH rect1 =  ZSplit.GetDomain(tmp[ix]),
                 rect2 = ZSplit.GetDomain(tmp[ix+1]) DO
            (* if ix > ix+1 then swap *)
              IF (horizontally AND (Rect.Middle(rect1).h > Rect.Middle(rect2).h)) OR
               (NOT horizontally AND (Rect.Middle(rect1).v > Rect.Middle(rect2).v)) THEN
                VAR 
                  t := tmp[ix];
                  tsize := tmpsize[ix];
                BEGIN (* swap *)
                  tmp[ix] := tmp[ix+1]; tmp[ix+1] := t;
                  tmpsize[ix] := tmpsize[ix+1]; tmpsize[ix+1] := tsize;
                END (* BEGIN *);
              END (* IF *)
            END (* WITH *)
          END (* FOR ix*);
          
          (* The LAST(tmp^) - swapsNotNeeded + 1 th element is in place
             so move it *)
          
          WITH elem = LAST(tmp^) - swapsNotNeeded + 1,
               element = tmp[elem],
               (* larger edge should be at avail_space -  leewayPerWidget *)
               largerEdge =  max_edge -  leewayPerWidget,
               smallerEdge = largerEdge - tmpsize[elem] + 1
           DO
            objrect := ZSplit.GetDomain(element);
            IF horizontally THEN
              objrect.west := smallerEdge;
              objrect.east := largerEdge;
            ELSE
              objrect.north := smallerEdge;
              objrect.south := largerEdge;
            END;
            (* move the blighter *)
            ZSplit.Move(element, objrect);
            max_edge := max_edge - leewayPerWidget - tmpsize[elem];
          END (* WITH *) 
        END (* FOR swapsNotNeeded*)
      END (* WITH *);

  END DistributeSelectedObjects;
  

(*********** End of Selection Methods and Procedures ***********)
BEGIN
END ZHandleVBT.


