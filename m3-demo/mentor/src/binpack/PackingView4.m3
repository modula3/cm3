(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jan 31 15:40:39 PST 1995 by kalsow *)
(*      modified on Thu Jan  5 15:45:50 PST 1995 by najork *)
(*      modified on Fri Jul  9 00:30:08 PDT 1993 by mhb    *)
(*      modified on Thu Jan  7 13:36:12 PST 1993 by steveg *)
<* PRAGMA LL *>

MODULE PackingView4;

IMPORT BinpackIE, GraphVBT, PackingView2, PackingView3, PaintOp,
       Rect, RefList, Thread, VBT, View, ZeusPanel;

TYPE Weight = PackingView2.Weight;

REVEAL
  T = PackingView3.T BRANDED OBJECT
      OVERRIDES
        <* LL=0 *>
        createGraph := CreateGraph;
      END;

TYPE
  PickableGraphVBT =
    GraphVBT.T BRANDED OBJECT
      view   : T;
      armed  : BOOLEAN   := FALSE; (* T while mouse down *)
      hot    : BOOLEAN   := FALSE; (* T=>mousing on a weight *)
      target : Weight;          (* what's being moused *)
      overlay: Weight;         (* overlay highlights target *)
    OVERRIDES
      <* LL=VBT.mu *>
      mouse    := Mouse;
      position := Position;
    END;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(PickableGraphVBT).init())
  END New;

PROCEDURE CreateGraph (view: T; nBins, nWts: INTEGER):
  GraphVBT.T =
  VAR
    old: GraphVBT.T := PackingView3.T.createGraph(
                         view, nBins, nWts);
    new: PickableGraphVBT := NEW(PickableGraphVBT,
                                 world := old.world,
                                 pixelSizeDivisor :=
                                   old.pixelSizeDivisor).init();
  BEGIN
    new.view := view;
    RETURN new;
   END CreateGraph;

PROCEDURE Mouse (mg: PickableGraphVBT; READONLY cd: VBT.MouseRec) =
  <* FATAL Thread.Alerted *>
  VAR target: Weight;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      mg.armed := TRUE;
      target := FindTarget(mg, cd.cp);
      IF target # NIL THEN
        mg.hot := TRUE;
        mg.target := target;
        Highlight(mg);
        VBT.SetCage(mg, VBT.CageFromPosition(cd.cp))
      END
    ELSE
      IF mg.hot THEN
        Lowlight(mg);
        IF (cd.clickType = VBT.ClickType.LastUp)
             AND SameTarget(mg, FindTarget(mg, cd.cp)) THEN
          BinpackIE.TryToDeleteWeight(mg.view, mg.target.id);
        END;
        mg.hot := FALSE;
      END;
      mg.armed := FALSE;
    END;
    mg.redisplay()
  END Mouse;

PROCEDURE Position (         mg: PickableGraphVBT;
                    READONLY cd: VBT.PositionRec   ) =
  VAR target: Weight;
  BEGIN
    IF NOT mg.armed THEN
      VBT.SetCage(mg, VBT.EverywhereCage);
      RETURN
    END;
    target := FindTarget(mg, cd.cp);
    IF target = NIL THEN
      IF mg.hot THEN mg.hot := FALSE; Lowlight(mg) END
    ELSE
      IF mg.hot AND NOT SameTarget(mg, target) THEN
        mg.hot := FALSE;
        Lowlight(mg)
      END;
      IF NOT mg.hot THEN
        mg.hot := TRUE;
        mg.target := target;
        Highlight(mg)
      END
    END;
    VBT.SetCage(mg, VBT.CageFromPosition(cd.cp));
    mg.redisplay()
  END Position;

PROCEDURE FindTarget (         mg: PickableGraphVBT;
                      READONLY cp: VBT.CursorPosition): Weight =
  VAR vList: RefList.T;
  BEGIN
    IF cp.gone THEN RETURN NIL END;
    LOCK mg.mu DO
      vList := mg.verticesAt(Rect.FromPoint(cp.pt))
    END;
    IF vList = NIL THEN
      RETURN NIL
    ELSE
      RETURN vList.head
    END
  END FindTarget;

PROCEDURE Highlight (mg: PickableGraphVBT) =
  BEGIN
    mg.overlay :=
      NEW(Weight, graph := mg, id := mg.target.id,
          pos := mg.target.pos, size := mg.target.size,
          border := 0.005, borderColor := PaintOp.Bg).init();
  END Highlight;

PROCEDURE Lowlight (mg: PickableGraphVBT) =
  BEGIN
    LOCK mg.mu DO mg.overlay.remove() END;
  END Lowlight;

PROCEDURE SameTarget (mg: PickableGraphVBT; w: Weight): BOOLEAN =
  BEGIN
    RETURN w.id = mg.target.id
  END SameTarget;


BEGIN
  ZeusPanel.RegisterView(New, "Pickable Packing", "Binpack");
END PackingView4.









