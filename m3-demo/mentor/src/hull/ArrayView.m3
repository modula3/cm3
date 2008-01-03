(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 21:46:26 PST 1995 by najork *)
(*      modified on Mon Jan 11 15:05:42 PST 1993 by steveg *)
(*      modified on Sat Oct 17 12:16:25 PDT 1992 by ramshaw*)
(*      modified on Wed Jul 29 00:35:12 1992 by saxe       *)

MODULE ArrayView;

IMPORT HullViewClass, Filter, GraphVBT, IntList, MyColors, R2,
       View, ZeusPanel, Site, SiteList, VBT, Random, Thread;

TYPE
  Sites = REF ARRAY OF GraphVBT.Vertex;

  T = HullViewClass.T BRANDED OBJECT
        mg: GraphVBT.T;
        nSites: INTEGER;
        sites: Sites;
        holders: Sites;
        firstTrue, lastTrue: INTEGER;
        testLite: GraphVBT.VertexHighlight; 
        testLiteOn: BOOLEAN;
        headLite, tailLite: GraphVBT.VertexHighlight; 
        headLiteOn, tailLiteOn: BOOLEAN;
        limbo: GraphVBT.Vertex;  
        version: INTEGER;
      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
        oeSetHalfPlane := SetHalfPlane;
        oeTestSite := TestSite;
        oeMoveHalfPlane := MoveHalfPlane;
        oeClearTest := ClearTest;
        oeSetTail := SetTail;
        oeClearHead := ClearHead;
        oeConfirm := Confirm;
        oeSwap := Swap;
        oeSentinel := Sentinel;
        oeReOrder := ReOrder;
        oeShuffle := Shuffle;
      END;

TYPE 
  BezierPath = GraphVBT.AnimationPath BRANDED OBJECT
    p0, p1, p2, p3: R2.T;
  OVERRIDES
    pos := BezierPos
  END;

VAR
  rand := NEW (Random.Default).init ();

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

PROCEDURE Startrun (view: T) =
  BEGIN
    LOCK VBT.mu DO
      EVAL Filter.Replace(view, NEW(GraphVBT.T).init())
    END;
    HullViewClass.T.startrun(view);
  END Startrun;

PROCEDURE Setup (view: T;  trueSites, auxSites: SiteList.T) =
  VAR
    nTrueSites, nAuxSites: INTEGER;
    wc: GraphVBT.WorldRectangle;
    s: Site.T;
    font: GraphVBT.WorldFont;
  BEGIN
    nTrueSites := SiteList.Length(trueSites);
    nAuxSites := SiteList.Length(auxSites);
    view.nSites := nTrueSites;
    wc := GraphVBT.WorldRectangle{
            w := -0.7, e := FLOAT(nTrueSites, REAL)+1.7, 
            s := FLOAT(nTrueSites, REAL)+0.7, n := -0.7};
    view.mg := NEW(GraphVBT.T, world := wc).init();
    font := view.mg.font(family := "Helvetica", weight := "bold", 
                         slant := GraphVBT.Slant.Roman, size := 2.8);
    LOCK VBT.mu DO
      EVAL Filter.Replace(view, view.mg);
    END;
    view.sites := NEW(Sites, nTrueSites+2); (* +2 for sentinels *)
    view.holders := NEW(Sites, nTrueSites+2); (* +2 for sentinels *)
    WHILE trueSites # NIL DO
      s := trueSites.head;
      trueSites := trueSites.tail;
      view.holders[s.uid]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(s.uid), 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            border := 0.0,
            size := R2.T{0.0, 0.0}).init();
      view.sites[s.uid]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(s.uid), 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            color := MyColors.White(),
            font := font,
            fontColor := MyColors.Black(),
            border := 0.003,
            label := s.lab,
            size := R2.T{0.8, 0.8}).init();
    END;
    view.holders[0]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{0.0, 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            size := R2.T{0.0, 0.0}).init();
    view.sites[0]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{0.0, 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            color := MyColors.Gray(),
            font := font,
            label := "",
            size := R2.T{0.8, 0.8}).init();
    view.holders[nTrueSites+1]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(nTrueSites+1,REAL), 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            size := R2.T{0.0, 0.0}).init();
    view.sites[nTrueSites+1]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(nTrueSites+1,REAL), 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            color := MyColors.Gray(),
            font := font,
            label := "",
            size := R2.T{0.8, 0.8}).init();
    view.limbo := NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{-2.0, -2.0},
            shape := GraphVBT.VertexShape.Rectangle,
            size := R2.T{0.0, 0.0}).init();
    view.testLite :=
      NEW(GraphVBT.VertexHighlight, vertex := view.limbo,
          color := MyColors.Test(),
          border := R2.T{0.55, 0.55}).init();
    view.testLiteOn := FALSE;
    view.tailLite :=
      NEW(GraphVBT.VertexHighlight, vertex := view.limbo,
          color := MyColors.Tail(),
          border := R2.T{0.55, 0.55}).init();
    view.tailLiteOn := FALSE;
    view.headLite :=
      NEW(GraphVBT.VertexHighlight, vertex := view.limbo,
          color := MyColors.Head(),
          border := R2.T{0.48, 0.48}).init();
    view.headLiteOn := FALSE;
    view.version := 0;
    view.mg.redisplay();
  END Setup;

PROCEDURE SetHalfPlane(view: T; tail, head: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    view.headLiteOn := FALSE;
    LOCK view.mg.mu DO view.headLite.move(view.limbo) END;
    view.redisplay();
    IF tail <= view.nSites+1 THEN
      IF view.tailLiteOn THEN
        LOCK view.mg.mu DO 
          view.tailLite.move(view.holders[Holder(view,tail)], TRUE) END;
        view.mg.animate(0.0, 1.0);
      ELSE
        LOCK view.mg.mu DO
          view.tailLite.move(view.holders[Holder(view,tail)], FALSE);
        END;
        view.tailLiteOn := TRUE
      END;
    END;
    IF head <= view.nSites+1 THEN
        LOCK view.mg.mu DO
          view.headLite.move(view.holders[Holder(view,head)], FALSE);
        END;
        view.headLiteOn := TRUE
    END;
    view.mg.redisplay();
  END SetHalfPlane;

PROCEDURE TestSite(view: T; i: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    IF view.testLiteOn THEN
      LOCK view.mg.mu DO 
        view.testLite.move(view.holders[Holder(view,i)], TRUE) END;
      view.mg.animate(0.0, 1.0);
    ELSE
      LOCK view.mg.mu DO
        view.testLite.move(view.holders[Holder(view,i)], FALSE);
      END;
      view.mg.redisplay();
      view.testLiteOn := TRUE
    END;
  END TestSite;

PROCEDURE ClearTest(view: T) =
  BEGIN
  LOCK view.mg.mu DO view.testLite.move(view.limbo, FALSE) END;
  view.mg.redisplay();
  view.testLiteOn := FALSE
  END ClearTest;

PROCEDURE ClearHead(view: T) =
  BEGIN
  LOCK view.mg.mu DO view.headLite.move(view.limbo, FALSE) END;
  view.mg.redisplay();
  view.headLiteOn := FALSE
  END ClearHead;

PROCEDURE SetTail(view: T; i: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    IF view.tailLiteOn THEN
      LOCK view.mg.mu DO
        view.tailLite.move(view.holders[Holder(view, i)], TRUE) END;
      view.mg.animate(0.0, 1.0);
    ELSE
      LOCK view.mg.mu DO 
        view.tailLite.move(view.holders[Holder(view, i)], FALSE) END;
      view.mg.redisplay();
      view.tailLiteOn := TRUE
    END;
  END SetTail;

PROCEDURE Holder(view: T; i: INTEGER): INTEGER =
  BEGIN RETURN ROUND(view.sites[i].pos[0]) END Holder;

PROCEDURE MoveHalfPlane(view: T; <* UNUSED *> tail: INTEGER; head: INTEGER) 
    RAISES {Thread.Alerted} =
  BEGIN
    IF view.headLiteOn THEN
      LOCK view.mg.mu DO 
       view.headLite.move(view.holders[Holder(view,head)], TRUE) END;
      view.mg.animate(0.0, 1.0)
    ELSE
      LOCK view.mg.mu DO
        view.headLite.move(view.holders[Holder(view,head)], FALSE);
      END;
      view.mg.redisplay();
      view.headLiteOn := TRUE
    END;
  END MoveHalfPlane;

PROCEDURE Swap(view: T; i,j: INTEGER) RAISES {Thread.Alerted} =
  VAR iPos, jPos, del: R2.T;
      iToJ, jToI: BezierPath;
  BEGIN
  INC(view.version);
  LOCK view.mg.mu DO
    FOR i := 0 TO view.nSites+1 DO
      view.holders[i].move(R2.Add(view.holders[i].pos, R2.Unit(1)), TRUE);
      view.sites[i].move(R2.Add(view.sites[i].pos, R2.Unit(1)), TRUE);
    END;
    view.sites[i].toFront();
    view.sites[j].toFront();
  END;
  view.mg.animate(0.0, 0.5);
  iPos := view.sites[i].pos;
  jPos := view.sites[j].pos;
  del := R2.Scale(2.0, R2.Unit(1));
  iToJ := NEW(BezierPath, p0 := iPos,
                          p1 := R2.Add(iPos, del),
                          p2 := R2.Add(jPos, del),
                          p3 := jPos);
  jToI := NEW(BezierPath, p0 := jPos,
                          p1 := R2.Add(jPos, del),
                          p2 := R2.Add(iPos, del),
                          p3 := iPos);
  LOCK view.mg.mu DO
      view.sites[i].move(jPos, TRUE, path := iToJ);
      view.sites[j].move(iPos, TRUE, path := jToI);
  END;
  view.mg.animate(0.5, 1.5);
  END Swap;

PROCEDURE BezierPos(path: BezierPath; t: REAL): R2.T =
  VAR q0, q1, q2, r0, r1: R2.T;
  BEGIN
  q0 := R2.Mix(path.p0, 1.0-t, path.p1, t);
  q1 := R2.Mix(path.p1, 1.0-t, path.p2, t);
  q2 := R2.Mix(path.p2, 1.0-t, path.p3, t);

  r0 := R2.Mix(q0, 1.0-t, q1, t);
  r1 := R2.Mix(q1, 1.0-t, q2, t);

  RETURN(R2.Mix(r0, 1.0-t, r1, t));
  END BezierPos;

PROCEDURE ReOrder(<* UNUSED *> view: T; <* UNUSED *> l: IntList.T) =
  BEGIN
  END ReOrder;

PROCEDURE Shuffle(view: T; hullSites, otherSites: IntList.T) 
    RAISES {Thread.Alerted} =
  VAR nHullSites: INTEGER;
      newOrder: REF ARRAY OF INTEGER;
      oldPos, newPos: REF ARRAY OF R2.T;
      paths: REF ARRAY OF BezierPath;
      allSites: IntList.T;
      del: R2.T;
  BEGIN
    nHullSites := IntList.Length (hullSites);
    newOrder := NEW (REF ARRAY OF INTEGER, view.nSites+2);
    allSites := IntList.Append(hullSites, otherSites);
    FOR i := 1 TO view.nSites DO
      newOrder[i] := allSites.head;
      allSites := allSites.tail;
    END;
    FOR i := 0 TO view.nSites+1 DO
      view.sites[i]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.Add(view.sites[i].pos, R2.Unit(1)),
            shape := GraphVBT.VertexShape.Rectangle,
            color := view.sites[i].color,
            font := view.sites[i].font,
            fontColor := view.sites[i].fontColor,
            border := view.sites[i].border,
            label := view.sites[i].label,
            size := view.sites[i].size).init();
    END;
    FOR i := 1 TO nHullSites DO
      view.sites[newOrder[i]].setColor(MyColors.Black());
      view.sites[newOrder[i]].setFontColor(MyColors.White());
      view.sites[newOrder[i]].setBorder(0.0);
    END;
    oldPos := NEW(REF ARRAY OF R2.T, view.nSites+2);
    newPos := NEW(REF ARRAY OF R2.T, view.nSites+2);
    paths := NEW(REF ARRAY OF BezierPath, view.nSites+2);
    FOR i := 1 TO view.nSites DO
      oldPos[i] := view.sites[i].pos;
      newPos[newOrder[i]] := oldPos[i];
    END;
    FOR i := 1 TO view.nSites DO
      del := R2.Scale (rand.real (min := 1.5, max := 4.5), R2.Unit(1));
      paths[i] := NEW(BezierPath, 
                      p0 := oldPos[i],
                      p1 := R2.Add(oldPos[i], del),
                      p2 := R2.Add(newPos[i], del),
                      p3 := newPos[i]);
    END;
    LOCK view.mg.mu DO
      FOR i := 1 TO view.nSites DO
         view.sites[i].move(newPos[i], TRUE, path := paths[i]);
      END;
    END;
    view.mg.animate(0.0, 2.0);
  END Shuffle;

PROCEDURE Sentinel(view: T; i, j: INTEGER) =
  BEGIN
  LOCK view.mg.mu DO view.sites[i].setLabel(view.sites[j].label) END;
  view.mg.redisplay();
  END Sentinel;

PROCEDURE Confirm(view: T; tail, head: INTEGER) =
  BEGIN
  FOR i := 0 TO view.nSites+1 DO
    view.sites[i]:=NEW(GraphVBT.Vertex, graph := view.mg,
            pos := view.sites[i].pos,
            shape := GraphVBT.VertexShape.Rectangle,
            color := view.sites[i].color,
            font := view.sites[i].font,
            fontColor := view.sites[i].fontColor,
            border := view.sites[i].border,
            label := view.sites[i].label,
            size := view.sites[i].size).init();
  END;
  LOCK view.mg.mu DO
    view.testLiteOn := FALSE;
    view.testLite.move(view.limbo, FALSE);
    IF view.tailLiteOn THEN 
      view.tailLite.move(view.holders[Holder(view,tail)]) END;
    IF tail <= view.nSites AND tail >= 1 THEN
      view.sites[tail].setColor(MyColors.Black());
      view.sites[tail].setFontColor(MyColors.White());
      view.sites[tail].setBorder(0.0);
    END;
    IF head <= view.nSites AND head >= 1 THEN
      view.sites[head].setColor(MyColors.Black());
      view.sites[head].setFontColor(MyColors.White());
      view.sites[head].setBorder(0.0);
    END;
  END;
  view.mg.redisplay();
END Confirm;

BEGIN
  ZeusPanel.RegisterView (New, "Array View", "Hull");
END ArrayView.


