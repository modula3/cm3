(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 20 11:38:34 PDT 1995 by msm     *)

<* PRAGMA LL *>

MODULE JoinParent;

IMPORT Axis, ETAgent, FilterClass, MouseSplit, Point, Rect, Region,
       ScrnCursor, Trestle, VBT, VBTClass, JoinedVBT, JoinScreen, VBTRep,
       TrestleImpl;

TYPE
  Ref = OBJECT
          <* LL >= {VBT.mu, child} *>
          child        : JoinedVBT.T;
          current      : T              := NIL;
          joinST       : JoinScreen.T;
          needsRescreen                 := TRUE;
          ignoreNextButton := FALSE;
          mouseFocus: T := NIL;
        END;

REVEAL
  T = Public BRANDED OBJECT
        cl   : Ref;
        trsl : Trestle.T      := NIL;
        oldst: VBT.ScreenType := NIL
      OVERRIDES
        init       := Be;
        paintbatch := JoinScreen.PaintBatch;
        setcursor  := JoinScreen.SetCursor;
        discard    := Discard;
        repaint    := Repaint;
        reshape    := Reshape;
        rescreen   := Rescreen;
        misc       := Misc
      END;

REVEAL JoinedVBT.T <: Join;

PROCEDURE Current (v: JoinedVBT.T): T =
  BEGIN
    IF v.parents = NIL OR v.parents.cl = NIL THEN RETURN NIL END;
    RETURN v.parents.cl.current
  END Current;

PROCEDURE ResetCages (v: JoinedVBT.T; prnt: T) =
  VAR
    p  := v.parents;
    cl := prnt.cl;
    cp := VBT.CursorPosition{pt := Point.Origin, screen := VBT.AllScreens,
                             gone := TRUE, offScreen := TRUE};
  BEGIN
    LOCK v DO
      cl.current := prnt;
      VBTClass.ForceEscape(v)
    END;
    IF cl.mouseFocus # NIL THEN
      VBTClass.Position(v, VBT.PositionRec{cp := cp, time := 0,
                                           modifiers := VBT.Modifiers{}});
      VBTClass.Mouse(
        v, VBT.MouseRec{whatChanged := VBT.Modifier.Mouse4, time := 0,
                        cp := cp, modifiers := VBT.Modifiers{},
                        clickType := VBT.ClickType.LastUp, clickCount := 0})
    END;
    cl.mouseFocus := NIL;
    cl.ignoreNextButton := FALSE;
    IF prnt = NIL THEN RETURN END;
    WHILE p # NIL DO
      IF p.trsl = prnt.trsl THEN VBT.SetCage(p, VBT.GoneCage) END;
      p := p.link
    END
  END ResetCages;

PROCEDURE SetInput (v: JoinedVBT.T; prnt: T) =
  VAR curParent: T := NIL; pt: T;
  BEGIN
    IF v.parent # NIL THEN curParent := v.parent.parent END;
    IF curParent = prnt THEN RETURN END;
    IF curParent # NIL AND (prnt = NIL OR curParent.trsl # prnt.trsl) THEN
      pt := v.parents;
      WHILE pt # NIL DO
        IF pt.trsl = curParent.trsl THEN
          ETAgent.ReleaseSelections(pt)
        END;
        pt := pt.link
      END;
      IF prnt # NIL THEN ResetCages(v, prnt) END
    END;
    LOCK v DO
      IF prnt = NIL THEN v.parent := NIL ELSE v.parent := prnt.ch END
    END;
    TrestleImpl.UpdateChalk(prnt);
  END SetInput;

PROCEDURE NeedsRescreen (v: JoinedVBT.T): BOOLEAN =
  VAR p := v.parents;
  BEGIN
    IF p = NIL OR NOT p.cl.needsRescreen THEN RETURN FALSE END;
    LOCK v DO p.cl.needsRescreen := FALSE END;
    RETURN TRUE
  END NeedsRescreen;

PROCEDURE ST (v: JoinedVBT.T): VBT.ScreenType =
  VAR
    p                  := v.parents;
    st: VBT.ScreenType;
  BEGIN
    IF p = NIL THEN RETURN v.st END;
    IF UniformST(p, st) THEN
      IF st = NIL THEN st := p.cl.joinST END;
      WHILE p # NIL DO
        IF p.ch.st # p.st THEN
          VBTClass.Rescreen(p.ch, p.st);
          VBTClass.Reshape(p.ch, p.domain, Rect.Empty)
        END;
        p := p.link;
      END
    ELSE
      st := p.cl.joinST;
      p.cl.joinST.eval();
      WHILE p # NIL DO
        IF p.ch.st # st THEN
          VBTClass.Rescreen(p.ch, st);
          VBTClass.Reshape(p.ch, p.domain, Rect.Empty)
        END;
        p := p.link;
      END
    END;
    RETURN st
  END ST;

PROCEDURE UniformST (p: T; VAR st: VBT.ScreenType): BOOLEAN =
  BEGIN
    st := NIL;
    WHILE p # NIL DO
      IF p.st # st AND p.st # NIL THEN
        IF st = NIL THEN st := p.st ELSE RETURN FALSE END
      END;
      p := p.link
    END;
    RETURN TRUE
  END UniformST;

PROCEDURE Domain (v: JoinedVBT.T): Rect.T =
  VAR
    res := Rect.Empty;
    p   := v.parents;
  BEGIN
    WHILE p # NIL DO res := Rect.Join(res, p.domain); p := p.link END;
    RETURN res
  END Domain;

PROCEDURE NewRef (v: JoinedVBT.T): Ref =
  BEGIN
    RETURN NEW(Ref, child := v, joinST := JoinScreen.New())
  END NewRef;

TYPE
  ChildT = VBT.Split OBJECT
             cs: ScrnCursor.T;
             cl: Ref;
           OVERRIDES
             getcursor := GetCursor;
             succ      := ChSucc;
             setcage   := SetCage;
             setcursor := ChSetCursor;
             position  := Position;
             mouse     := Mouse;
             shape     := Shape;
             axisOrder := AxisOrder;
           END;

PROCEDURE ChSucc (<* UNUSED *> v: ChildT; <* UNUSED *> ch: VBT.T): VBT.T =
  BEGIN
    RETURN NIL
  END ChSucc;

PROCEDURE GetCursor (v: ChildT): ScrnCursor.T =
  BEGIN
    RETURN v.cs
  END GetCursor;

PROCEDURE SetCage (v: ChildT; ch: VBT.T) =
  BEGIN
    VBT.SetCage(v, VBTClass.Cage(ch))
  END SetCage;
 
PROCEDURE ChSetCursor (v: ChildT; ch: VBT.T) =
  VAR cs := ch.getcursor();
  BEGIN
    LOCK v DO
      v.cs := cs;
      IF v.parent # NIL THEN v.parent.setcursor(v) END
    END
  END ChSetCursor;

PROCEDURE SetCursor (v: T; cs: ScrnCursor.T) =
  VAR ch: ChildT := v.ch;
  BEGIN
    LOCK ch DO ch.cs := cs; v.setcursor(ch) END
  END SetCursor;

PROCEDURE Position (v: ChildT; READONLY cd: VBT.PositionRec) =
  VAR
    cl                     := v.cl;
    ch       : JoinedVBT.T;
    par, vpar: T;
    b        : BOOLEAN;
  BEGIN
    IF cl = NIL THEN RETURN END;
    vpar := v.parent;
    b := vpar = cl.current;
    ch := cl.child;
    IF ch.parent # NIL THEN par := ch.parent.parent ELSE par := NIL END;
    IF par # NIL AND par.trsl = vpar.trsl THEN
      IF b = cd.cp.gone THEN
        LOCK ch DO
          IF b THEN
            cl.current := NIL
          ELSE
            b := TRUE;
            cl.current := vpar;
            ch.parent := v
          END
        END
      ELSIF cl.current = NIL AND vpar = par THEN
        b := TRUE
      END;
      IF b THEN VBTClass.Position(ch, cd) END
    END
  END Position;

PROCEDURE Mouse (v: ChildT; READONLY cd: VBT.MouseRec) =
  VAR
    cl                     := v.cl;
    ch       : JoinedVBT.T;
    par, vpar: T;
  BEGIN
    IF cl = NIL THEN RETURN END;
    vpar := v.parent;
    ch := cl.child;
    IF ch.parent # NIL THEN par := ch.parent.parent ELSE par := NIL END;
    IF cd.clickType = VBT.ClickType.FirstDown
         AND (cl.mouseFocus = NIL OR par # NIL AND par.trsl = vpar.trsl) THEN
      SetInput(ch, vpar);
      LOCK ch DO cl.current := vpar; ch.parent := v END;
      cl.mouseFocus := vpar;
      cl.ignoreNextButton := FALSE;
      VBTClass.Position(ch, VBT.PositionRec{cd.cp, cd.time, cd.modifiers});
      VBTClass.Mouse(ch, cd)
    ELSIF cd.clickType = VBT.ClickType.LastUp
            AND cd.whatChanged = VBT.Modifier.Mouse4
            AND cd.modifiers
                  = VBT.Modifiers{VBT.Modifier.Mod0, VBT.Modifier.Mod1,
                                  VBT.Modifier.Mod2, VBT.Modifier.Mod3} THEN
      IF cl.mouseFocus = NIL THEN
        SetInput(ch, vpar);
        cl.ignoreNextButton := FALSE
      END
    ELSIF par # NIL AND par.trsl = vpar.trsl THEN
      IF NOT cl.ignoreNextButton OR NOT cd.cp.gone THEN
        VBTClass.Mouse(ch, cd)
      END;
      cl.ignoreNextButton := vpar # cl.mouseFocus;
      IF NOT cl.ignoreNextButton AND cd.clickType = VBT.ClickType.LastUp THEN
        cl.mouseFocus := NIL
      END
    END
  END Mouse;

PROCEDURE Shape (v: ChildT; axis: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF v.cl = NIL THEN RETURN VBT.Split.shape(v, axis, n) END;
    RETURN VBTClass.GetShape(v.cl.child, axis, n)
  END Shape;

PROCEDURE AxisOrder (v: ChildT): Axis.T =
  BEGIN
    IF v.cl = NIL THEN RETURN VBT.Split.axisOrder(v) END;
    RETURN v.cl.child.axisOrder()
  END AxisOrder;

PROCEDURE Be (prntP: T; v: JoinedVBT.T): T =
  VAR
    cl  : Ref;
    mark      := FALSE;
  BEGIN
    LOCK v DO
      IF v.parents # NIL THEN cl := v.parents.cl ELSE cl := NewRef(v) END;
      prntP.link := v.parents;
      v.parents := prntP;
      prntP.cl := cl;
      prntP.oldst := prntP.st;
      EVAL ETAgent.T.init(prntP, NEW(ChildT, cl := cl));
      (* does the ClearShortCircuit, and VBT.Mark *)
      SetCursor(prntP, v.getcursor());
      IF v.parent = NIL THEN v.parent := prntP.ch END;
      IF cl.joinST.addScreen(prntP.st) THEN
        cl.needsRescreen := TRUE;
        mark := TRUE
      END
    END;
    IF v.st = NIL THEN
      LOCK v DO cl.needsRescreen := FALSE END;
      VBTClass.Rescreen(v, ST(v))
    ELSIF mark THEN
      VBTRep.Mark(v)
    END;
    RETURN prntP;
  END Be;

PROCEDURE New (v: JoinedVBT.T): T =
  BEGIN
    RETURN NEW(T).init(v);
  END New;

PROCEDURE Rem(prntP: T) =
  VAR
    cl := prntP.cl;
    pl: T;
    v: JoinedVBT.T;
  BEGIN                          (* LL = VBT.mu *)
    IF cl = NIL THEN RETURN END;
    v := cl.child;
    pl := v.parents;
    LOCK v DO
      (* delete prntP from list of parents *)
      IF pl = prntP THEN
        v.parents := pl.link;
      ELSE
        WHILE pl # NIL AND pl.link # prntP DO pl := pl.link END;
        IF pl = NIL THEN RETURN END; (* prnt not a parent of v"*)
        pl.link := prntP.link;
      END;
      IF prntP = cl.current THEN cl.current := NIL END
    END;

    prntP.cl := NIL;
    prntP.link := NIL;
    IF cl.joinST.removeScreen(prntP.oldst) THEN
      LOCK v DO cl.needsRescreen := TRUE END;
      VBT.Mark(v)
    END;
    IF v.parent = prntP.ch THEN SetInput(v, v.parents) END
  END Rem;

PROCEDURE Child (prnt: T): JoinedVBT.T =
  BEGIN
    IF prnt.cl = NIL THEN RETURN NIL ELSE RETURN prnt.cl.child END
  END Child;

PROCEDURE Succ (v: JoinedVBT.T; prntP: T): T =
  BEGIN
    LOCK v DO
      IF prntP = NIL THEN RETURN v.parents ELSE RETURN prntP.link END
    END
  END Succ;

PROCEDURE Discard (prntP: T) =
  BEGIN
    IF prntP.cl # NIL THEN Rem(prntP) END
  END Discard;
  
PROCEDURE Repaint (v: T; READONLY br: Region.T) =
  VAR
    cl              := v.cl;
    ch: JoinedVBT.T;
  BEGIN
    IF cl = NIL THEN RETURN END;
    ch := cl.child;
    IF ch.parents = v AND v.link = NIL THEN
      VBTClass.Repaint(ch, br)
    ELSIF ch.ch # NIL THEN
      LOCK ch.ch DO
        LOCK ch DO VBTClass.ForceRepaint(ch.ch, br, FALSE) END
      END;
      VBT.Mark(ch)
    END
  END Repaint;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  VAR
    cl                  := v.cl;
    ch    : JoinedVBT.T;
    m1, m2: BOOLEAN;
    st: JoinScreen.T;
  BEGIN
    v.trsl := Trestle.ScreenOf(v, Point.Origin).trsl;
    IF cl = NIL THEN v.oldst := cd.st; RETURN END;
    st := cl.joinST;
    m1 := st.removeScreen(v.oldst);
    v.oldst := cd.st;
    m2 := st.addScreen(cd.st);
    ch := cl.child;
    IF m1 OR m2 THEN
      IF ch.parents = v AND v.link = NIL THEN
        VBTClass.Rescreen(v.ch, cd.st);
        LOCK ch DO cl.needsRescreen := FALSE END;
        IF cd.st # NIL THEN
          VBTClass.Rescreen(ch, cd.st)
        ELSE
          VBTClass.Rescreen(ch, cl.joinST)
        END
      ELSE
        LOCK ch DO cl.needsRescreen := TRUE END;
        VBT.Mark(ch)
      END
    END
  END Rescreen;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    cl              := v.cl;
    ch: JoinedVBT.T;
  BEGIN
    IF cl = NIL THEN RETURN END;
    ch := cl.child;
    Public.reshape(v, cd);
    IF ch.parents = v AND v.link = NIL THEN
      VBTClass.Reshape(ch, cd.new, cd.saved)
    ELSIF ch.ch # NIL THEN
      LOCK ch.ch DO
        LOCK ch DO
          VBTClass.ForceRepaint(ch.ch, Region.Difference(
                                         Region.FromRect(cd.new),
                                         Region.FromRect(cd.saved)), FALSE)
        END
      END;
      VBT.Mark(ch)
    END
  END Reshape;

PROCEDURE Misc (v: T; READONLY cd: VBT.MiscRec) =
  VAR
    cl              := v.cl;
    ch: JoinedVBT.T;
    curParent: T := NIL;
  BEGIN
    IF cl = NIL THEN RETURN END;
    ch := cl.child;
    IF ch.parent # NIL THEN curParent := ch.parent.parent END;
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      Rem(v);
      IF ch.parents # NIL THEN RETURN END
    END;
    IF cd.type = VBT.TakeSelection THEN
      IF curParent = NIL OR curParent.trsl = v.trsl THEN
        SetInput(ch, v)
      END;
      IF curParent.trsl = v.trsl THEN VBTClass.Misc(ch, cd) END
    ELSIF cd.selection = VBT.NilSel THEN
      VBTClass.Misc(ch, cd)
    ELSE
      Public.misc(v, cd)
    END
  END Misc;

BEGIN
END JoinParent.
