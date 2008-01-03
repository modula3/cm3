(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 31 13:27:14 PST 1995 by kalsow *)
(*      modified on Thu Nov 18 14:14:39 PST 1993 by mhb    *)
(*      modified on Wed Jun 23 15:10:01 PDT 1993 by steveg *)
(*      modified on Fri Feb 12 16:14:15 PST 1993 by johnh *)


MODULE View EXPORTS View, ViewClass;
<* PRAGMA LL *>

IMPORT Algorithm, Cursor, Fmt, RefList, RefListUtils, PaintOp, Point, Rd,
       ReactivityVBT, Rect, StableVBT, TextureVBT, Thread,
       Trestle, TrestleComm, VBT, ViewClass, Wr, ZeusClass,
       ZeusPanelFriends, ZeusUtil;

REVEAL
  T = ViewClass.TT BRANDED OBJECT
      OVERRIDES
        init       := DefaultInit;
        isCompat   := DefaultIsCompat;
        install    := DefaultInstall;
        delete     := DefaultDelete;
        snapshot   := DefaultSnapshot;
        restore    := DefaultRestore;
        startrun   := DefaultStartrun;
        endrun     := DefaultEndrun;
        reactivity := DefaultReactivity;
      END;

TYPE
  Waiter =
    Thread.Closure OBJECT v: T;  OVERRIDES apply := WaiterThread; END;

<*FATAL TrestleComm.Failure, Wr.Failure, Thread.Alerted *>

PROCEDURE DefaultInit (v: T; ch: VBT.T): T =
  <* LL = VBT.mu *>
  BEGIN
    v.evtCond := NEW(Thread.Condition);
    IF ch = NIL THEN ch := TextureVBT.New(PaintOp.Bg) END;
    EVAL ReactivityVBT.T.init(v, ch);
    RETURN v;
  END DefaultInit;

PROCEDURE Activate(v: T; on: BOOLEAN) =
  <* LL = VBT.mu *>
  BEGIN
    IF on THEN
      v.reactivity(FALSE);
    ELSE
      ReactivityVBT.Set(v, ReactivityVBT.State.Dormant, cursor)
    END;
  END Activate;

PROCEDURE DefaultIsCompat (<*UNUSED*> v: T; alg: ZeusClass.T): BOOLEAN =
  BEGIN
    RETURN ISTYPE(alg, Algorithm.T)
  END DefaultIsCompat;

PROCEDURE DefaultInstall (v: T) = 
<* LL = VBT.mu *>
  BEGIN
    v.reactivity(FALSE);
    Trestle.Attach (v);
    Trestle.Decorate (v, applName := "Zeus View", windowTitle := v.name);
    EVAL Thread.Fork(NEW(Waiter, v := v));
  END DefaultInstall;

PROCEDURE WaiterThread (waiter: Waiter): REFANY RAISES {} =
<* LL = {} *>
  BEGIN
    WITH v = waiter.v DO
      Trestle.AwaitDelete (v);
      LOCK VBT.mu DO
        ZeusPanelFriends.DetachView (v);
        VBT.Discard (v);
      END
    END;
    RETURN NIL
  END WaiterThread;


PROCEDURE DefaultDelete (v: T) = 
<* LL = VBT.mu *>
  BEGIN
    Trestle.Delete (v);
  END DefaultDelete;

PROCEDURE DefaultSnapshot (v: T; wr: Wr.T) =
<* LL = VBT.mu *>
  VAR
    dom := VBT.Domain(v);
    nw  := Trestle.ScreenOf(v, Rect.NorthWest(dom));
    se  := Trestle.ScreenOf(v, Rect.SouthEast(dom));
  BEGIN
    IF nw.id # Trestle.NoScreen THEN
      Wr.PutText(wr, "(ScreenPos " & Fmt.Int(nw.id) & " " & Fmt.Int(nw.q.h)
                       & " " & Fmt.Int(nw.q.v) & " " & Fmt.Int(se.q.h)
                       & " " & Fmt.Int(se.q.v) & ")\n");
    ELSE
      Wr.PutText(wr, "()\n");
    END;
  END DefaultSnapshot;

PROCEDURE DefaultRestore (v: T; rd: Rd.T)
  RAISES {ZeusClass.Error} =
  <* LL = VBT.mu *>
  VAR
    id    : Trestle.ScreenID;
    nw, se: Point.T;
    list  := ZeusUtil.RdToList(rd);
  PROCEDURE NarrowToInt (a: REFANY): INTEGER
    RAISES {ZeusClass.Error} =
    BEGIN
      TYPECASE a OF
      | REF INTEGER (ri) => RETURN ri^;
      ELSE
        RAISE ZeusClass.Error(
                "NARROW failed in View.DefaultRestore");
      END;
    END NarrowToInt;
  BEGIN
    IF list = NIL THEN
      Trestle.MoveNear(v, NIL);
    ELSE
      IF RefList.Length(list) # 6 THEN
        RAISE
          ZeusClass.Error("View.DefaultRestore: bad ScreenPos");
      END;
      TRY
        ZeusUtil.KeywordCheck(list, "ScreenPos")
      EXCEPT
        ZeusUtil.BadSnapshot (msg) =>
          RAISE ZeusClass.Error(
                  "View.DefaultRestore: bad ScreenPos: " & msg);
      END;
      EVAL RefListUtils.Pop(list);      (* first elem is ScreenPos *)
      id := NarrowToInt(RefListUtils.Pop(list));
      nw.h :=
        NarrowToInt(RefListUtils.Pop(list)) - ZeusPanelFriends.XDRIFT;
      nw.v :=
        NarrowToInt(RefListUtils.Pop(list)) - ZeusPanelFriends.YDRIFT;
      se.h :=
        NarrowToInt(RefListUtils.Pop(list)) - ZeusPanelFriends.XDRIFT;
      se.v :=
        NarrowToInt(RefListUtils.Pop(list)) - ZeusPanelFriends.YDRIFT;
      StableVBT.SetShape(v, ABS(se.h - nw.h), ABS(se.v - nw.v));
      IF ZeusUtil.ScreenPosOK(id, nw) THEN
        Trestle.Overlap(v, id, nw);
      ELSE
        Trestle.MoveNear(v, NIL);
      END;
    END;
  END DefaultRestore;

PROCEDURE DefaultStartrun (<*UNUSED*>v: T) = 
<* LL = {} *>
  BEGIN
    (* should the default method repaint the VBT with the bg color? *)
  END DefaultStartrun;

PROCEDURE DefaultEndrun (<*UNUSED*> v: T) = 
<* LL = {} *>
  BEGIN
  END DefaultEndrun;


PROCEDURE DefaultReactivity (v: T; on: BOOLEAN) =
  <* LL <= VBT.mu *>
  BEGIN
    (* The following test should not be necessary; call the reactivity
       method only when the view isCompat with the algorithm. *)
    (* IF ReactivityVBT.Get(v) # ReactivityVBT.State.Dormant THEN *)
    IF on THEN
      ReactivityVBT.Set(v, ReactivityVBT.State.Active, Cursor.DontCare);
    ELSE
      ReactivityVBT.Set(v, ReactivityVBT.State.Passive, cursor);
    END;
  END DefaultReactivity;

VAR cursor: Cursor.T;

BEGIN
 cursor := Cursor.FromName(ARRAY OF TEXT{"XC_iron_cross"});
END View.



