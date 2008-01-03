(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 13:21:44 PST 1995 by kalsow *)
(*      modified on Wed Jun 23 15:10:49 PDT 1993 by steveg *)
(*      modified on Mon Feb 15 21:25:01 PST 1993 by johnh  *)
(*      modified on Tue Jul 21 17:46:38 PDT 1992 by sclafani *)
(*      modified on Tue May 12 10:28:45 1992 by mhb    *)

MODULE Zeus EXPORTS Zeus, ZeusPrivate;
<* PRAGMA LL *>

IMPORT Algorithm, AlgorithmClass, Animate, RefList, RefListUtils, Thread,
       VBT, View, ViewClass, ZeusClass, ZeusCodeView;
(* IMPORT FormsVBT, List, MethodList, Rsrc, Text, Thread, Trestle, VBT;*)

REVEAL
  Session = PrivateSession BRANDED OBJECT
              rw       : INTEGER            := 0;
              m        : MUTEX;
              c        : Thread.Condition;
              initiator: ZeusClass.T        := NIL;
              (* who initiated current Edit, Notify, or Broadcast *)
              locked: BOOLEAN := FALSE; (* "is editing lock held?" *)
              lockedBy : ZeusClass.T;  (* valid only when locked = TRUE *)
              lockedMsg: TEXT;         (* valid only when locked = TRUE *)
              evtMu    : MUTEX;        (* for event dispatching LL > VBT.mu *)
              evtCond  : Thread.Condition;
              evtViewCt: CARDINAL           := 0;
            OVERRIDES
              init := InitDefault;
              pre  := PreDefault;
              post := PostDefault;
            END;

TYPE 
  Prop = REF RECORD zeus: Session END;


PROCEDURE InitDefault(zeus: Session): Session =
  BEGIN (* LL = arbitrary *)
    zeus.rw := 0;
    zeus.m := NEW(MUTEX);
    zeus.c := NEW(Thread.Condition);
    zeus.evtMu := NEW(MUTEX);
    zeus.evtCond := NEW(Thread.Condition);
    RETURN zeus
  END InitDefault;

PROCEDURE PreDefault (<*UNUSED*> zeus     : Session;
                      <*UNUSED*> initiator: ZeusClass.T;
                      <*UNUSED*> style    : EventStyle;
                      <*UNUSED*> priority : INTEGER;
                      <*UNUSED*> t        : TEXT         ) =
  BEGIN (* LL = arbitrary *)
  END PreDefault;

PROCEDURE PostDefault (<*UNUSED*> zeus     : Session;
                       <*UNUSED*> initiator: ZeusClass.T;
                       <*UNUSED*> style    : EventStyle;
                       <*UNUSED*> priority : INTEGER;
                       <*UNUSED*> t        : TEXT         ) =
  BEGIN (* LL = arbitrary *)
  END PostDefault;

PROCEDURE AttachAlg (zeus: Session; alg: Algorithm.T) =
  BEGIN                         (* LL = VBT.mu *)
    AcquireExclusive(zeus);
    zeus.alg := alg;
    Mark(zeus, alg);
    ReleaseExclusive(zeus);
  END AttachAlg;

PROCEDURE AttachView (zeus: Session; view: View.T) =
  BEGIN                         (* LL = VBT.mu *)
    AcquireExclusive(zeus);
    IF NOT RefList.Member(zeus.views, view) THEN
      RefListUtils.Push(zeus.views, view);
      Mark(zeus, view);
      LOCK zeus.evtMu DO
        view.evtArg := NEW(ViewEvtRec, zeus := zeus);
        view.evtHandler := Thread.Fork(NEW(ViewClosure, view := view));
        WakeView(zeus, view);
        Thread.Wait(zeus.evtMu, zeus.evtCond);
      END;
      Configure(zeus, ZeusClass.StateChange.ViewAttached, view);
    END;
    ReleaseExclusive(zeus);
  END AttachView;

PROCEDURE DetachView (view: View.T) =
  VAR zeus := Resolve(view);
  BEGIN                         (* LL = VBT.mu *)
    IF zeus = NIL THEN RETURN  END;
    AcquireExclusive (zeus);
    IF RefList.Member (zeus.views, view) THEN
      IF view.evtHandler # NIL THEN
        view.evtHQuit := TRUE;
        Thread.Alert(view.evtHandler)
      END;
      RefListUtils.Delete (zeus.views, view);
      VBT.RemProp (view, TYPECODE (Prop));
      Configure (zeus, ZeusClass.StateChange.ViewDetached, view);
    END;
    ReleaseExclusive (zeus);
  END DetachView;

(*    (* Not converted to M3 *)
PROCEDURE Destroy(zeus: Session) RAISES {};
  VAR
    cl:   Private;
    rest: RefList.T;
    view: ZeusClass.T;
  BEGIN
    cl := zeus^.private;
    AcquireExclusive(cl);
    rest := zeus^.views;
    WHILE rest # NIL DO
      view := NARROW(rest^.first, ZeusClass.T);
      MethodList.Clear(view, TYPECODE(Session));
      rest := rest^.tail;
    END;
    MethodList.Clear(alg, TYPECODE(Session));
    zeus^.views := NIL;
    zeus^.alg   := NIL;
    ReleaseExclusive(cl);
  END Destroy;
*)

PROCEDURE Initiator (zeus: Session): ZeusClass.T=
  BEGIN                         (* LL = VBT.mu *)
    RETURN zeus.initiator;
  END Initiator;

PROCEDURE Mark (zeus: Session; v: ZeusClass.T) =
  BEGIN                         (* LL = VBT.mu *)
    WITH prop = NEW(Prop) DO
      prop.zeus := zeus;
      VBT.PutProp(v, prop);
    END
  END Mark;

PROCEDURE Resolve (v: ZeusClass.T): Session =
  BEGIN                         (* LL = VBT.mu *)
    WITH prop = NARROW(VBT.GetProp(v, TYPECODE(Prop)), Prop) DO
      IF prop = NIL THEN RETURN NIL ELSE RETURN prop.zeus END
    END
  END Resolve;


PROCEDURE AlertViews (zeus: Session) =
  <* LL = arbitrary *>
  (* Send an alert to any view of this zeus session that has registered
     itself as alertable. *)
  VAR rest: RefList.T;
  BEGIN
    AcquireShared(zeus);
    rest := zeus.views;
    WHILE rest # NIL DO
      WITH v = NARROW(RefListUtils.Pop(rest), View.T) DO
        IF v.alertable THEN Thread.Alert(v.evtHandler) END;
      END;
    END;
    ReleaseShared(zeus);
  END AlertViews;
    

(* **** Synchronizing Editing Actions **** *)

PROCEDURE Lock (zeus: Session; view: View.T; msg: TEXT): BOOLEAN =
  BEGIN                         (* LL = VBT.mu *)
    IF zeus.locked THEN
      RETURN FALSE;
    ELSE
      zeus.locked := TRUE;
      zeus.lockedBy := view;
      zeus.lockedMsg := msg;
      Configure(zeus, ZeusClass.StateChange.LockedBy, view);
      RETURN TRUE;
    END;
  END Lock;

PROCEDURE Unlock (zeus: Session; view: View.T): BOOLEAN =
  BEGIN                         (* LL = VBT.mu *)
    IF (NOT zeus.locked) OR (zeus.lockedBy # view) THEN
      RETURN FALSE;
    ELSE
      zeus.locked := FALSE;
      Configure(zeus, ZeusClass.StateChange.UnlockedBy, view);
      RETURN TRUE;
    END;
  END Unlock;

PROCEDURE LockInfo(zeus: Session; VAR view: View.T; VAR msg: TEXT): BOOLEAN =
  BEGIN                         (* LL = VBT.mu *)
      IF  NOT zeus.locked THEN
        RETURN FALSE;
      ELSE
        view := zeus.lockedBy;
        msg := zeus.lockedMsg;
        RETURN TRUE;
      END;
  END LockInfo;

PROCEDURE IsLocked (zeus: Session): BOOLEAN =
  BEGIN                         (* LL = VBT.mu *)
    RETURN zeus.locked
  END IsLocked;

PROCEDURE CheckLock (zeus: Session; <*UNUSED*> initiator: ZeusClass.T)
  RAISES {Locked} =
  BEGIN                         (* LL = VBT.mu *)
    IF zeus.locked AND (zeus.lockedBy # zeus.initiator) THEN
      RAISE Locked("View is read-only -- " & zeus.lockedMsg);
    END;
  END CheckLock;


(* **** Dispatching Events **** *)

PROCEDURE Dispatch (initiator   : ZeusClass.T;
                    style       : EventStyle;
                    priority    : INTEGER;
                    eventName   : TEXT;
                    dispatchProc: DispatchProc;
                    evtArgs     : REFANY        )
  RAISES {Error, Locked, Thread.Alerted} =
  (* If style = EventStyle.Broadcast, EventStyle.Output, or
     EventStyle.Code, then LL = {}.  Otherwise LL = VBT.mu *)
  VAR zeus := Resolve(initiator);
  BEGIN
    IF style # EventStyle.Broadcast THEN
      zeus.pre(initiator, style, priority, eventName)
    END;
    Animate.ResetATime();
    TRY
      CASE style OF
      | EventStyle.Output, EventStyle.Update =>
          AlgToViews(zeus, initiator, dispatchProc, evtArgs);

      | EventStyle.Edit, EventStyle.Notify =>
          IF style = EventStyle.Edit THEN CheckLock(zeus, initiator) END;
          ViewToAlg(zeus, initiator, dispatchProc, evtArgs);

      | EventStyle.Broadcast =>
          ViewToAlg(zeus, initiator, dispatchProc, evtArgs);
          AlgToViews(zeus, initiator, dispatchProc, evtArgs);

      | EventStyle.Code =>
          AlgToCodeViews(zeus, initiator, dispatchProc, evtArgs);

      END;
    FINALLY
      IF (style # EventStyle.Broadcast) THEN
        zeus.post(initiator, style, priority, eventName)
      END;
    END;
  END Dispatch;


TYPE
  ViewClosure = Thread.Closure OBJECT
                  view: View.T;
                OVERRIDES
                  apply := ViewThread;
                END;

  ViewEvtRec = REF RECORD
                     zeus  : Session;
                     proc  : DispatchProc;
                     args  : REFANY;
                     errVal: REFANY;
                   END;


PROCEDURE AlgToViews (zeus        : Session;
                      initiator   : ZeusClass.T;
                      dispatchProc: DispatchProc;
                      evtArgs     : REFANY        ) RAISES {Error} =
  <* LL <= VBT.mu *>
  VAR
    rest, rest2: RefList.T;
    myview     : View.T;
    errorVal   : REFANY;
    ct                  := 0;
  BEGIN
    AcquireShared(zeus);         (* is this needed?  something stronger? *)
    rest := zeus.views;
    rest2 := rest;
    ReleaseShared(zeus);
    LOCK zeus.evtMu DO
      zeus.evtWasHandled := FALSE;
      zeus.evtViewCt := 0;
      WHILE rest # NIL DO
        myview := NARROW(RefListUtils.Pop(rest), View.T);
        IF myview.isCompat(initiator) THEN
          INC(ct);
          myview.evtHandled := TRUE;
          WITH rec = NARROW(myview.evtArg, ViewEvtRec) DO
            rec.proc := dispatchProc;
            rec.args := evtArgs;
          END;
          WakeView(zeus, myview);
        END;
      END;
      rest := rest2;
      IF ct # 0 THEN Thread.Wait(zeus.evtMu, zeus.evtCond); END;
      WHILE rest # NIL DO
        myview := NARROW(RefListUtils.Pop(rest), View.T);
        IF myview.isCompat(initiator) THEN
          IF myview.evtHandled THEN zeus.evtWasHandled := TRUE END;
          WITH rec = NARROW(myview.evtArg, ViewEvtRec) DO
            IF (rec.errVal # NIL) AND (errorVal = NIL) THEN
              errorVal := rec.errVal;
            END;
          END;
        END;
      END;
    END;
    IF errorVal # NIL THEN RAISE Error(errorVal); END;
  END AlgToViews;

PROCEDURE ViewThread (self: ViewClosure): REFANY =
  BEGIN
    WITH v   = self.view,
         rec = NARROW(v.evtArg, ViewEvtRec) DO
      TRY
        WHILE TRUE DO
          WakeZeusAndSleep(rec.zeus, v);
          TRY
            rec.errVal := NIL;
            rec.proc(v, rec.args);
          EXCEPT
          | Thread.Alerted => IF v.evtHQuit THEN RETURN NIL END;
          END;
        END;
      EXCEPT
      | Thread.Alerted =>
        <* ASSERT v.evtHQuit *>
        (* WakeZeusAndSleep raises Alerted only when v.evtHQuit *)
      END;
    END;
    RETURN NIL;
  END ViewThread;

PROCEDURE WakeView (zeus: Session; view: View.T) =
  (* LL = {zeus.evtMu} *)
  BEGIN
    INC(zeus.evtViewCt);
    Thread.Signal(view.evtCond);
  END WakeView;

PROCEDURE WakeZeusAndSleep (zeus: Session; view: View.T)
  RAISES {Thread.Alerted} =
  (* LL = {} *)
  BEGIN
    LOCK zeus.evtMu DO
      DEC(zeus.evtViewCt);
      IF zeus.evtViewCt = 0 THEN Thread.Signal(zeus.evtCond) END;
      LOOP
        TRY
          Thread.AlertWait(zeus.evtMu, view.evtCond);
          RETURN;
        EXCEPT
          Thread.Alerted => IF view.evtHQuit THEN RAISE Thread.Alerted END;
        END;
      END;
    END;
  END WakeZeusAndSleep;


PROCEDURE ViewToAlg (zeus        : Session;
                     initiator   : ZeusClass.T;
                     dispatchProc: DispatchProc;
                     evtArgs     : REFANY        ) RAISES {Error} =
  (* LL <= VBT.mu *) (* ? *)
  VAR errorVal: REFANY;
  BEGIN
    AcquireShared(zeus);
    zeus.initiator := initiator;
    TRY
      TRY
        zeus.alg.evtHandled := TRUE;    (* default fe methods set it FALSE *)
        dispatchProc(zeus.alg, evtArgs);
        zeus.evtWasHandled := zeus.alg.evtHandled;
      EXCEPT
      | Thread.Alerted =>  (* shouldn't happen *)
      END;
    FINALLY
      zeus.initiator := NIL;
      ReleaseShared(zeus);
    END;
    IF errorVal # NIL THEN RAISE Error(errorVal); END;
  END ViewToAlg;

PROCEDURE AlgToCodeViews (           zeus        : Session;
                                     initiator   : ZeusClass.T;
                          <*UNUSED*> dispatchProc: DispatchProc;
                                     evtArgs     : REFANY        ) =
  VAR
    rest: RefList.T;
    arg          := NARROW(evtArgs, ZeusCodeView.Arg);
  BEGIN                          (* LL = {} *)
    AcquireShared(zeus);
    rest := zeus.views;
    ReleaseShared(zeus);
    zeus.evtWasHandled := FALSE;
    WHILE rest # NIL DO
      TYPECASE RefListUtils.Pop(rest) OF
      | ZeusCodeView.T (myview) =>
          IF myview.isCompat(initiator) THEN
            zeus.evtWasHandled := TRUE;
            LOCK VBT.mu DO
              myview.cv.event(arg.highlight, 0, arg.procedureName);
            END;
          END;
      ELSE
      END;
    END;
  END AlgToCodeViews;


(* **** Utilities **** *)

PROCEDURE Configure (zeus       : Session;
                     whatChanged: ZeusClass.StateChange;
                     instigator : ZeusClass.T            ) =
  <* LL = VBT.mu *>
  VAR
    rest: RefList.T;
    view: View.T;
  BEGIN
    zeus.alg.config(whatChanged, instigator);
    rest := zeus.views;
    WHILE rest # NIL DO
      view := NARROW(rest.head, View.T);
      view.config(whatChanged, instigator);
      rest := rest.tail;
    END;
  END Configure;


(* **** Reader/Writer **** *)

PROCEDURE Acquire (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    AcquireShared (zeus);
  END Acquire;

PROCEDURE Release (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    ReleaseShared (zeus);
  END Release;

(* The following implements a simple reader/writer scheme. 
   See SPwM3, p103. Alternatively, track down Andrew Birrell. *)

PROCEDURE AcquireExclusive (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    LOCK zeus.m DO
      WHILE zeus.rw # 0 DO Thread.Wait (zeus.m, zeus.c) END;
      zeus.rw := -1;
    END
  END AcquireExclusive;

PROCEDURE AcquireShared (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    LOCK zeus.m DO
      WHILE zeus.rw < 0 DO Thread.Wait (zeus.m, zeus.c) END;
      INC (zeus.rw)
    END
  END AcquireShared;

PROCEDURE ReleaseExclusive (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    LOCK zeus.m DO 
      zeus.rw := 0; 
      Thread.Broadcast (zeus.c) 
    END
  END ReleaseExclusive;

PROCEDURE ReleaseShared (zeus: Session) =
<* LL <= VBT.mu *>
  BEGIN
    LOCK zeus.m DO 
      DEC(zeus.rw); 
      IF zeus.rw = 0 THEN Thread.Signal (zeus.c) END 
    END
  END ReleaseShared; 


(* ****  Mainline  **** *)

BEGIN
  Thread.IncDefaultStackSize(10000);
  stdoutMu := NEW(MUTEX);
  stderrMu := NEW(MUTEX);
END Zeus.




