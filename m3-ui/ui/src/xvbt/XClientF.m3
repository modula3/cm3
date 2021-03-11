(* Copyright C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Aug  2 11:12:59 PDT 1996 by msm      *)
(*      modified on Fri Feb 18 16:17:16 PST 1994 by kalsow   *)
(*      modified on Wed Nov 24 09:16:54 PST 1993 by steveg   *)
(*      modified on Fri Oct 29 16:22:55 PDT 1993 by sfreeman *)
(* modified on Fri May 7 16:53:30 PDT 1993 by mjordan *)
(* modified on Fri Jan 29 11:55:41 PST 1993 by jdd *)
(* modified on Mon Feb 24 13:59:43 PST 1992 by muller *)
(* modified on Sat Jan 11 19:03:47 PST 1992 by gnelson*)


<*PRAGMA LL*>

(* Partitioning following the efforts of
   Steve.Freeman@computer-lab.cambridge.ac.uk - 92-05-13 *)

UNSAFE MODULE XClientF;

IMPORT XClient, TrestleOnX, TrestleClass, Trestle, Rect, ProperSplit,
       IntRefTbl, IntTextTbl, TextIntTbl, X, XEventQueue, Thread,
       XAtomQueue, XScreenType, VBT, Ctypes, TrestleComm, Fmt, XProperties,
       RTParams, KeyboardKey, RTHeapRep,
       VBTClass, Env, M3toC, XInput, XMessenger, Split, Text,
       IP, ASCII, XExtensions, IntIntTbl, XClientExt, TrslOnXF;

FROM XClient IMPORT T;

REVEAL
  SimpleWaitFor = SimpleWaitForPublic BRANDED OBJECT
                  OVERRIDES
                    match  := SimpleMatch;
                    notify := SimpleNotify;
                  END;

  T_Abs = T_Ext BRANDED OBJECT
            await: WaitFor := NIL; (* list of awaited events *)
            awaitCount := ARRAY [0 .. X.LASTEvent - 1] OF INTEGER{0, ..};
            (* awaitCount[i] is the number of awaited events that might
               match an event of type i. *)
            awaitCountExt: IntIntTbl.T := NIL;
            (* X extensions use event types > X.LastEvent, but we don't
               know what values, so keep any extension values in a table *)
            coverage : CARDINAL := 0;
            atomQ               := XAtomQueue.Empty;
            atomCount           := 0;
            (* atomQ contains atoms that are available for transferring
               selections; atomCount is the number of atoms that have been
               created solely for this purpose. *)
            meterMaid: Thread.T := NIL;
            gcCursor : X.Cursor := X.None;
          END;

PROCEDURE SimpleMatch (wf: SimpleWaitFor; READONLY ev: X.XEvent): BOOLEAN =
  VAR match: BOOLEAN;
  BEGIN
    WITH e    = LOOPHOLE(ADR(ev), X.XAnyEventStar),
         type = e.type                              DO
      IF type = 0 THEN
        match := LOOPHOLE(ADR(ev), X.XErrorEventStar).serial = wf.reqno
      ELSE
        match := e.window = wf.d
      END;
      IF match THEN
        FOR i := FIRST(wf.types) TO LAST(wf.types) DO
          IF wf.types[i] = type THEN RETURN TRUE END
        END
      END;
      RETURN FALSE
    END
  END SimpleMatch;

PROCEDURE SimpleNotify (         wf   : SimpleWaitFor;
                        READONLY evRec: X.XEvent;
                                 xcon : XClient.T      ) =
  BEGIN
    wf.turn := TRUE;
    wf.ev := evRec;
    wf.timeout := FALSE;
    Thread.Signal(wf);
    WHILE wf.turn AND NOT xcon.dead DO Thread.Wait(xcon, wf); END;
  END SimpleNotify;

PROCEDURE StartMeterMaid (trsl: T; stackSize := 20000) =
  BEGIN
    EVAL Thread.Fork(
           NEW(MeterMaidClosure, trsl := trsl, stackSize := stackSize));
  END StartMeterMaid;

TYPE
  MeterMaidClosure = Thread.SizedClosure OBJECT
                       trsl: XClient.T
                     OVERRIDES
                       apply := MeterMaid
                     END;

PROCEDURE MeterMaid (cl: MeterMaidClosure): REFANY RAISES {} =
  VAR prev, wf: WaitFor;
  BEGIN
    WITH trsl = cl.trsl DO
      LOOP
        Thread.Pause(1.0D0);
        LOCK trsl DO
          prev := NIL;
          wf := trsl.await;
          WHILE wf # NIL DO
            IF wf.timelimit = 0 OR trsl.dead THEN
              DeleteWait(trsl, prev, wf);
              wf.turn := TRUE;
              wf.timeout := TRUE;
              Thread.Signal(wf);
              wf := prev
            ELSIF wf.timelimit > 0 THEN
              DEC(wf.timelimit)
            END;
            IF wf = NIL THEN
              wf := trsl.await
            ELSE
              prev := wf;
              wf := wf.next
            END;
          END;
          IF trsl.await = NIL THEN trsl.meterMaid := NIL; RETURN NIL END
        END
      END
    END
  END MeterMaid;

PROCEDURE Kill (trsl: T) <* LL.sup = trsl *> =
  BEGIN
    LOCK TrestleClass.closeMu DO
      IF NOT trsl.closed THEN trsl.closed := TRUE; END
    END;
    trsl.dead := TRUE;
    Thread.Broadcast(trsl.qEmpty);
    Thread.Broadcast(trsl.qNonEmpty);
    Thread.Broadcast(trsl.evc);
    IF trsl.meterMaid = NIL AND trsl.await # NIL THEN
      StartMeterMaid(trsl)
    END;
    EVAL Thread.Fork(NEW(KillClosure, trsl := trsl))
  END Kill;

TYPE
  KillClosure =
    Thread.Closure OBJECT trsl: T OVERRIDES apply := DoKill END;

PROCEDURE DoKill (self: KillClosure): REFANY RAISES {} =
  BEGIN
    LOCK self.trsl DO
      TRY
        X.XCloseDisplay(self.trsl.dpy)
      EXCEPT
        X.Error =>   (* skip *)
      END
    END;
    Thread.Pause(60.0D0);
    LOCK errMu DO
      FOR i := 0 TO LAST(dpyTable^) DO
        IF dpyTable[i].trsl = self.trsl THEN dpyTable[i].trsl := NIL END
      END
    END;
    RETURN NIL
  END DoKill;

PROCEDURE Await (trsl: T_Abs; wf: WaitFor; timelimit: INTEGER := -1):
  INTEGER RAISES {TrestleComm.Failure} =
  (* LL = trsl *)
  BEGIN
    IF trsl.dead THEN RETURN Timeout END;

    RegisterWaiter(trsl, wf);
    RETURN WaitWaiter(trsl, wf, timelimit);
  END Await;

PROCEDURE DeleteWait (trsl: T; prev, wf: WaitFor) =
  VAR
    count: INTEGER;
    type : X.Int;
  BEGIN
    IF prev = NIL THEN trsl.await := wf.next ELSE prev.next := wf.next END;
    wf.next := NIL;
    FOR i := FIRST(wf.types) TO LAST(wf.types) DO
      type := wf.types[i];
      WITH tbl = trsl.awaitCountExt DO
        CASE type OF
        | -1 =>                  (* skip *)
        | 0 .. X.LASTEvent - 1 => DEC(trsl.awaitCount[type]);
        ELSE
          IF tbl # NIL AND tbl.get(type, count) THEN
            DEC(count);
            IF count = 0 THEN
              EVAL tbl.delete(type, count);
              IF tbl.size() = 0 THEN trsl.awaitCountExt := NIL; END;
            ELSE
              EVAL tbl.put(type, count);
            END;
          END;
        END;
      END;
    END
  END DeleteWait;

PROCEDURE FindWaiter (trsl: T; READONLY ev: X.XEvent): WaitFor =
  (* LL = trsl *)
  VAR res, prev: WaitFor;
      count: INTEGER;
  BEGIN
    WITH e = LOOPHOLE(ADR(ev), X.XAnyEventStar) DO
      CASE e.type OF
      |  0..X.LASTEvent - 1 =>
        IF trsl.awaitCount[e.type] = 0 THEN
          RETURN NIL;
        END;
      ELSE
        IF trsl.awaitCountExt = NIL OR
          NOT trsl.awaitCountExt.get(e.type, count) OR
          count = 0 THEN
          RETURN NIL;
        END;
      END;
      prev := NIL;
      res := trsl.await;
      WHILE (res # NIL) AND NOT res.match(ev) DO
        prev := res;
        res := res.next
      END;
      IF res # NIL THEN DeleteWait(trsl, prev, res) END;
      RETURN res
    END
  END FindWaiter;

PROCEDURE RegisterWaiter (trsl: T_Abs; wf: WaitFor) =
  VAR
    count: INTEGER;
    type : X.Int;
  BEGIN
    FOR i := FIRST(wf.types) TO LAST(wf.types) DO
      type := wf.types[i];
      CASE type OF
      | -1 =>                    (* skip *)
      | 0 .. X.LASTEvent - 1 => INC(trsl.awaitCount[type]);
      ELSE
        IF trsl.awaitCountExt = NIL THEN
          trsl.awaitCountExt := NEW(IntIntTbl.Default).init(1);
        END;
        IF trsl.awaitCountExt.get(type, count) THEN
          INC(count);
        ELSE
          count := 1;
        END;
        EVAL trsl.awaitCountExt.put(type, count);
      END;
    END;
    wf.next := trsl.await;
    trsl.await := wf;
  END RegisterWaiter;

PROCEDURE WaitWaiter (trsl: T_Abs; wf: WaitFor; timelimit: INTEGER := -1):
  INTEGER RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    IF trsl.dead THEN RETURN Timeout END;
    wf.timelimit := timelimit;

    IF trsl.meterMaid = NIL THEN
      trsl.meterMaid := Thread.Fork(NEW(MeterMaidClosure, trsl := trsl,
                                        stackSize := 20000))
    END;
    X.XFlush(trsl.dpy);
    IF X.XEventsQueued(trsl.dpy, X.QueuedAfterReading) # 0 THEN
      Thread.Signal(trsl.qNonEmpty)
    END;
    WHILE NOT wf.turn DO Thread.Wait(trsl, wf) END;
    wf.turn := FALSE;
    Thread.Signal(wf);
    IF wf.timeout THEN RETURN Timeout; END;
    WITH e = LOOPHOLE(ADR(wf.ev), X.XAnyEventStar) DO RETURN e.type END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END WaitWaiter;

(* ---------- various utilities ---------- *)

PROCEDURE ToRect (x, y, width, height: INTEGER): Rect.T =
  BEGIN
    RETURN
      Rect.T{west := x, north := y, east := x + width, south := y + height}
  END ToRect;

PROCEDURE NewAtom (v: T): X.Atom RAISES {TrestleComm.Failure} =
  <*FATAL XAtomQueue.Exhausted*>
  BEGIN
    IF XAtomQueue.IsEmpty(v.atomQ) THEN
      INC(v.atomCount);
      RETURN
        XClient.ToAtom(v, "_DEC_TRESTLE_NEWATOM_" & Fmt.Int(v.atomCount))
    END;
    RETURN XAtomQueue.Remove(v.atomQ)
  END NewAtom;

PROCEDURE FreeAtom (v: T; VAR sym: X.Atom) =
  BEGIN
    IF sym # X.None THEN XAtomQueue.Insert(v.atomQ, sym); sym := X.None END
  END FreeAtom;

PROCEDURE BackDoor (v: T; READONLY ev: X.XEvent) =
  BEGIN
    XEventQueue.Insert(v.errq, ev);
    Thread.Signal(v.qNonEmpty)
  END BackDoor;

PROCEDURE SetUngrabs (trsl: T) RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    FOR i := FIRST(Ungrab) TO LAST(Ungrab) DO
      trsl.ungrab[i] := X.XKeysymToKeycode(trsl.dpy, Ungrab[i])
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    (* for all vbts, fix the grabs they have by ungrabbing all, and
       regrabbing what we want -- someday. *)
  END SetUngrabs;

PROCEDURE ValidateNW (trsl: T; ch: Child; st: XScreenType.T)
  RAISES {TrestleComm.Failure} =
  VAR chw: X.Window;
      h, v: Ctypes.int;
  BEGIN
    TRY
    IF NOT ch.nwValid THEN
      ch.nwValid := X.XTranslateCoordinates(
                      trsl.dpy, ch.w, st.root, 0, 0, ADR(h),
                      ADR(v), ADR(chw)) # X.False;
      ch.nw.v := v;
      ch.nw.h := h
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ValidateNW;

PROCEDURE GetDomain (ur: Child; VAR (*OUT*) width, height: CARDINAL) =
  (* Return the domain of ur's X window, or 0,0 when the window is
     unmapped, and clear ur.reshapeComing.  LL = ur.ch.parent *)
  BEGIN
    IF ur.mapped THEN
      width := ur.width;
      height := ur.height
    ELSE
      width := 0;
      height := 0
    END;
    ur.reshapeComing := FALSE
  END GetDomain;

PROCEDURE AdjustCoverage (xcon: T; d: [-1 .. 1] := 0)
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    INC(xcon.coverage, d);
    IF xcon.coverage = 0 THEN X.XFlush(xcon.dpy) END;
    IF X.XEventsQueued(xcon.dpy, X.QueuedAfterReading) # 0 THEN
      Thread.Signal(xcon.qNonEmpty)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END AdjustCoverage;

PROCEDURE Delete (trsl: XClient.T; ch: VBT.T; ur: Child) RAISES {} =
  VAR
    junk: REFANY;
    code         := VBT.Deleted;
  BEGIN
    IF ur = NIL THEN RETURN END;
    LOCK trsl DO
      EVAL trsl.vbts.delete(ur.w, junk);
      FOR s := FIRST(trsl.sel^) TO LAST(trsl.sel^) DO
        IF trsl.sel[s].v = ch THEN trsl.sel[s].v := NIL END
      END;
      IF trsl.dead THEN code := VBT.Disconnected END;
      ur.xcage := X.None
    END;
    ProperSplit.Delete(trsl, ur);
    VBTClass.Misc(ch, VBT.MiscRec{code, VBT.NullDetail, 0, VBT.NilSel});
    VBT.Discard(ch)
  END Delete;

PROCEDURE Reshape (ch: VBT.T; width, height: CARDINAL; sendMoved := FALSE) =
  (* Reshape ch to new width and height.  If this is a no-op, but sendMoved
     is true, then send a miscellaneous code.  LL = VBT.mu *)
  BEGIN
    IF (ch.domain.east # width) OR (ch.domain.south # height) THEN
      WITH new = Rect.FromSize(width, height) DO
        VBTClass.Reshape(ch, new, Rect.Meet(ch.domain, new))
      END
    ELSIF sendMoved THEN
      VBTClass.Misc(
        ch, VBT.MiscRec{VBT.Moved, VBT.NullDetail, 0, VBT.NilSel})
    END
  END Reshape;

(* ---------- connection management ---------- *)

TYPE
  DpyTable = REF ARRAY OF
                   RECORD
                     dpy : X.DisplayStar;
                     trsl: T
                   END;

VAR
  errMu := NEW(MUTEX);           (* LL > any VBT. *)
  (* protection = errMu *)
  dpyTable, hackDpyTable: DpyTable := NIL;
(* maps dpys and hack dpys to their corresponding Ts. *)

VAR
  openMu    := NEW(MUTEX);       (* LL maximal *)
  opening   := FALSE;
  firstTime := TRUE;

PROCEDURE Connect (inst: TEXT; trsl: T := NIL): Trestle.T
  RAISES {TrestleComm.Failure} =
  VAR
    dpy, hackdpy           : X.DisplayStar := NIL;
    cpos, dpos             : INTEGER;
    machine, rest, fullinst: TEXT;
  BEGIN
    TRY
      IF inst = NIL AND Env.Get("ARGOENABLED") # NIL THEN
        inst := Env.Get("TRUE_DISPLAY");
      END;
      IF inst = NIL THEN inst := Env.Get("DISPLAY"); END;
      IF inst = NIL THEN inst := ":0" END;
      cpos := Text.FindChar(inst, ':');
      dpos := Text.FindCharR(inst, '.');
      IF cpos >= 0 AND Text.Length(inst) > cpos + 1
           AND Text.GetChar(inst, cpos + 1) IN ASCII.Digits THEN
        TRY
          IF cpos = 0 THEN
            machine := IP.GetCanonicalByAddr(IP.GetHostAddr());
          ELSE
            machine := Text.Sub(inst, 0, cpos);
            rest := IP.GetCanonicalByName(machine);
            IF rest # NIL THEN machine := rest END
          END;
          IF machine = NIL THEN
            machine := "localhost";
          END;
        EXCEPT
          IP.Error =>
        END;
        IF dpos <= cpos THEN
          rest := Text.Sub(inst, cpos)
        ELSE
          rest := Text.Sub(inst, cpos, dpos - cpos)
        END;
        IF machine = NIL THEN
          machine := "localhost";
        END;
        fullinst := machine & rest;
      ELSE
        fullinst := inst
      END;
      WITH s = M3toC.SharedTtoS(inst) DO
        TRY
          LOCK openMu DO
            IF firstTime THEN
              TrslOnXF.Init();
              firstTime := FALSE;
              EVAL Thread.Fork(NEW(InitClosure))
            END;
            opening := TRUE
          END;
          X.XInitThreads();
          dpy := X.XOpenDisplay(s);
          IF doHack THEN
            TRY
              hackdpy := X.XOpenDisplay(s)
            EXCEPT
              X.Error => hackdpy := NIL
            END
          END
        FINALLY
          M3toC.FreeSharedS(inst, s);
          LOCK openMu DO opening := FALSE END;
        END
      END;
      IF dpy = NIL THEN
        IF hackdpy = NIL THEN
          RAISE TrestleComm.Failure
        ELSE
          dpy := hackdpy;
          hackdpy := NIL
        END
      END;
      IF trsl = NIL THEN trsl := NEW(T) END;
      trsl.dpy := dpy;
      IF trsl.st = NIL THEN trsl.st := NEW(VBT.ScreenType) END;
      trsl.inst := inst;
      trsl.fullinst := fullinst;
      (* The st is irrelevant except that it must be non-NIL so that
         marking the trsl for redisplay is not a noop. *)
      trsl.gcCursor := X.None;
      TrestleOnX.Enter(trsl);
      TRY
        LOCK errMu DO
          WITH table = dpyTable,
               hack  = hackDpyTable DO
            IF table = NIL THEN
              table := NEW(DpyTable, 1);
              IF doHack THEN hack := NEW(DpyTable, 1) END
            ELSE
              WITH new = NEW(DpyTable, NUMBER(table^) + 1) DO
                FOR i := 0 TO LAST(table^) DO new[i + 1] := table[i] END;
                table := new
              END;
              IF doHack AND hackdpy # NIL THEN
                WITH new = NEW(DpyTable, NUMBER(hack^) + 1) DO
                  FOR i := 0 TO LAST(hack^) DO new[i + 1] := hack[i] END;
                  hack := new
                END
              END
            END;
            table[0].trsl := trsl;
            table[0].dpy := trsl.dpy;
            IF doHack AND hackdpy # NIL THEN
              hack[0].trsl := trsl;
              hack[0].dpy := hackdpy
            END
          END
        END;
        trsl.sel := NEW(SelArray, 0);
        trsl.vbts := NEW(IntRefTbl.Default).init();
        trsl.atoms := NEW(IntTextTbl.Default).init();
        trsl.names := NEW(TextIntTbl.Default).init();
        trsl.evc := NEW(Thread.Condition);
        trsl.qEmpty := NEW(Thread.Condition);
        trsl.qNonEmpty := NEW(Thread.Condition);
        trsl.defaultScreen := X.XDefaultScreen(trsl.dpy);
        trsl.screens :=
          NEW(REF ARRAY OF XScreenType.T, X.XScreenCount(trsl.dpy));
        trsl.takeFocus := XClient.ToAtom(trsl, "WM_TAKE_FOCUS");
        trsl.wmMoved := XClient.ToAtom(trsl, "WM_MOVED");
        trsl.decTakeFocus := XClient.ToAtom(trsl, "DEC_WM_TAKE_FOCUS");
        trsl.protocols := XClient.ToAtom(trsl, "WM_PROTOCOLS");
        trsl.deleteWindow := XClient.ToAtom(trsl, "WM_DELETE_WINDOW");
        trsl.miscAtom := XClient.ToAtom(trsl, "_DEC_TRESTLE_MISCCODE");
        trsl.paNewScreen := XClient.ToAtom(trsl, "_PALO_ALTO_NEW_SCREEN");
        trsl.paNewDisplay :=
          XClient.ToAtom(trsl, "_PALO_ALTO_NEW_DISPLAY");
        trsl.paAddDisplay :=
          XClient.ToAtom(trsl, "_PALO_ALTO_ADD_DISPLAY");
        SetUngrabs(trsl);
        XProperties.ExtendSel(trsl.sel, VBT.Target);
        trsl.sel[VBT.Target.sel].name := XClient.ToAtom(trsl, "SECONDARY");
        XProperties.ExtendSel(trsl.sel, VBT.Source);
        trsl.sel[VBT.Source.sel].name := XClient.ToAtom(trsl, "PRIMARY");
        XProperties.ExtendSel(trsl.sel, VBT.KBFocus);
        trsl.sel[VBT.KBFocus.sel].name := X.None;
        FixForOpenWin(trsl);
        IF hackdpy # NIL THEN
          TRY
            trsl.gcCursor :=
              X.XCreateFontCursor(hackdpy, 142 (*X.XC_trek*));
            IF trsl.gcCursor # X.None THEN
              VAR bg, fg: X.XColor;
              BEGIN
                bg.red := 65535;
                bg.green := 65535;
                bg.blue := 65535;
                bg.flags := X.DoRed + X.DoGreen + X.DoBlue;
                fg.red := 65535;
                fg.green := 0;
                fg.blue := 0;
                fg.flags := X.DoRed + X.DoGreen + X.DoBlue;
                X.XRecolorCursor(hackdpy, trsl.gcCursor, ADR(fg), ADR(bg))
              END
            END
          EXCEPT
            X.Error => trsl.gcCursor := X.None
          END
        END;
        XProperties.InitialiseXClient(trsl);
        XExtensions.InitXClient(trsl);
      FINALLY
        TrestleOnX.Exit(trsl, 1)
      END;
      FOR i := 0 TO LAST(trsl.screens^) DO
        trsl.screens[i] := XScreenType.New(trsl, trsl.dpy, i)
      END;
      XInput.Start(trsl);
      XMessenger.Start(trsl);
      TrestleOnX.Enter(trsl);
      TRY
        FOR i := 0 TO LAST(trsl.screens^) DO
          X.XSelectInput(trsl.dpy, trsl.screens[i].root, X.EnterWindowMask)
        END
      FINALLY
        TrestleOnX.Exit(trsl, -1)
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
    RETURN trsl
  END Connect;

PROCEDURE FixForOpenWin (trsl: T) RAISES {TrestleComm.Failure} =
  VAR
    selAtom := XClient.ToAtom(trsl, "_SUN_QUICK_SELECTION_KEY_STATE");
    dupAtom := XClient.ToAtom(trsl, "DUPLICATE");
    w       := X.XRootWindow(trsl.dpy, X.XDefaultScreen(trsl.dpy));
    type                  : X.Atom            := X.None;
    len, remaining: INTEGER;
    format: Ctypes.int;
    data                  : Ctypes.unsigned_char_star;
  BEGIN
    TRY
      EVAL X.XGetWindowProperty(
             trsl.dpy, w, selAtom, 0, 1, X.False, X.AnyPropertyType,
             ADR(type), ADR(format), ADR(len), ADR(remaining), ADR(data));
      IF type = X.None THEN
        X.XChangeProperty(trsl.dpy, w, selAtom, 4 (*atom*), 32,
                          X.PropModeReplace,
                          LOOPHOLE(ADR(dupAtom), Ctypes.unsigned_char_star),
                          1)
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END FixForOpenWin;

PROCEDURE DoConnect (<*UNUSED*> self     : TrestleClass.ConnectClosure;
                                inst     : TEXT;
                     <*UNUSED*> localOnly: BOOLEAN;
                     VAR (*OUT*) t: Trestle.T): BOOLEAN =
  BEGIN
    TRY
      t := Connect(inst);
      RETURN TRUE
    EXCEPT
      TrestleComm.Failure => t := NIL; RETURN FALSE
    END
  END DoConnect;

CONST
  Ungrab = ARRAY [0 .. 12] OF
             INTEGER{
             KeyboardKey.Caps_Lock, KeyboardKey.Shift_Lock,
             KeyboardKey.Meta_L, KeyboardKey.Meta_R, KeyboardKey.Alt_L,
             KeyboardKey.Alt_R, KeyboardKey.Super_L, KeyboardKey.Super_R,
             KeyboardKey.Hyper_L, KeyboardKey.Hyper_R,
             KeyboardKey.Scroll_Lock, KeyboardKey.Kana_Lock,
             KeyboardKey.Num_Lock};

PROCEDURE IOError (dpy: X.DisplayStar): Ctypes.int
  RAISES {X.Error} =
  VAR
    trsl : T := NIL;
    found    := FALSE;
  BEGIN
    IF doHack AND hackDpyTable # NIL THEN
      FOR i := 0 TO LAST(hackDpyTable^) DO
        IF dpy = hackDpyTable[i].dpy THEN RAISE X.Error END
      END
    END;
    LOCK errMu DO
      IF dpyTable # NIL THEN
        FOR i := 0 TO LAST(dpyTable^) DO
          IF dpyTable[i].dpy = dpy THEN
            trsl := dpyTable[i].trsl;
            found := TRUE;
            EXIT
          END
        END
      END
    END;
    IF trsl # NIL AND NOT trsl.dead THEN
      Kill(trsl)
    ELSIF NOT found THEN
      LOCK openMu DO IF NOT opening THEN RETURN iohandler(dpy) END END
    END;
    RAISE X.Error
  END IOError;

PROCEDURE Error (dpy: X.DisplayStar; errEv: X.XErrorEventStar):
  Ctypes.int =
  VAR
    trsl : T        := NIL;
    ev   : X.XEvent;
    found           := FALSE;
    <* FATAL X.Error *>
  BEGIN
    IF doHack AND hackDpyTable # NIL THEN
      FOR i := 0 TO LAST(hackDpyTable^) DO
        IF dpy = hackDpyTable[i].dpy THEN RETURN 0 END
      END
    END;
    WITH evp = LOOPHOLE(ADR(ev), X.XErrorEventStar) DO evp^ := errEv^ END;
    LOCK errMu DO
      IF dpyTable = NIL THEN RETURN ehandler(dpy, errEv) END;
      FOR i := 0 TO LAST(dpyTable^) DO
        IF dpyTable[i].dpy = dpy THEN
          trsl := dpyTable[i].trsl;
          found := TRUE;
          EXIT
        END
      END
    END;
    IF trsl # NIL THEN
      BackDoor(trsl, ev);
      RETURN 0
    ELSIF NOT found THEN
      RETURN ehandler(dpy, errEv)
    ELSE
      RETURN 0
    END;
  END Error;

VAR
  doHack := RTParams.IsPresent("StarTrek");
(* If doHack is TRUE, XClient will change the cursor of every installed
   window to the Star Trek cursor whenever the garbage collector is
   running.  You can enable this with @M3StarTrek. *)

TYPE
  GCClosure = RTHeapRep.MonitorClosure OBJECT
              OVERRIDES
                before := HackOn;
                after  := HackOff
              END;

TYPE InitClosure = Thread.Closure OBJECT OVERRIDES apply := DoHackInit END;

PROCEDURE DoHackInit (<*UNUSED*> self: InitClosure): REFANY =
  BEGIN
    IF doHack THEN RTHeapRep.RegisterMonitor(NEW(GCClosure)) END;
    RETURN NIL
  END DoHackInit;

VAR hacking := FALSE;

PROCEDURE HackOn (<*UNUSED*> cl: GCClosure) =
  BEGIN
    HackToggle(TRUE);
    hacking := TRUE
  END HackOn;

PROCEDURE HackOff (<*UNUSED*> cl: GCClosure) =
  BEGIN
    IF hacking THEN HackToggle(FALSE); hacking := FALSE END
  END HackOff;

PROCEDURE HackToggle (on: BOOLEAN) =
  <*FATAL Split.NotAChild*>
  VAR dead: BOOLEAN;
  BEGIN
    IF hackDpyTable = NIL THEN RETURN END;
    FOR i := 0 TO LAST(hackDpyTable^) DO
      WITH dpy  = hackDpyTable[i].dpy,
           trsl = hackDpyTable[i].trsl DO
        dead := dpy # NIL;
        IF dpy # NIL AND trsl # NIL AND NOT trsl.dead THEN
          TRY
            VAR v := Split.Succ(trsl, NIL);
            BEGIN
              WHILE v # NIL DO
                VAR ur: Child := v.upRef;
                BEGIN
                  IF ur # NIL AND ur.w # X.None AND ur.xcage # X.None THEN
                    IF on THEN
                      X.XDefineCursor(dpy, ur.w, trsl.gcCursor)
                    ELSE
                      X.XDefineCursor(dpy, ur.w, ur.csid)
                    END
                  END
                END;
                v := Split.Succ(trsl, v)
              END
            END;
            X.XSync(dpy, X.True);
            dead := FALSE
          EXCEPT
            X.Error => (* skip *)
          END
        END;
        IF dead THEN
          TRY
            X.XCloseDisplay(dpy)
          EXCEPT
            X.Error => (* skip *)
          END;
          dpy := NIL
        END
      END
    END;
  END HackToggle;

VAR
  ehandler  := X.XSetErrorHandler(Error);
  iohandler := X.XSetIOErrorHandler(IOError);

BEGIN
END XClientF.
