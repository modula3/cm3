(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 26 13:47:05 PST 1995 by kalsow     *)
(*      modified on Wed Jun  2 15:21:34 PDT 1993 by muller     *)

UNSAFE MODULE RTException EXPORTS RTException, RTExRep;

IMPORT RT0, RTMisc, RTIO, RTParams, RTOS;
IMPORT Thread, RTThread, M3toC, Ctypes, Csetjmp;

VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;

TYPE
  FinallyProc = PROCEDURE () RAISES ANY;

EXCEPTION
  OUCH; (* to keep the compiler from complaining *)

PROCEDURE Raise (en: ExceptionPtr;  arg: ExceptionArg) RAISES ANY =
  VAR
    f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RAISE", en, arg);
      RTIO.PutText ("\n");
      DumpStack ();
    END;

    LOOP
      IF (f = NIL) THEN NoHandler (en, raises := FALSE); END;

      CASE f.class OF
      | ORD (ScopeKind.Except) =>
          ex := LOOPHOLE (f, PF1).handles;
          WHILE (ex^ # 0) DO
            IF (ex^ = en.uid) THEN ResumeRaise (en, arg) END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.ExceptElse) =>
          (* 's' is a TRY-EXCEPT-ELSE frame => go for it *)
          ResumeRaise (en, arg);
      | ORD (ScopeKind.Finally),
        ORD (ScopeKind.FinallyProc),
        ORD (ScopeKind.Lock) =>
          (* ignore for this pass *)
      | ORD (ScopeKind.Raises) =>
          (* check that this procedure does indeed raise 'en' *)
          ex := LOOPHOLE (f, PF3).raises;
          IF ex = NIL THEN NoHandler (en); END;
          LOOP
            IF (ex^ = 0) THEN  NoHandler (en) END;
            IF (ex^ = en.uid)  THEN  (* ok, it passes *) EXIT  END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.RaisesNone) =>
          NoHandler (en);
      ELSE
        BadStack ();
      END;

      f := f.next;   (* try the previous frame *)
    END;
  END Raise;

PROCEDURE ResumeRaise (en: ExceptionPtr;  arg: ExceptionArg) RAISES ANY =
  VAR
    f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RERAISE", en, arg);
      RTIO.PutText ("\n");
      DumpStack ();
    END;

    LOOP
      IF (f = NIL) THEN  BadStack ();  END;

      CASE f.class OF
      | ORD (ScopeKind.ExceptElse),
        ORD (ScopeKind.Finally) =>
          InvokeHandler (f, en, arg);
      | ORD (ScopeKind.Except) =>
          ex := LOOPHOLE (f, PF1).handles;
          WHILE (ex^ # 0) DO
            IF (ex^ = en.uid) THEN InvokeHandler (f, en, arg) END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.FinallyProc) =>
          InvokeFinallyHandler (f, en, arg);
      | ORD (ScopeKind.Lock) =>
          ReleaseLock (f);
      | ORD (ScopeKind.Raises) =>
          (* already checked during the first pass *)
      ELSE
          BadStack ();
      END;

      RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
      f := f.next;                         (* try the previous frame *)
    END;
  END ResumeRaise;

PROCEDURE InvokeHandler (f: Frame;  en: ExceptionPtr;
                         arg: ExceptionArg) RAISES ANY =
  VAR p := LOOPHOLE (f, PF1);
  BEGIN
    IF DEBUG THEN
      PutExcept ("INVOKE HANDLER", en, arg);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    p.exception := en;                   (* record the exception *)
    p.arg := arg;                        (* and it argument *)
    Csetjmp.ulongjmp (p.jmpbuf, 1);      (* and jump... *)
    RAISE OUCH;
  END InvokeHandler;

PROCEDURE InvokeFinallyHandler (f: Frame; en: ExceptionPtr;
                                arg: ExceptionArg) RAISES ANY =
  VAR
    p := LOOPHOLE (f, PF2);
    cl: RT0.ProcedureClosure;
  BEGIN
    IF DEBUG THEN
      PutExcept ("INVOKE FINALLY HANDLER", en, arg);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    (* build a nested procedure closure  *)
    cl.marker := RT0.ClosureMarker;
    cl.proc   := p.handler;
    cl.frame  := p.frame;
    
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    CallProc (LOOPHOLE (ADR (cl), FinallyProc));
  END InvokeFinallyHandler;

PROCEDURE CallProc (p: FinallyProc) RAISES ANY =
  (* we need to fool the compiler into generating a call
     to a nested procedure... *)
  BEGIN
    p ();
  END CallProc;

PROCEDURE ReleaseLock (f: Frame) =
  VAR p := LOOPHOLE (f, PF4);
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> UNLOCK:");
      RTIO.PutText ("  frame=");  RTIO.PutAddr (p);
      RTIO.PutText ("  mutex=");  RTIO.PutAddr (LOOPHOLE (p.mutex, ADDRESS));
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    RTThread.SetCurrentHandlers (f.next); (* cut to the new handler *)
    Thread.Release (p.mutex);            (* and release the lock *)
  END ReleaseLock;

PROCEDURE NoHandler (en: ExceptionPtr;  raises := TRUE) =
  VAR nm := EName (en);
  BEGIN
    IF (raises) THEN
      RTMisc.FatalError (NIL, 0, "Exception \"", nm, "\" not in RAISES list");
    ELSE
      RTMisc.FatalError (NIL, 0, "Unhandled exception \"", nm, "\"");
    END;
  END NoHandler;

PROCEDURE BadStack () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "corrupt exception stack");
  END BadStack;

(*----------------------------------------------------------- diagnostics ---*)

PROCEDURE SanityCheck () =
  CONST Min_SK = ORD (FIRST (ScopeKind));
  CONST Max_SK = ORD (LAST (ScopeKind));
  VAR f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
  VAR i: INTEGER;
  BEGIN
    WHILE (f # NIL) DO
      i := f.class;
      IF (i < Min_SK) OR (Max_SK < i) THEN BadStack () END;
      f := f.next;
    END;
  END SanityCheck;

PROCEDURE DumpStack () =
  VAR f := LOOPHOLE(RTThread.GetCurrentHandlers(), Frame);
  BEGIN
    IF NOT DEBUG AND NOT dump_enabled THEN RETURN; END;

    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    RTIO.PutText ("------------------ EXCEPTION HANDLER STACK ---------------------\n");
    WHILE (f # NIL) DO
      RTIO.PutAddr (f);

      CASE f.class OF
      | ORD (ScopeKind.Except) =>
          RTIO.PutText (" TRY-EXCEPT ");
          DumpHandles (LOOPHOLE (f, PF1).handles);
      | ORD (ScopeKind.ExceptElse) =>
          RTIO.PutText (" TRY-EXCEPT-ELSE ");
      | ORD (ScopeKind.Finally) =>
          RTIO.PutText (" TRY-FINALLY ");
      | ORD (ScopeKind.FinallyProc) =>
          VAR x := LOOPHOLE (f, PF2); BEGIN
            RTIO.PutText (" TRY-FINALLY  proc = ");
            RTIO.PutAddr (x.handler);
            RTIO.PutText ("   frame = ");
            RTIO.PutAddr (x.frame);
          END;
      | ORD (ScopeKind.Raises) =>
          RTIO.PutText (" RAISES ");
          DumpHandles (LOOPHOLE (f, PF3).raises);
      | ORD (ScopeKind.RaisesNone) =>
          RTIO.PutText (" RAISES {}");
      | ORD (ScopeKind.Lock) =>
          VAR x := LOOPHOLE (f, PF4); BEGIN
            RTIO.PutText (" LOCK  mutex = ");
            RTIO.PutAddr (LOOPHOLE (x.mutex, ADDRESS));
          END;
      ELSE
         RTIO.PutText (" *** BAD EXCEPTION RECORD, class = ");
         RTIO.PutInt (f.class);
         RTIO.PutText (" ***\n");
         EXIT;
      END;
      RTIO.PutText ("\n");
      f := f.next;
    END;
    RTIO.PutText ("----------------------------------------------------------------\n");
    RTIO.Flush ();

    RTOS.UnlockHeap ();
  END DumpStack;

PROCEDURE DumpHandles (x: ExceptionList) =
  VAR first := TRUE;
  BEGIN
    RTIO.PutText (" {");
    IF (x # NIL) THEN
      WHILE (x^ # 0) DO
        IF (NOT first) THEN RTIO.PutText (", ");  END;
        first := FALSE;
        RTIO.PutHex (x^);
        INC (x, ADRSIZE (x^));
      END;
    END;
    RTIO.PutText ("}");
  END DumpHandles;

PROCEDURE PutExcept (tag: TEXT;  en: ExceptionPtr;  arg: ExceptionArg) =
  BEGIN
    RTIO.PutText ("---> ");   RTIO.PutText (tag);
    RTIO.PutText (":  en=");  RTIO.PutAddr (en);
    RTIO.PutText (" uid=");   RTIO.PutHex (en.uid);
    RTIO.PutText (" ");       RTIO.PutString (en.name);
    RTIO.PutText ("  arg=");  RTIO.PutAddr (arg);
  END PutExcept;

PROCEDURE EName (en: ExceptionPtr): TEXT =
  BEGIN
    RETURN M3toC.StoT (LOOPHOLE (en.name, Ctypes.char_star));
  END EName;

BEGIN
  dump_enabled := RTParams.IsPresent ("stackdump");
  EVAL SanityCheck; (* avoid the unused warning *)
END RTException.
