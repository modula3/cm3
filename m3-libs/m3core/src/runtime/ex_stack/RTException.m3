(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May  2 11:48:50 PDT 1995 by kalsow     *)
(*      modified on Wed Jun  2 15:21:34 PDT 1993 by muller     *)

UNSAFE MODULE RTException EXPORTS RTException, RTExRep;

IMPORT RT0, RTMisc, RTProcedureSRC, RTIO, RTModule, RTOS, RTStack;
IMPORT Thread, M3toC, Cstring, Ctypes, RTProcedure, RTParams;

VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;

EXCEPTION
  OUCH; (* to keep the compiler from complaining *)

PROCEDURE Raise (en: ExceptionPtr;  arg: ExceptionArg) RAISES ANY =
  VAR
    here, f: RTStack.Frame;
    s: Scope;
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RAISE", en, arg);
      RTIO.PutText ("\n");
      DumpStack ();
    END;

    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)
    LOOP
      IF (f.pc = NIL) THEN
        (* we're at the end of the stack (or we got lost along the way!) *)
        NoHandler (en, raises := FALSE);
      END;

      s := FindScope (f.pc);
      IF (s # NIL) THEN
        LOOP
          IF (s.start <= f.pc) AND (f.pc <= s.stop) THEN
            CASE ORD (s.kind) OF
            | ORD (ScopeKind.Except) =>
                ex := s.excepts;
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
            | ORD (ScopeKind.RaisesNone) =>
                NoHandler (en);
            | ORD (ScopeKind.Raises) =>
                (* check that this procedure does indeed raise 'en' *)
                ex := s.excepts;
                IF ex = NIL THEN NoHandler (en); END;
                LOOP
                  IF (ex^ = 0) THEN  NoHandler (en) END;
                  IF (ex^ = en.uid)  THEN  (* ok, it passes *) EXIT  END;
                  INC (ex, ADRSIZE (ex^));
                END;
            ELSE <*ASSERT FALSE*>
            END;

            IF (s.outermost # '\000') THEN EXIT END;
          END;

          IF (s.end_of_list # '\000') THEN EXIT END;

          (* try the next scope in the list *)
          INC (s, ADRSIZE (s^));
        END;
      END;

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END;
  END Raise;

PROCEDURE ResumeRaise (en: ExceptionPtr;  arg: ExceptionArg) RAISES ANY =
  VAR
    here, f: RTStack.Frame;
    s: Scope;
    ex: ExceptionList;
  BEGIN

    IF DEBUG THEN
      PutExcept ("RERAISE", en, arg);
      RTIO.PutText ("\n");
      DumpStack ();
    END;

    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)
    LOOP
      IF (f.pc = NIL) THEN
        (* we're at the end of the stack (or we got lost along the way!) *)
        NoHandler (en, raises := FALSE);
      END;

      s := FindScope (f.pc);
      IF (s # NIL) THEN
        LOOP
          IF (s.start <= f.pc) AND (f.pc <= s.stop) THEN
            CASE ORD (s.kind) OF
            | ORD (ScopeKind.Except) =>
                ex := s.excepts;
                WHILE (ex^ # 0) DO
                  IF (ex^ = en.uid) THEN InvokeHandler (s, f, en, arg) END;
                  INC (ex, ADRSIZE (ex^));
                END;
                MarkHandler (s, f, en, arg);
                (* we need to mark every frame so that no matter where
                   we unwind to, it sees a marked frame => exception *)
            | ORD (ScopeKind.ExceptElse) =>
                (* 's' is a TRY-EXCEPT-ELSE frame => go for it *)
                InvokeHandler (s, f, en, arg);
            | ORD (ScopeKind.Finally),
              ORD (ScopeKind.FinallyProc) =>
                InvokeHandler (s, f, en, arg);
            | ORD (ScopeKind.Lock) =>
                ReleaseLock (s, f);
            | ORD (ScopeKind.Raises) =>
                (* already checked during the first pass *)
            ELSE <*ASSERT FALSE *>
            END;

            IF (s.outermost # '\000') THEN EXIT END;
          END;

          IF (s.end_of_list # '\000') THEN EXIT END;

          (* try the next scope in the list *)
          INC (s, ADRSIZE (s^));
        END;
      END;

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END;
  END ResumeRaise;

PROCEDURE InvokeHandler (s: Scope;  READONLY f: RTStack.Frame;
                         en: ExceptionPtr;  arg: ExceptionArg) RAISES ANY =
  VAR p : UNTRACED REF ExceptionInfo := f.sp + s.offset;
  BEGIN
    IF DEBUG THEN
      PutExcept ("INVOKE HANLDER", en, arg);
      PutFrame (f, p);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    p.exception := en;
    p.arg := arg;
    RTStack.Unwind (f);
    RTMisc.FatalErrorPC (LOOPHOLE (f.pc, INTEGER), "Unwind returned!");
    RAISE OUCH;
  END InvokeHandler;

PROCEDURE MarkHandler (s: Scope;  READONLY f: RTStack.Frame;
                         en: ExceptionPtr;  arg: ExceptionArg) =
  VAR p : UNTRACED REF ExceptionInfo := f.sp + s.offset;
  BEGIN
    IF DEBUG THEN
      PutExcept ("MARK HANDLER", en, arg);
      PutFrame (f, p);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    p.exception := en;
    p.arg := arg;
  END MarkHandler;

PROCEDURE ReleaseLock (s: Scope;  READONLY f: RTStack.Frame) =
  VAR p : UNTRACED REF MUTEX := f.sp + s.offset;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> UNLOCK:");
      PutFrame (f, p);
      RTIO.PutText ("  mutex=");  RTIO.PutAddr (LOOPHOLE (p^, ADDRESS));
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    <*ASSERT p^ # NIL *>
    Thread.Release (p^);
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

(*------------------------------------------------------- scope searching ---*)
(* Note: we assume that the text of a single compilation unit is contiguous
   (i.e. the linker doesn't split and shuffle compilation units in little
    pieces). *)

TYPE
  PCMap = UNTRACED REF ARRAY OF MapEntry;
  MapEntry = RECORD
    base   : ADDRESS;
    module : RT0.ModulePtr;
  END;

VAR pc_map: PCMap := NIL;

PROCEDURE FindScope (pc: ADDRESS): Scope =
  VAR
    base: ADDRESS;
    lo, hi, mid, limit: CARDINAL;
    p: UNTRACED REF MapEntry;
    s: Scope;
  BEGIN
    IF (pc_map = NIL) THEN
      RTOS.LockHeap ();
        BuildPCMap ();
      RTOS.UnlockHeap ();
    END;

    (* binary search of the sorted table *)
    limit:= NUMBER (pc_map^);
    base := ADR (pc_map[0]);
    lo   := 0;
    hi   := limit;
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      p := base + mid * ADRSIZE (p^);
      IF (pc < p.base)
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;
    IF (lo > 0) THEN DEC (lo) END;

    (* linear search of the modules that might contain pc *)
    LOOP
      IF (lo >= limit) THEN RETURN NIL END;
      p := base + lo * ADRSIZE (p^);
      IF (p.base > pc) THEN RETURN NIL END;
      IF FindScopeInModule (pc, p.module.try_scopes, s) THEN RETURN s END;
      INC (lo);
    END;
  END FindScope;

PROCEDURE FindScopeInModule (pc: ADDRESS;  s: Scope;  VAR x: Scope): BOOLEAN =
  VAR above, below: BOOLEAN := FALSE;
  BEGIN
    x := NIL;
    IF (s = NIL) THEN RETURN FALSE END;
    LOOP
      IF (s.start <= pc) AND (pc <= s.stop) THEN  x := s; RETURN TRUE;  END;
      IF (s.start <= pc) THEN below := TRUE END;
      IF (s.stop >= pc) THEN above := TRUE END;
      IF (s.end_of_list # '\000') THEN RETURN (above AND below) END;
      INC (s, ADRSIZE (s^));
    END;
  END FindScopeInModule;

(*----------------------------------------------------- sorted module map ---*)

PROCEDURE BuildPCMap () =
  VAR n: INTEGER;  map: PCMap;  m: RT0.ModulePtr;
  BEGIN
    (* first, count the modules with exception scopes *)
    n := 0;
    FOR i := 0 TO RTModule.Count () - 1 DO
      m := RTModule.Get (i);
      IF (m # NIL) AND (m.try_scopes # NIL) THEN INC (n); END;
    END;

    (* allocate space for the map *)
    map := NEW (PCMap, n);

    (* install the modules with exception scopes into the map *)
    n := 0;
    FOR i := 0 TO RTModule.Count () - 1  DO
      m := RTModule.Get (i);
      IF (m # NIL) AND (m.try_scopes # NIL) THEN
        map[n].base := MinPC (m);
        map[n].module := m;
        INC (n);
      END;
    END;

    (* sort the maps *)
    QuickSort (map^, 0, n);
    InsertionSort (map^, 0, n);

    (* and finally, install them *)
    pc_map := map;
  END BuildPCMap;

PROCEDURE MinPC (m: RT0.ModulePtr): ADDRESS =
  VAR
    s   := LOOPHOLE (m.try_scopes, Scope);
    min := s.start;
  BEGIN
    LOOP
      IF (s.start < min) THEN min := s.start; END;
      IF (s.end_of_list # '\000') THEN EXIT; END;
      INC (s, ADRSIZE (s^));
    END;
    RETURN min;
  END MinPC;

PROCEDURE QuickSort (VAR a: ARRAY OF MapEntry;  lo, hi: INTEGER) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp, t_lo, t_hi, t_i: MapEntry;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      t_lo := a [lo];
      t_hi := a [hi-1];
      t_i  := a [i];
      IF (t_lo.base < t_i.base) THEN
        IF (t_i.base < t_hi.base) THEN
          key := t_i;
        ELSIF (t_lo.base < t_hi.base) THEN
          key := t_hi;
          a[hi-1] := t_i;
          a[i] := key;
        ELSE
          key := t_lo;
          a[lo] := t_hi;
          a[hi-1] := t_i;
          a[i] := key;
        END;
      ELSE
        IF (t_hi.base < t_i.base) THEN
          key := t_i;
          a[hi-1] := t_lo;
          a[lo] := t_hi;
        ELSIF (t_lo.base < t_hi.base) THEN
          key := t_lo;
          a[lo] := t_i;
          a[i] := t_lo;
        ELSE
          key := t_hi;
          a[hi-1] := t_lo;
          a[lo] := t_i;
          a[i] := t_hi;
        END;
      END;

      (* partition the array *)

      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE (a [j].base > key.base) DO DEC (j) END;
      tmp := a [j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE (a [i].base < key.base) DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a [i];
        INC (i);

        WHILE (a [j].base > key.base) DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a [j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1);   lo := i;
        ELSE  QuickSort (a, i, hi);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;

PROCEDURE InsertionSort (VAR a: ARRAY OF MapEntry;  lo, hi: INTEGER) =
  VAR j: INTEGER;  key: MapEntry;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a [i];
      j := i-1;
      WHILE (j >= lo) AND (key.base < a [j].base) DO
        a[j+1] := a [j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

(*----------------------------------------------------------- diagnostics ---*)

VAR NoName := ARRAY [0..15] OF CHAR {'s','t','a','t','i','c',' ',
                                     'p','r','o','c','e','d','u','r','e'};
PROCEDURE DumpStack () =
  CONST CallInstructionSize = 4;
  VAR
    here, f: RTStack.Frame;
    s: Scope;
    ex: ExceptionList;
    name: RTProcedureSRC.Name;
    file: RTProcedureSRC.Name;
    proc: RTProcedure.Proc;
    offset: INTEGER;
    info: ADDRESS;
  BEGIN
    IF NOT DEBUG AND NOT dump_enabled THEN RETURN; END;

    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    RTIO.PutText ("------------------------- STACK DUMP ---------------------------\n");
    RTIO.PutText ("----PC----  ----SP----  \n");
    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)

    WHILE (f.pc # NIL) DO

      (* print the active scopes *)
      s := FindScope (f.pc);
      IF (s # NIL) THEN
        LOOP
          IF (s.start <= f.pc) AND (f.pc <= s.stop) THEN
            RTIO.PutText ("   [");
            RTIO.PutAddr (s.start);
            RTIO.PutText ("..");
            RTIO.PutAddr (s.stop);
            RTIO.PutText ("]  ");
            info := f.sp + s.offset;
            CASE ORD (s.kind) OF
            | ORD (ScopeKind.Finally),
              ORD (ScopeKind.FinallyProc) =>
                RTIO.PutText ("TRY-FINALLY");
                DumpInfo (info);
            | ORD (ScopeKind.Lock) =>
                RTIO.PutText ("LOCK");
                DumpInfo (info);
                RTIO.PutText ("  mutex = ");
                RTIO.PutAddr (LOOPHOLE (info, UNTRACED REF ADDRESS)^);
            | ORD (ScopeKind.Except) =>
                ex := s.excepts;
                RTIO.PutText ("TRY-EXCEPT");  DumpHandles (ex);
                DumpInfo (info);
            | ORD (ScopeKind.ExceptElse) =>
                RTIO.PutText ("TRY-EXCEPT-ELSE");
                DumpInfo (info);
            | ORD (ScopeKind.Raises),
              ORD (ScopeKind.RaisesNone) =>
                RTIO.PutText ("RAISES");
                DumpHandles (s.excepts);
            ELSE
                (* we found a mysterious scope!? *)
                RTIO.PutText ("??? BAD EXCEPTION SCOPE, kind = ");
                RTIO.PutInt (ORD (s.kind));
                RTIO.PutText (" ???\n");
                EXIT;
            END;
            RTIO.PutText ("\n");

            IF (s.outermost # '\000') THEN EXIT END;
          END;

          IF (s.end_of_list # '\000') THEN EXIT END;

          (* try the next scope in the list *)
          INC (s, ADRSIZE (s^));
        END;
      END;

      (* print the procedure's frame *)
      RTIO.PutAddr (f.pc-CallInstructionSize, 10);
      RTIO.PutText ("  ");
      RTIO.PutAddr (f.sp, 10);
      RTProcedureSRC.FromPC (f.pc, proc, file, name);
      IF (name # NIL) THEN
        offset := f.pc - proc;
        IF (0 <= offset) AND (offset < 2048) THEN
          RTIO.PutText ("  ");  RTIO.PutString (name);
          IF (offset # 0) THEN RTIO.PutText (" + "); RTIO.PutHex (offset); END;
          IF (file # NIL) THEN RTIO.PutText(" in "); RTIO.PutString(file); END;
        END;
      END;
      name := RTStack.ProcName (f);
      IF (name # NIL)
        AND Cstring.memcmp (name, ADR(NoName), NUMBER(NoName)) # 0 THEN
        RTIO.PutText ("  [");  RTIO.PutString (name);  RTIO.PutText ("]");
      END;
      RTIO.PutText ("\n");

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END;
    RTIO.PutText ("----------------------------------------------------------------\n");
    RTIO.Flush ();

    RTOS.UnlockHeap (); (* re-enable thread switching *)
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

PROCEDURE DumpInfo (a: ADDRESS) =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("  info=");
      RTIO.PutAddr (a);
    END;
  END DumpInfo;

PROCEDURE PutExcept (tag: TEXT;  en: ExceptionPtr;  arg: ExceptionArg) =
  BEGIN
    RTIO.PutText ("---> ");   RTIO.PutText (tag);
    RTIO.PutText (":  en=");  RTIO.PutAddr (en);
    RTIO.PutText (" uid=");   RTIO.PutHex (en.uid);
    RTIO.PutText (" ");       RTIO.PutString (en.name);
    RTIO.PutText ("  arg=");  RTIO.PutAddr (arg);
  END PutExcept;

PROCEDURE PutFrame (READONLY f: RTStack.Frame;  info: ADDRESS) =
  BEGIN
    RTIO.PutText ("  pc=");    RTIO.PutAddr (f.pc);
    RTIO.PutText ("  sp=");    RTIO.PutAddr (f.sp);
    RTIO.PutText ("  info=");  RTIO.PutAddr (info);
  END PutFrame;

PROCEDURE EName (en: ExceptionPtr): TEXT =
  BEGIN
    RETURN M3toC.StoT (LOOPHOLE (en.name, Ctypes.char_star));
  END EName;

BEGIN
  dump_enabled := RTParams.IsPresent ("stackdump");
  <*ASSERT RTStack.Has_walker*>
END RTException.
