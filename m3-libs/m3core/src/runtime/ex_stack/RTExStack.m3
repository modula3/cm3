(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTExStack EXPORTS RTException;

IMPORT RT0, RTOS, RTError, RTIO, RTStack, RTParams, RTEHScan;
IMPORT RTProcedureSRC, Cstring;
(*
IMPORT RTProcedureSRC, RTModule, RTProcedure, CString;
*)
FROM RT0 IMPORT RaiseActivation;

(*----------------------------------------- compiler generated descriptors --*)

(* This defines the low-level data structures
   used by the exception runtime's stack walker.
   See also m3middle/src/M3RT, which also describes these structures.
*)

(*
TYPE
  ScopeKind = { Except, ExceptElse,
                Finally, FinallyProc,
                Raises, RaisesNone,
                Lock };

TYPE
  Scope = UNTRACED REF RECORD
    kind        : CHAR;    (* ScopeKind *)
    outermost   : CHAR;    (* BOOLEAN => last scope that covers [start..stop]*)
    end_of_list : CHAR;    (* BOOLEAN => last scope in module list *)
    pad         : CHAR;
    start       : ADDRESS; (* first PC of the handled scope *)
    stop        : ADDRESS; (* last PC of the handled scope *)
    excepts     : ExceptionList; (* NIL-terminated list of handled exceptions *)
    offset      : INTEGER; (* frame offset of ExceptionInfo *)
  END;

TYPE
  ExceptionList = UNTRACED REF (*ARRAY OF*) RT0.ExceptionUID;
*)

(*---------------------------------------------------------------------------*)

VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;

EXCEPTION
  OUCH; (* to keep the compiler from complaining *)

TYPE
  CharArr = REF ARRAY OF CHAR;

(* The unwinder needs to alloc buffers for the context and the cursor.
   Called from the unwinder. *)
PROCEDURE AllocBuf(size : INTEGER) : ADDRESS =
  VAR arr : CharArr;
  BEGIN
    arr := NEW(CharArr,size);
    RETURN ADR(arr[0]);
  END AllocBuf;

PROCEDURE Raise (VAR act: RaiseActivation) RAISES ANY =
  VAR
    here, f: RTStack.Frame;
    excRef : REF RaiseActivation;
    scan : BOOLEAN;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RAISE", act);
      DumpStack ();
    END;

    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)
    LOOP
      IF (f.pc = NIL) THEN
        (* we're at the end of the stack (or we got lost along the way!) *)
        InvokeBackstop (act, raises := FALSE);
      END;

      IF f.lsda # NIL THEN
        (* scan the dwarf eh scopes found by the unwinder *)
        scan := RTEHScan.ScanEHTable(f, act.exception.uid);
        IF scan THEN
          excRef := NEW(REF RaiseActivation);
          excRef^ := act;
          ResumeRaise (excRef^)
        END;
      END;

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END; (* loop *)
  END Raise;

PROCEDURE ResumeRaise (VAR a: RaiseActivation) RAISES ANY =
  VAR
    here, f: RTStack.Frame;
    scan : BOOLEAN;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RERAISE", a);
      DumpStack ();
    END;

    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)
    LOOP
      IF (f.pc = NIL) THEN
        (* we're at the end of the stack (most likely unhandled exception) *)
        InvokeBackstop (a, raises := FALSE);
      END;

      IF f.lsda # NIL THEN
        (* scan the dwarf eh scopes found by the unwinder *)
        scan := RTEHScan.ScanEHTable(f, a.exception.uid);
        IF scan THEN
          (* landingpad set in scan *)
          f.excRef := ADR(a);
          RTStack.Unwind (f);
          RTError.MsgPC (LOOPHOLE (f.pc, INTEGER), "Unwind returned!");
          RAISE OUCH;
        END;
      END;

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END;
  END ResumeRaise;


(*----------------------------------------------------------- diagnostics ---*)

VAR NoName := ARRAY [0..15] OF CHAR {'s','t','a','t','i','c',' ',
                                     'p','r','o','c','e','d','u','r','e'};

PROCEDURE DumpStack () =
  CONST CallInstructionSize = 8; (* was 4 - should be gotten from Target *)
  VAR
    here, f: RTStack.Frame;
    name: RTProcedureSRC.Name;
    scan : BOOLEAN;
  BEGIN
    (* reimplement this proc *)
    IF NOT DEBUG AND NOT dump_enabled THEN RETURN; END;

    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    RTIO.PutText ("------------------------- STACK DUMP ---------------------------\n");
    RTIO.PutText ("----PC----  ----SP----  \n");
    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)

    WHILE (f.pc # NIL) DO

      (* print the active scopes *)

(* think the best we can do here is call EHScan and enable debug so it
prints out the exception table and action list *)
      IF f.lsda # NIL THEN
        (* scan the dwarf eh scopes found by the unwinder *)
(* dont have a uid maybe change scanehtable to take another parm debug
*)
        scan := RTEHScan.ScanEHTable(f, 0 (*a.exception.uid*));
      END;

      (* print the procedure's frame *)
      RTIO.PutAddr (f.pc - CallInstructionSize, 10);
      RTIO.PutText ("  ");
      RTIO.PutAddr (f.sp, 10);

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

(*
VAR NoName := ARRAY [0..15] OF CHAR {'s','t','a','t','i','c',' ',
                                     'p','r','o','c','e','d','u','r','e'};
PROCEDURE DumpStack () =
  CONST CallInstructionSize = 8; (* was 4 - should be gotten from Target *)
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
(* problem here with offset *)
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
      (*
      RTProcedureSRC.FromPC (f.pc, proc, file, name);
      IF (name # NIL) THEN
        offset := f.pc - proc;
        IF (0 <= offset) AND (offset < 2048) THEN
          RTIO.PutText ("  ");  RTIO.PutString (name);
          IF (offset # 0) THEN RTIO.PutText (" + "); RTIO.PutHex (offset); END;
          IF (file # NIL) THEN RTIO.PutText(" in "); RTIO.PutString(file); END;
        END;
      END;
      *)
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
*)

PROCEDURE PutExcept (tag: TEXT;  READONLY a: RaiseActivation) =
  BEGIN
    RTIO.PutText ("---> ");   RTIO.PutText (tag);
    RTIO.PutText (":  en=");  RTIO.PutAddr (a.exception);
    RTIO.PutText (" uid=");   RTIO.PutHex (a.exception.uid);
    RTIO.Flush ();
    RTIO.PutText (" ");       RTIO.PutString (a.exception.name);
    RTIO.PutText ("  arg=");  RTIO.PutAddr (a.arg);
    RTIO.PutText ("\n  module: ");  RTIO.PutAddr (a.module);
    IF (a.module # NIL) AND (a.module.file # NIL) THEN
      RTIO.PutText ("  ");          RTIO.PutString (a.module.file);
    END;
    RTIO.PutText ("\n  line: ");    RTIO.PutInt (a.line);
    RTIO.PutText ("   pc: ");       RTIO.PutAddr (a.pc);
    RTIO.PutText ("   info0: ");    RTIO.PutAddr (a.info0);
    RTIO.PutText ("   info1: ");    RTIO.PutAddr (a.info1);
    IF (a.un_except # NIL) THEN
      RTIO.PutText ("\n  unhandled: ");
      RTIO.PutText (" ");             RTIO.PutString (a.un_except.name);
      RTIO.PutText ("  arg=");        RTIO.PutAddr (a.un_arg);
    END;
    RTIO.PutText ("\n");
  END PutExcept;

BEGIN
  dump_enabled := RTParams.IsPresent ("stackdump");
  DEBUG := RTParams.IsPresent ("debugex");
  <*ASSERT RTStack.Has_walker*>
END RTExStack.
