(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTExStack EXPORTS RTException;

IMPORT RT0, RTOS, RTIO, RTStack, RTParams, RTEHScan;
IMPORT RTProcedureSRC;
FROM RT0 IMPORT RaiseActivation;
(*
IMPORT Cstring;
*)


VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;

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
  VAR excRef: REF RaiseActivation;
  BEGIN
    IF DEBUG THEN
      PutExcept ("RAISE", act);
      PrintStack (act.exception.uid);
    END;
    (* Copy the activation to the M3 heap so the pointer remains valid
       across stack unwinding.  ThrowM3Exc carries ADR(excRef^) as
       _M3Exc.act; the generated catch clauses store that into _m3_caught
       so that CompileHandler1's Load_addr(info) chain retrieves the
       correct exception descriptor and argument. *)
    excRef := NEW(REF RaiseActivation);
    excRef^ := act;
    RTStack.ThrowM3Exc(ADR(excRef^));
    (* ThrowM3Exc never returns; this is an unreachable safety net. *)
    InvokeBackstop (act, raises := FALSE);
  END Raise;

PROCEDURE ResumeRaise (VAR a: RaiseActivation) RAISES ANY =
  BEGIN
    IF DEBUG THEN
      PutExcept ("RERAISE", a);
      PrintStack (a.exception.uid);
    END;
    (* Re-throw via native C++ so the unwinder dispatches to the correct
       enclosing TRY/EXCEPT or TRY/FINALLY handler.  ADR(a) is a heap
       pointer because Raise always copies the activation to the M3 heap
       before throwing; ThrowM3Exc passes that address through as
       _M3Exc.act, which the catch clause stores into _m3_caught. *)
    RTStack.ThrowM3Exc(ADR(a));
    (* ThrowM3Exc never returns; this is an unreachable safety net. *)
    InvokeBackstop (a, raises := FALSE);
  END ResumeRaise;


(*----------------------------------------------------------- diagnostics ---*)

(*
VAR NoName := ARRAY [0..15] OF CHAR {'s','t','a','t','i','c',' ',
                                     'p','r','o','c','e','d','u','r','e'};
*)

PROCEDURE DumpStack () =
  BEGIN
    (* Match ex_frame: the stack dump is a diagnostic aid, off by default.
       Without this gate every unhandled exception (the RTProcess.Crash path)
       prints a full libunwind stack dump, which differs from the historical
       ex_frame behaviour and from the m3tests reference outputs. *)
    IF NOT DEBUG AND NOT dump_enabled THEN RETURN; END;
    PrintStack(0);
  END DumpStack;

PROCEDURE PrintStack (uid : INTEGER) =
  CONST CallInstructionSize = 8; (* was 4 - should be gotten from Target *)
  VAR
    here, f: RTStack.Frame;
    name: RTProcedureSRC.Name;
    scan : BOOLEAN;
  BEGIN
    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    RTIO.PutText ("------------------------- STACK DUMP ---------------------------\n");
    RTIO.PutText ("----PC----      ----SP----      --------Procedure--------\n");

    RTStack.CurrentFrame (here);
    RTStack.PreviousFrame (here, f); (* skip self *)

    WHILE (f.pc # NIL) DO

      IF f.lsda # NIL THEN
        IF uid # 0 THEN
          (* scan the dwarf eh scopes found by the unwinder *)
          scan := RTEHScan.ScanEHTable(f, uid);
          IF scan THEN
            (* print the exception stuff. Fixme. Would have to modify
               ScanEHTable to take a parameter to dump its debug info. *)
          END;
        END;
      END;

      (* print the procedure's frame *)
      (* This is only approximate. The pc will be the one after the call that
         results in the chain leading to crash (which prints this trace).
         And call instructions can vary in size. *)
      RTIO.PutAddr (f.pc - CallInstructionSize, 10);
      RTIO.PutText ("  ");
      RTIO.PutAddr (f.sp, 10);

      name := RTStack.ProcName (f);
      RTIO.PutText ("  [");
      IF (name # NIL)
        (* not sure why they were checking for "static procedure" probably
           an artifact of an old architecture/os *)
        (* AND Cstring.memcmp (name, ADR(NoName), NUMBER(NoName)) # 0 *) THEN
        RTIO.PutString (name);
      END;
      RTIO.PutText ("]\n");

      (* try the previous frame *)
      RTStack.PreviousFrame (f, f);
    END;

    RTIO.PutText ("----------------------------------------------------------------\n");
    RTIO.Flush ();

    RTOS.UnlockHeap (); (* re-enable thread switching *)
  END PrintStack;

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
  DEBUG := RTParams.IsPresent ("debugex");
  dump_enabled := RTParams.IsPresent ("stackdump");
  <*ASSERT RTStack.Has_walker*>
END RTExStack.
