(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  6 09:30:40 PDT 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:34:43 PDT 1993 by muller     *)

UNSAFE MODULE RTProcedure EXPORTS RTProcedure, RTProcedureSRC;

IMPORT RT0, RTMisc, RTModule, Fingerprint, Word;

CONST
  Max_procedure_length = 20000; (* more than 99% of procedures are shorter
                                   than this. *)

TYPE
  Info     = RT0.ProcPtr;
  InfoList = UNTRACED REF ARRAY OF Info;
  NameList = UNTRACED REF ARRAY OF Name;

(* NOTE: since this module may be invoked during a startup
   crash, we don't use a mutex to protect writes to the
   global variables.  Instead, we assume that assigning a
   REF is an atomic operation.  It's possible that a thread
   race duing Init() will cause some garbage memory to be
   permanently allocated.  *)

VAR
 nProcs : INTEGER  := 0;
 info   : InfoList := NIL;
 units  : NameList := NIL;
 min_pc : ADDRESS  := NIL;
 max_pc : ADDRESS  := NIL;

(*----------------------------------------------------------- RTProcedure ---*)

PROCEDURE ToFingerprint (<*UNUSED*> p: Proc): Fingerprint.T =
  BEGIN
    RTMisc.FatalError (NIL, 0, "RTProcedure.ToFingerprint is not supported");
    RETURN Fingerprint.Zero;
  END ToFingerprint;

PROCEDURE FromFingerprint (<*UNUSED*> READONLY fp: Fingerprint.T): Proc =
  BEGIN
    RTMisc.FatalError (NIL, 0, "RTProcedure.FromFingerprint is not supported");
    RETURN NIL;
  END FromFingerprint;

(*-------------------------------------------------------- RTProcedureSRC ---*)

PROCEDURE NumProcedures (): CARDINAL =
  BEGIN
    IF (nProcs = 0) THEN CountProcs () END;
    RETURN nProcs;
  END NumProcedures;

PROCEDURE FromPC (pc: ADDRESS;  VAR p: Proc;  VAR file, name: Name) =
  VAR x: Info;  best, best_diff, diff: INTEGER;
  BEGIN
    IF (info = NIL) THEN Init () END;
    p    := NIL;
    name := NIL;
    file := NIL;

    IF (pc < min_pc) OR (max_pc <= pc) THEN (*don't bother*) RETURN; END;

    best := Locate (pc);  (* try the hash table for an exact match *)

    IF (best < 0) THEN    (* resort to linear search *)
      best_diff := LAST (INTEGER);
      FOR i := 0 TO LAST (info^) DO
        x := info[i];
        IF (x # NIL) THEN
          diff := (pc - x.proc);
          IF (0 <= diff) AND (diff < best_diff) THEN
            best := i;
            best_diff := diff;
          END;
        END;
      END;
    END;

    IF (best >= 0) THEN
      x    := info[best];
      p    := x.proc;
      name := x.name;
      file := units[best];
    END;
  END FromPC;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE CountProcs () =
  VAR j, n: INTEGER;  p: RT0.ProcPtr;
  BEGIN
    n := 0;
    FOR i := 0 TO RTModule.Count() - 1 DO
      p := RTModule.Get (i).proc_info;
      IF (p # NIL) THEN
        j := 0;
        WHILE (p^.proc # NIL) DO INC (p, ADRSIZE (p^)); INC (j) END;
        INC (n, j);
      END;
    END;
    nProcs := n; (* ... we'll assume that this update is atomic ... *)
  END CountProcs;

PROCEDURE Init () =
  VAR
    p: RT0.ProcPtr;
    m: RT0.ModulePtr;
    my_info: InfoList;
    my_units: NameList;
  BEGIN
    IF (nProcs = 0) THEN CountProcs () END;
    min_pc := LOOPHOLE (LAST (INTEGER), ADDRESS);
    max_pc := NIL;

    (* allocate the an array of Info pointers *)
    my_info  := NEW (InfoList, 3 * nProcs);
    my_units := NEW (NameList, 3 * nProcs);

    (* for each procedure, insert its info entry into the array *)
    FOR i := 0 TO RTModule.Count () - 1 DO
      m := RTModule.Get (i);
      p := m.proc_info;
      IF (p # NIL) THEN
        WHILE (p.proc # NIL) DO
          Insert (m, p^, my_info, my_units);
          IF (p.proc < min_pc) THEN min_pc := p.proc; END;
          IF (p.proc > max_pc) THEN max_pc := p.proc; END;
          INC (p, ADRSIZE (p^));
        END;
      END;
    END;

    (* update the globals to record the new info *)
    IF (info = NIL) THEN
      max_pc := max_pc + Max_procedure_length;
      units := my_units;
      info := my_info;
      (* ... we'll assume that this update is atomic ... *)
    END;
  END Init;

(** CONST Multiplier = 1052824; **)
CONST Multiplier = 2 * 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 + 1;
(* See Knuth Vol. 2, Theorem A, page 16. *)

PROCEDURE Insert (m: RT0.ModulePtr;  VAR xx: RT0.ProcInfo;
                  info: InfoList;  units: NameList) =
  (* called while 'mu' is held *)
  VAR x: Info;  hash, index: INTEGER;
  BEGIN
    hash := LOOPHOLE (xx.proc, INTEGER);
    LOOP
      index := Word.Mod (hash, NUMBER (info^));
      x := info [index];
      IF (x = NIL) THEN
        info [index] := ADR (xx);
        units [index] := m.file;
        RETURN;
      END;
      IF (x.proc = xx.proc) THEN  RETURN  END;
      hash := Word.Plus (1, Word.Times (hash, Multiplier));
    END;
  END Insert;

PROCEDURE Locate (proc: Proc): INTEGER =
  (* called while 'mu' is held *)
  VAR x: Info;  hash, index: INTEGER;
  BEGIN
    hash := LOOPHOLE (proc, INTEGER);
    LOOP
      index := Word.Mod (hash, NUMBER (info^));
      x := info [index];
      IF (x = NIL)       THEN RETURN -1 END;
      IF (x.proc = proc) THEN RETURN index   END;
      hash := Word.Plus (1, Word.Times (hash, Multiplier));
    END;
  END Locate;

BEGIN
END RTProcedure.

