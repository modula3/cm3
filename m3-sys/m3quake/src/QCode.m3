(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 13:22:27 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QCode;

IMPORT Quake;

REVEAL
  Stream = Stream_ BRANDED OBJECT OVERRIDES
    emit     := Emit;
    patch    := Patch;
    add_proc := AddProc;
  END;

PROCEDURE Emit (s: Stream;  op: Op;  a: INTEGER) =
  BEGIN
    IF (s.instrs = NIL) OR (s.cursor >= NUMBER (s.instrs^)) THEN
      ExpandInstrs (s);
    END;
    WITH i = s.instrs [s.cursor] DO  i.op := op;  i.a := a;  END;
    INC (s.cursor);
  END Emit;

PROCEDURE ExpandInstrs (s: Stream) =
  VAR n: INTEGER;  new: REF ARRAY OF Instr;
  BEGIN
    IF (s.instrs = NIL) THEN
      s.instrs := NEW (REF ARRAY OF Instr, 100);
    ELSE
      n := NUMBER (s.instrs^);
      new := NEW (REF ARRAY OF Instr, n + n);
      SUBARRAY (new^, 0, n) := s.instrs^;
      s.instrs := new;
      new := NIL;
    END;
  END ExpandInstrs;

PROCEDURE Patch (s: Stream;  pc: INTEGER;  op: Op;  a: INTEGER) =
  BEGIN
    WITH i = s.instrs[pc] DO  i.op := op;  i.a := a;  END;
  END Patch;

PROCEDURE AddProc (s: Stream;  nm: Quake.ID): INTEGER =
  BEGIN
    IF (s.procs = NIL) OR (s.n_procs >= NUMBER (s.procs^)) THEN
      ExpandProcs (s);
    END;
    s.procs [s.n_procs] := NEW (ProcInfo,
                                code := s,
                                entry := s.cursor,
                                name := nm);
    INC (s.n_procs);
    RETURN s.n_procs - 1;
  END AddProc;

PROCEDURE ExpandProcs (s: Stream) =
  VAR n: INTEGER;  new: REF ARRAY OF ProcInfo;
  BEGIN
    IF (s.procs = NIL) THEN
      s.procs := NEW (REF ARRAY OF ProcInfo, 10);
    ELSE
      n := NUMBER (s.procs^);
      new := NEW (REF ARRAY OF ProcInfo, n + n);
      SUBARRAY (new^, 0, n) := s.procs^;
      s.procs := new;
      new := NIL;
    END;
  END ExpandProcs;

BEGIN
END QCode.
