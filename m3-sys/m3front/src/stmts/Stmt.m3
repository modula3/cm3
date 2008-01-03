(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Stmt.m3                                               *)
(* Last modified on Thu Apr  6 12:59:21 PDT 1995 by kalsow     *)
(*      modified on Tue Jan 16 06:44:48 1990 by muller         *)

MODULE Stmt EXPORTS Stmt, StmtRep;

IMPORT CG, AssertStmt, AssignStmt, BlockStmt, CaseStmt, ExitStmt;
IMPORT EvalStmt, ForStmt, IfStmt, LockStmt, LoopStmt, RepeatStmt;
IMPORT ReturnStmt, RaiseStmt, TryStmt, TypeCaseStmt, WhileStmt, WithStmt;
IMPORT Scanner, Token, Coverage, Error, Tracer, M3ID, DebugStmt;
FROM Scanner IMPORT GetToken, cur;

TYPE TK = Token.T;

PROCEDURE Parse (): T =
  VAR t, first, last: T;
  BEGIN
    first := NIL;  last := NIL;
    LOOP
      CASE cur.token OF
      | TK.tCONST,
        TK.tTYPE,
        TK.tREVEAL,
        TK.tVAR,
        TK.tEXTERNAL,
        TK.tINLINE,
        TK.tUNUSED,
        TK.tOBSOLETE,
        TK.tEXCEPTION,
        TK.tCALLCONV,
        TK.tPROCEDURE,
        TK.tFATAL,
        TK.tBEGIN    => t := BlockStmt.Parse (TRUE);
      | TK.tIDENT,
        TK.tLPAREN,
        TK.tARRAY,
        TK.tRECORD   => t := AssignStmt.Parse ();
      | TK.tASSERT   => t := AssertStmt.Parse ();
      | TK.tCASE     => t := CaseStmt.Parse ();
      | TK.tDEBUG    => t := DebugStmt.Parse ();
      | TK.tEXIT     => t := ExitStmt.Parse ();
      | TK.tEVAL     => t := EvalStmt.Parse ();
      | TK.tFOR      => t := ForStmt.Parse ();
      | TK.tIF       => t := IfStmt.Parse ();
      | TK.tLOCK     => t := LockStmt.Parse ();
      | TK.tLOOP     => t := LoopStmt.Parse ();
      | TK.tRAISE    => t := RaiseStmt.Parse ();
      | TK.tREPEAT   => t := RepeatStmt.Parse ();
      | TK.tRETURN   => t := ReturnStmt.Parse ();
      | TK.tTRY      => t := TryStmt.Parse ();
      | TK.tTYPECASE => t := TypeCaseStmt.Parse ();
      | TK.tWHILE    => t := WhileStmt.Parse ();
      | TK.tWITH     => t := WithStmt.Parse ();
      ELSE RETURN first;
      END;
      IF (first = NIL) THEN first := t ELSE last.next := t END;
      last := t;
      IF (cur.token = TK.tSEMI) THEN
        GetToken (); (* ; *)
        VAR err_line := -1; BEGIN
          WHILE (cur.token = TK.tSEMI) DO
            (* handle empty statements gracefully *)
            IF (err_line # Scanner.offset) THEN
              Error.Msg ("empty statement");
              err_line := Scanner.offset;
            END;
            GetToken (); (* ; *)
          END;
        END;
      ELSIF cur.token IN Token.StmtStart THEN
        (* assume the simple mistake and keep going *)
        Error.Msg ("expected \";\", found " &
          M3ID.ToText (Token.name[cur.token]) & " instead");
      ELSE
        RETURN first;
      END;
    END;
    (* RETURN first; *)
  END Parse;

PROCEDURE Init (t: T) =
  BEGIN
    t.next   := NIL;
    t.origin := Scanner.offset;
  END Init;

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState) =
  (* we defer calling Coverage.NoteLine until typechecking because
     some statements (e.g. assign, call) adjust their origins after
     they've called Init. *)
  BEGIN
    WHILE (t # NIL) DO
      Scanner.offset := t.origin;
      Coverage.NoteLine ();
      t.check (cs);
      t := t.next;
    END;
  END TypeCheck;

PROCEDURE Compile (t: T): Outcomes =
  VAR oc, xc: Outcomes;  (**** x: T; ***)
  BEGIN
    (*** x := t; ****)
    oc := Outcomes {Outcome.FallThrough};
    WHILE (t # NIL) DO
      CG.Free_temps ();
      Scanner.offset := t.origin;
      CG.Gen_location (t.origin);
      Coverage.CountLine ();
      Tracer.EmitPending ();
      xc := t.compile ();
      (**** DumpOutcome (t, xc); ****)
      oc := oc + xc;
      IF (Outcome.FallThrough IN xc) THEN
        t := t.next;
      ELSE
        IF (t.next # NIL) THEN
          Scanner.offset := t.next.origin;
          Error.Warn (1, "unreachable statement");
        END;
        t := NIL;
        oc := oc - Outcomes {Outcome.FallThrough};
      END;
    END;
    Tracer.EmitPending ();
    (*** DumpOutcome (x, oc); ****)
    CG.Free_temps ();
    RETURN oc;
  END Compile;

PROCEDURE GetOutcome (t: T): Outcomes =
  VAR oc, xc: Outcomes;  x: T;
  BEGIN
    x := t;
    oc := Outcomes {Outcome.FallThrough};
    WHILE (t # NIL) DO
      xc := t.outcomes ();
      (**** DumpOutcome (t, xc); ****)
      oc := oc + xc;
      IF (Outcome.FallThrough IN xc) THEN
        t := t.next;
      ELSE
        t := NIL;
        oc := oc - Outcomes {Outcome.FallThrough};
      END;
    END;
    (*** DumpOutcome (x, oc); ****)
    RETURN oc;
  END GetOutcome;


(***
PROCEDURE DumpOutcome (t: T;  READONLY oc: Outcomes) =
  CONST
    Msg = ARRAY [0..7] OF TEXT {
       "stmt -> {}",
       "stmt -> {FallThrough}",
       "stmt -> {Exit}",
       "stmt -> {FallThrough, Exit}",
       "stmt -> {Return}",
       "stmt -> {FallThrough, Return}",
       "stmt -> {Exit, Return}",
       "stmt -> {FallThrough, Exit, Return}"
    };
  VAR i := 0;  save: INTEGER;
  BEGIN
    IF (Outcome.FallThrough IN oc) THEN INC (i, 1) END;
    IF (Outcome.Exits       IN oc) THEN INC (i, 2) END;
    IF (Outcome.Returns     IN oc) THEN INC (i, 4) END;
    save := Scanner.offset;
    Scanner.offset := t.origin;
    Error.Warn (1, Msg[i]);
    Scanner.offset := save;
  END DumpOutcome;
*****)

BEGIN
END Stmt.
