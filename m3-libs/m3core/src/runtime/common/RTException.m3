(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RTException;

IMPORT RT0, RuntimeError, RTError, M3toC;

TYPE  RTE = RuntimeError.T;
CONST ArgMap = ARRAY BOOLEAN(*raises*) OF RTE {
                   RTE.UnhandledException, RTE.BlockedException };

VAR cur_backstop: Backstop := NIL;

PROCEDURE SetBackstop (h: Backstop): Backstop =
  VAR old := cur_backstop;
  BEGIN
    cur_backstop := h;
    RETURN old;
  END SetBackstop;

PROCEDURE InvokeBackstop (VAR a: RT0.RaiseActivation;
                          raises: BOOLEAN)  RAISES ANY =
  BEGIN
    IF (cur_backstop = NIL)
      THEN DefaultBackstop (a, raises);
      ELSE cur_backstop (a, raises);
    END;
  END InvokeBackstop;

PROCEDURE DefaultBackstop (VAR a: RT0.RaiseActivation;
                           raises: BOOLEAN) RAISES ANY =
  VAR
    err := RuntimeError.Self ();
    arg := LOOPHOLE (ORD (ArgMap [raises]), ADDRESS);
    not_arg := LOOPHOLE (ORD (ArgMap [NOT raises]), ADDRESS);
  BEGIN
    IF (a.exception = err) AND (a.arg = arg) THEN
      (* We're already raising the "Unhandled exception" exception! *)
      Crash (a, raises, err);
    ELSIF (a.exception = err) AND (a.arg = not_arg) THEN
      Crash (a, NOT raises, err);
    ELSE
      a.un_except := a.exception;
      a.un_arg    := a.arg;
      a.exception := err;
      a.arg       := arg;
      Raise (a);
    END;
  END DefaultBackstop;

PROCEDURE Crash (VAR a: RT0.RaiseActivation;  raises: BOOLEAN;  rte: ADDRESS) =
  CONST
    Pre  = ARRAY BOOLEAN OF TEXT { "Unhandled exception: ",  "Exception \"" };
    Post = ARRAY BOOLEAN OF TEXT { "",              "\" not in RAISES list" };
  VAR
    ex    := a.un_except;
    exarg := LOOPHOLE (a.un_arg, INTEGER);
    pre   : TEXT    := NIL;
    ename : TEXT    := NIL;
    post  : TEXT    := NIL;
    file  : ADDRESS := NIL;
  BEGIN
    IF (a.module # NIL) THEN file := a.module.file; END;
    IF (ex # rte) OR (exarg < 0) OR (NUMBER (RTE) <= exarg) THEN
      pre   := Pre[raises];
      ename := M3toC.StoT (LOOPHOLE (ex.name, ADDRESS));
      post  := Post[raises];
    ELSIF (exarg = ORD (RTE.AssertFailed))  AND (a.info0 # NIL) THEN
      pre   := "<*ASSERT*> failed: ";
      ename := LOOPHOLE (a.info0, TEXT);
      post  := "";
    ELSE
      pre   := "";
      ename := RuntimeError.Tag (VAL (exarg, RuntimeError.T));
      post  := "";
    END;
    IF (file = NIL) AND (a.pc # NIL)
      THEN RTError.MsgPC (LOOPHOLE (a.pc, INTEGER), pre, ename, post);
      ELSE RTError.MsgS (file, a.line, pre, ename, post);
    END;
  END Crash;

BEGIN
END RTException.
