(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Apr 14 16:30:31 PDT 1996 by heydon                   *)

MODULE RandomImpl;

IMPORT JunoScope, Atom, Random, ExternalProc, JunoArgs, RTVal;
FROM ExternalProc IMPORT Closure, Bind;

VAR (*CONST*)
  Generator := NEW(Random.Default).init();
  ModSym := Atom.FromText("Random");
  Real01Sym := Atom.FromText("Real01");
  RealSym := Atom.FromText("Real");
  IntSym := Atom.FromText("Int");

PROCEDURE New(): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 1);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(ModSym, scp);
    Bind(Real01Sym, NEW(Closure, invoke := Real01Proc), in := 0, out := 1);
    Bind(RealSym,   NEW(Closure, invoke := RealProc),   in := 2, out := 1);
    Bind(IntSym,    NEW(Closure, invoke := IntProc),    in := 2, out := 1);
    RETURN res
  END New;

PROCEDURE Real01Proc(<*UNUSED*> cl: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteValue(1, RTVal.FromReal(Generator.real(0.0, 1.0)));
    RETURN TRUE
  END Real01Proc;

PROCEDURE RealProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    lo := JunoArgs.ReadReal(2, (*INOUT*) err);
    hi := JunoArgs.ReadReal(1, (*INOUT*) err);
  BEGIN
    IF NOT err AND lo < hi THEN
      JunoArgs.WriteValue(3, RTVal.FromReal(Generator.real(lo, hi)));
      RETURN TRUE
    END;
    RETURN FALSE
  END RealProc;

PROCEDURE IntProc(<*UNUSED*> cl: Closure): BOOLEAN =
  VAR
    err := FALSE;
    lo := JunoArgs.ReadInt(2, (*INOUT*) err);
    hi := JunoArgs.ReadInt(1, (*INOUT*) err);
  BEGIN
    IF NOT err AND lo < hi THEN
      JunoArgs.WriteValue(3, RTVal.FromInt(Generator.integer(lo, hi-1)));
      RETURN TRUE
    END;
    RETURN FALSE
  END IntProc;

BEGIN
END RandomImpl.

      
