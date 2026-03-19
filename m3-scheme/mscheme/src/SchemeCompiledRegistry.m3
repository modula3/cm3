(* Copyright (c) 2026 Mika Nystrom.  All rights reserved. *)

MODULE SchemeCompiledRegistry;
IMPORT TextRefTbl, Env;

TYPE Closure = OBJECT proc : Installer END;

VAR tbl := NEW(TextRefTbl.Default).init();
    disabled := FALSE;

PROCEDURE Register(name : TEXT; installer : Installer) =
  BEGIN EVAL tbl.put(name, NEW(Closure, proc := installer)) END Register;

PROCEDURE Unregister(name : TEXT) =
  VAR ref : REFANY;
  BEGIN EVAL tbl.delete(name, ref) END Unregister;

PROCEDURE Lookup(name : TEXT) : Installer =
  VAR ref : REFANY;
  BEGIN
    IF disabled THEN RETURN NIL END;
    IF tbl.get(name, ref) THEN RETURN NARROW(ref, Closure).proc
    ELSE RETURN NIL END
  END Lookup;

PROCEDURE Disable() = BEGIN disabled := TRUE END Disable;
PROCEDURE Enable()  = BEGIN disabled := FALSE END Enable;

BEGIN
  IF Env.Get("MSCHEME_INTERPRETED") # NIL THEN disabled := TRUE END
END SchemeCompiledRegistry.
