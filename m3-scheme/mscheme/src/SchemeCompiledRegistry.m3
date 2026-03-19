(* Copyright (c) 2026 Mika Nystrom.  All rights reserved. *)

MODULE SchemeCompiledRegistry;
IMPORT TextRefTbl;

TYPE Closure = OBJECT proc : Installer END;

VAR tbl := NEW(TextRefTbl.Default).init();

PROCEDURE Register(name : TEXT; installer : Installer) =
  BEGIN EVAL tbl.put(name, NEW(Closure, proc := installer)) END Register;

PROCEDURE Lookup(name : TEXT) : Installer =
  VAR ref : REFANY;
  BEGIN
    IF tbl.get(name, ref) THEN RETURN NARROW(ref, Closure).proc
    ELSE RETURN NIL END
  END Lookup;

BEGIN END SchemeCompiledRegistry.
