MODULE PhysicalUnitFmtLex;
(*Copyright (c) 1996, m3na project

1/1/96  <name>    Initial version
*)

IMPORT PhysicalUnit       AS U,
       Fmt                AS F;

<*UNUSED*> CONST Module = "PhysicalUnitFmtLex.";

PROCEDURE Fmt(unit:T):TEXT =
  VAR
    it:=unit.iterate();
    dim:INTEGER;
    exp:U.ExpType;
    res:TEXT:="{";
  BEGIN
    WHILE it.next(dim,exp) DO
      res:=res&"("&F.Int(dim)&","&F.Int(exp)&")";
    END;
    RETURN res&"}";
  END Fmt;

BEGIN
END PhysicalUnitFmtLex.
