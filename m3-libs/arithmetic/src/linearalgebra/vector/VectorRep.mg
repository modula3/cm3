GENERIC MODULE VectorRep(R, V);
(*Arithmetic for Modula-3, see doc for details *)
FROM NADefinitions IMPORT Error;

<*UNUSED*>
CONST Module = "VectorRep.";

PROCEDURE Clear (VAR z: T) =
  BEGIN
    FOR i := 0 TO LAST(z) DO z[i] := R.Zero; END;
  END Clear;

PROCEDURE Apply (READONLY x: T; f: V.ApplyFtn) RAISES {Error} =
  BEGIN
    FOR j := 0 TO LAST(x) DO f(x[j]); END;
  END Apply;

PROCEDURE Map (READONLY x: T; f: V.MapFtn): V.T RAISES {Error} =
  VAR z:=NEW(V.T,NUMBER(x)) ;
  BEGIN
    FOR j := 0 TO LAST(x) DO z[j] := f(x[j]); END;
    RETURN z;
  END Map;

PROCEDURE Reduce (READONLY x: T; f: V.ReduceFtn; accu: R.T): R.T RAISES {Error} =
  BEGIN
    FOR j := 0 TO LAST(x) DO accu := f(accu, x[j]); END;
    RETURN accu;
  END Reduce;


(*-----------------*)
BEGIN
END VectorRep.
