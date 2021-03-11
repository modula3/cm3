(* $Id$ *)

GENERIC MODULE SchemeAutoTbl(Tbl, Key, Value);
IMPORT SchemeM3TableOps;
IMPORT Scheme, SchemeBoolean, SchemeLongReal, SchemeSymbol;
FROM SchemeUtils IMPORT First, Second;
IMPORT SchemeUtils;

(* various NOWARN pragmas because depending on the declaration of the
   FromScheme and ToScheme routines, we may or may not get Scheme.E 
   in here.. *)

PROCEDURE Put(obj, arg : Scheme.Object;
              <*UNUSED*>opName : Scheme.Symbol) : Scheme.Object RAISES { Scheme.E } <*NOWARN*> =
  BEGIN
    <* ASSERT ISTYPE(obj, Tbl.T) *>
    WITH tbl = NARROW(obj, Tbl.T),
         hadIt = tbl.put(Key.FromScheme(First(arg)),
                         Value.FromScheme(Second(arg))) DO
      RETURN SchemeBoolean.Truth(hadIt)
    END
  END Put;

PROCEDURE Get(obj, arg : Scheme.Object;
              <*UNUSED*>opName : Scheme.Symbol) : Scheme.Object  RAISES { Scheme.E } <*NOWARN*>=
  VAR
    val : Value.T;
  BEGIN
    <* ASSERT ISTYPE(obj, Tbl.T) *>
    WITH tbl = NARROW(obj, Tbl.T),
         hadIt = tbl.get(Key.FromScheme(arg),
                         val) DO
      IF hadIt THEN
        RETURN SchemeUtils.List2(SchemeBoolean.True(),
                                Value.ToScheme(val))
      ELSE
        RETURN SchemeUtils.List2(SchemeBoolean.False(),
                                NIL)
      END
    END
  END Get;

PROCEDURE Size(obj: Scheme.Object;
               <*UNUSED*>arg : Scheme.Object;
               <*UNUSED*>opName : Scheme.Symbol) : Scheme.Object =
  BEGIN
    <* ASSERT ISTYPE(obj, Tbl.T) *>
    WITH tbl = NARROW(obj, Tbl.T) DO
      RETURN SchemeLongReal.FromI(tbl.size())
    END
  END Size;

PROCEDURE Iterate(obj: Scheme.Object;
               <*UNUSED*>arg : Scheme.Object;
               <*UNUSED*>opName : Scheme.Symbol) : Scheme.Object =
  BEGIN
    <* ASSERT ISTYPE(obj, Tbl.T) *>
    WITH tbl = NARROW(obj, Tbl.T) DO
      RETURN tbl.iterate()
    END
  END Iterate;

PROCEDURE IterNext(obj : Scheme.Object;
                <*UNUSED*>arg : Scheme.Object;
                <*UNUSED*>opName : Scheme.Symbol) : Scheme.Object  RAISES { Scheme.E } <*NOWARN*> =
  BEGIN
    <* ASSERT ISTYPE(obj, Tbl.Iterator) *>
    WITH iter = NARROW(obj, Tbl.Iterator) DO
      VAR
        key : Key.T;
        val : Value.T;
      BEGIN
        WITH res = iter.next(key,val) DO
          IF res THEN
            RETURN SchemeUtils.List3(SchemeBoolean.True(),
                                     Key.ToScheme(key),
                                     Value.ToScheme(val))
          ELSE
            RETURN SchemeUtils.List3(SchemeBoolean.False(),
                                     NIL,
                                     NIL)
          END
        END
      END
    END
  END IterNext;

PROCEDURE Register() =
  BEGIN
    SchemeM3TableOps.Register(tcT, SchemeSymbol.Symbol("put"), Put);
    SchemeM3TableOps.Register(tcT, SchemeSymbol.Symbol("get"), Get);
    SchemeM3TableOps.Register(tcT, SchemeSymbol.Symbol("size"), Size);
    SchemeM3TableOps.Register(tcT, SchemeSymbol.Symbol("iterate"), Iterate);
    SchemeM3TableOps.Register(tcI, SchemeSymbol.Symbol("next"), IterNext);
  END Register;

VAR
  tcT := TYPECODE(Tbl.T);
  tcI := TYPECODE(Tbl.Iterator);
BEGIN END SchemeAutoTbl.
