(* $Id$ *)

GENERIC MODULE SXFuncOps(Arg, Result);
IMPORT SXClass;
IMPORT XTime AS Time;
FROM SX IMPORT Uninitialized;
IMPORT SX;
IMPORT SXInt;
IMPORT SXIterator;

TYPE 
  Unary = UnaryRoot OBJECT
    f : F1;
  OVERRIDES
    recalc := UnaryRecalc;
  END;

  UnaryRoot = OpResult OBJECT
    a : Arg.T;
  OVERRIDES
    dependsOn := UnaryDepends;
  END;

  BinaryRoot = OpResult OBJECT
    a, b : Arg.T;
  OVERRIDES
    dependsOn := BinaryDepends;
  END;

  Binary = BinaryRoot OBJECT
    f : F2;
  OVERRIDES
    recalc := BinaryRecalc;
  END;

  BinaryShortCircuit = BinaryRoot OBJECT
    f : F2;
    ssOp : Arg.Base;
    ssRes : Result.Base;
  OVERRIDES
    recalc := BinarySSRecalc;
  END;

  NAryRoot = OpResult OBJECT
    a  : REF ARRAY OF Arg.T;
  OVERRIDES
    dependsOn := NAryDepends;
  END;

  NAryLock = NAryRoot OBJECT mu : MUTEX END;

  NAry = NAryLock OBJECT
    f  : FN;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryRecalc;
  END;

  NAryU = NAry OBJECT
    u  : CARDINAL;
  OVERRIDES
    recalc := NAryURecalc;
  END;

  IAry = NAryLock OBJECT
    f  : FI;
    i  : SXInt.T;
    a  : REF ARRAY OF Arg.T;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := IAryRecalc;
  END;

PROCEDURE UnaryDepends(b : UnaryRoot) : SXIterator.T =
  BEGIN RETURN SXIterator.One(b.a) END UnaryDepends;

PROCEDURE BinaryDepends(b : BinaryRoot) : SXIterator.T =
  BEGIN RETURN SXIterator.Two(b.a, b.b) END BinaryDepends;

PROCEDURE NAryDepends(b : NAryRoot) : SXIterator.T =
  VAR
    aa := NEW(REF ARRAY OF SX.T, NUMBER(b.a^));
  BEGIN 
    FOR i := FIRST(aa^) TO LAST(aa^) DO
      aa[i] := b.a[i]
    END;
    RETURN SXIterator.Many(aa^) 
  END NAryDepends;

(**********************************************************************)

PROCEDURE UnaryRecalc(u : Unary; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      RETURN u.update(u.f(u.a.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END UnaryRecalc;

PROCEDURE BinaryRecalc(b : Binary; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      RETURN b.update(b.f(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinaryRecalc;

PROCEDURE BinarySSRecalc(b : BinaryShortCircuit; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      IF b.a.value() = b.ssOp THEN RETURN b.update(b.ssRes, when) END
    EXCEPT
      Uninitialized => (* skip *)
    END;

    TRY
      IF b.b.value() = b.ssOp THEN RETURN b.update(b.ssRes, when) END
    EXCEPT
      Uninitialized => (* skip *)
    END;

    TRY
      RETURN b.update(b.f(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinarySSRecalc;

PROCEDURE NAryRecalc(n : NAry; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.f(n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END NAryRecalc;      

PROCEDURE NAryURecalc(n : NAryU; when : Time.T) : BOOLEAN =
  <*FATAL SX.Uninitialized*>
  VAR u := 0;
  BEGIN
    (* prepare av *)
    LOCK n.mu DO
      FOR i := FIRST(n.a^) TO LAST(n.a^) DO
        (* note that we could get SX.Uninitialized here if we use
           .uninitialize on the input SXs. 
           
           should we lock n.a?
        *)
        IF n.a[i].initialized() THEN
          n.av[i-u] := n.a[i].value()
        ELSIF u = n.u THEN
          RETURN FALSE
        ELSE
          INC(u)
        END;
      END;
      RETURN n.update(n.f(SUBARRAY(n.av^,0,NUMBER(n.a^)-u)),when)
    END
  END NAryURecalc;      

PROCEDURE IAryRecalc(n : IAry; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.f(n.i.value(),n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END IAryRecalc;      

(**********************************************************************)

PROCEDURE UnaryFunc(a : Arg.T; f : F1; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(Unary, opName := opName, f := f, a := a).init() DO
      SX.Lock(SX.Array { a });
      TRY
        EVAL UnaryRecalc(res, a.updated);
        a.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a })
      END;
      RETURN res
    END
  END UnaryFunc;

PROCEDURE BinaryFunc(a, b : Arg.T; f : F2; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(Binary, opName := opName, f := f, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinaryRecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
      RETURN res
    END
  END BinaryFunc;

PROCEDURE BinarySymmetricShortCircuitFunc(a, b : Arg.T; f : F2; 
                                          ssOp : Arg.Base; ssRes : Result.Base;
                                          opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(BinaryShortCircuit, 
                   opName := opName, 
                   ssOp := ssOp, ssRes := ssRes,
                   f := f, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinarySSRecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
      RETURN res
    END
  END BinarySymmetricShortCircuitFunc;

PROCEDURE NAryFunc(READONLY a : ARRAY OF Arg.T; f : FN; opName : TEXT := NIL) : Result.T =
  BEGIN
    RETURN NAryFuncs(NEW(NAry),a,f,opName)
  END NAryFunc;

<*UNUSED*>
PROCEDURE NAryUFunc(READONLY a : ARRAY OF Arg.T; f : FN; un : CARDINAL; opName : TEXT := NIL) : Result.T =
  BEGIN
    RETURN NAryFuncs(NEW(NAryU, u := un),a,f,opName)
  END NAryUFunc;

PROCEDURE NAryFuncs(res : NAry; READONLY a : ARRAY OF Arg.T; f : FN; opName : TEXT := NIL) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH sa = NEW(REF SX.Array, NUMBER(a))^ DO

      res.opName := opName;
      res.mu := NEW(MUTEX);
      res.f := f;
      res.a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a));
      res.av := NEW(REF ARRAY OF Arg.Base, NUMBER(a));
      
      EVAL res.init();
      
      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;

      SX.Lock(sa);
      TRY
        EVAL res.recalc(max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END NAryFuncs;

PROCEDURE IAryFunc(int : SXInt.T;
                   READONLY a : ARRAY OF Arg.T; f : FI;
                   opName : TEXT) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH res = NARROW(NEW(IAry, 
                          opName := opName,
                          mu := NEW(MUTEX),
                          i := int,
                          f := f, 
                          a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a)), 
                          av := NEW(REF ARRAY OF Arg.Base, NUMBER(a))).init(),
                      IAry),
         sa = NEW(REF SX.Array, NUMBER(a)+1)^ DO
      
      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;
      sa[LAST(sa)] := int;

      SX.Lock(sa);
      TRY
        EVAL IAryRecalc(res, max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END IAryFunc;

(**********************************************************************)

TYPE 
  UnaryO = UnaryRoot OBJECT
    o : O1;
  OVERRIDES
    recalc := UnaryORecalc;
  END;

  BinaryO = BinaryRoot OBJECT
    o : O2;
  OVERRIDES
    recalc := BinaryORecalc;
  END;

  NAryO = NAryLock OBJECT
    o : ON;
    av : REF ARRAY OF Arg.Base; (* temporary storage, allocated once only *)
  OVERRIDES
    recalc := NAryORecalc;
  END;

  NAryOU = NAryO OBJECT
    u  : CARDINAL;
  OVERRIDES
    recalc := NAryOURecalc;
  END;

PROCEDURE UnaryORecalc(u : UnaryO; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      RETURN u.update(u.o.op(u.a.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END UnaryORecalc;

PROCEDURE BinaryORecalc(b : BinaryO; when : Time.T) : BOOLEAN = 
  BEGIN
    TRY
      RETURN b.update(b.o.op(b.a.value(),b.b.value()),when)
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END BinaryORecalc;

PROCEDURE NAryORecalc(n : NAryO; when : Time.T) : BOOLEAN =
  BEGIN
    TRY
      (* prepare av *)
      LOCK n.mu DO
        FOR i := FIRST(n.a^) TO LAST(n.a^) DO
          n.av[i] := n.a[i].value()
        END;
        RETURN n.update(n.o.op(n.av^),when)
      END
    EXCEPT
      Uninitialized  => RETURN FALSE (* skip -- don't initialize me yet *)
    END
  END NAryORecalc;      

PROCEDURE NAryOURecalc(n : NAryOU; when : Time.T) : BOOLEAN =
  <*FATAL SX.Uninitialized*>
  VAR u := 0;
  BEGIN
    (* prepare av *)
    LOCK n.mu DO
      FOR i := FIRST(n.a^) TO LAST(n.a^) DO
        (* note that we could get SX.Uninitialized here if we use
           .uninitialize on the input SXs. 
           
           should we lock n.a?
        *)
        IF n.a[i].initialized() THEN
          n.av[i-u] := n.a[i].value()
        ELSIF u = n.u THEN
          RETURN FALSE
        ELSE
          INC(u)
        END;
      END;
      RETURN n.update(n.o.op(SUBARRAY(n.av^,0,NUMBER(n.a^)-u)),when)
    END
  END NAryOURecalc;      

PROCEDURE UnaryOFunc(a : Arg.T; o : O1; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(UnaryO, opName := opName, o := o, a := a).init() DO
      SX.Lock(SX.Array { a });
      TRY
        EVAL UnaryORecalc(res, a.updated);
        a.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a })
      END;
      RETURN res
    END
  END UnaryOFunc;

PROCEDURE BinaryOFunc(a, b : Arg.T; o : O2; opName : TEXT := NIL) : Result.T =
  BEGIN 
    WITH res = NEW(BinaryO, opName := opName, o := o, a := a, b := b).init() DO
      SX.Lock( SX.Array { a, b }) ;
      TRY
        EVAL BinaryORecalc(res, MAX(a.updated,b.updated));
        a.depends(res); b.depends(res)
      FINALLY
        SX.Unlock(SX.Array { a,b })
      END;
      RETURN res
    END
  END BinaryOFunc;

PROCEDURE NAryOFunc(READONLY a : ARRAY OF Arg.T; o : ON; opName : TEXT := NIL) : Result.T =
  BEGIN RETURN NAryOFuncs(NEW(NAryO), a, o, opName) END NAryOFunc;

PROCEDURE NAryOUFunc(READONLY a : ARRAY OF Arg.T; o : ON; un : CARDINAL; opName : TEXT := NIL) : Result.T =
  BEGIN RETURN NAryOFuncs(NEW(NAryOU, u := un), a, o, opName) END NAryOUFunc;

PROCEDURE NAryOFuncs(res : NAryO; READONLY a : ARRAY OF Arg.T; o : ON; opName : TEXT := NIL) : Result.T =
  VAR 
    max := FIRST(Time.T);
  BEGIN 
    WITH sa = NEW(REF SX.Array, NUMBER(a))^ DO

      res.opName := opName;
      res.mu := NEW(MUTEX);
      res.o := o;
      res.a  := NEW(REF ARRAY OF Arg.T,    NUMBER(a));
      res.av := NEW(REF ARRAY OF Arg.Base, NUMBER(a));

      EVAL res.init();

      FOR i := FIRST(a) TO LAST(a) DO
        sa[i] := a[i];
        res.a[i] := a[i]; 
        max := MAX(max, a[i].updated)
      END;

      SX.Lock(sa);
      TRY
        EVAL NAryORecalc(res, max);
        FOR i := FIRST(a) TO LAST(a) DO
          a[i].depends(res);
        END
      FINALLY
        SX.Unlock(sa)
      END;

      RETURN res
    END
  END NAryOFuncs;

BEGIN END SXFuncOps.
