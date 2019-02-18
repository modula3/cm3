(* $Id$ *)

MODULE StepFunction;
IMPORT LongRealSeq, LongRealPair, LongrealSetDef, LongrealArraySort;
IMPORT Wr, Fmt, Thread;

<* FATAL Thread.Alerted *>

REVEAL
  Private = T BRANDED Brand & " Private" OBJECT 
    stepCache : LongRealSeq.T := NIL;
  METHODS
    recalcSteps();
  OVERRIDES
    domain := PDomain;
    steps := PSteps;
    iterateSteps := Iterate;
    formatStepsWr := FormatStepsWr;
  END;

PROCEDURE PSteps(t : Private) : LongRealSeq.T =
  BEGIN 
    IF t.stepCache = NIL THEN t.recalcSteps() END;
    RETURN t.stepCache
  END PSteps;

PROCEDURE PDomain(t : T) : LongRealPair.T =
  BEGIN 
    RETURN LongRealPair.T{t.steps().get(0),
                          t.steps().get(t.steps().size()-1)}
  END PDomain;

REVEAL
  Iterator = PubIterator BRANDED Brand & " Iterator" OBJECT
    seq : LongRealSeq.T;
    i := 0;
  OVERRIDES
    next := INext;
  END;

PROCEDURE Iterate(t : T) : Iterator =
  BEGIN RETURN NEW(Iterator, seq := t.steps()) END Iterate;

PROCEDURE INext(iter : Iterator; VAR x : LONGREAL) : BOOLEAN =
  BEGIN
    IF iter.i = iter.seq.size() THEN 
      RETURN FALSE 
    ELSE 
      x := iter.seq.get(iter.i);
      INC(iter.i);
      RETURN TRUE
    END
  END INext;

PROCEDURE FormatStepsWr(t : T; wr : Wr.T) RAISES { Wr.Failure } =
  <* FATAL DomainError *>
  VAR
    iter := t.iterateSteps();
    x : LONGREAL;
  BEGIN
    WHILE iter.next(x) DO
      Wr.PutText(wr,Fmt.LongReal(x));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr,Fmt.LongReal(t.eval(x)));
      Wr.PutChar(wr, '\n')
    END
  END FormatStepsWr;
(**********************************************************************)

TYPE
  BinOp = Private OBJECT
    a, b : T;
  OVERRIDES
    recalcSteps := BinRecalcSteps;
  END;

PROCEDURE BinRecalcSteps(bin : BinOp) =
  VAR 
    s : LONGREAL; 
    ap, bp := 0;
  BEGIN
    WITH asteps = bin.a.steps(),
         bsteps = bin.b.steps(),
         end = MIN(bin.a.domain().k2,bin.b.domain().k2) DO
      s := MAX(asteps.get(0),bsteps.get(0));
      WHILE ap < asteps.size() AND asteps.get(ap) < s DO
        INC(ap)
      END;

      WHILE bp < bsteps.size() AND bsteps.get(bp) < s DO
        INC(bp)
      END;

      (* oh, I'm so lazy *)
      WITH set = NEW(LongrealSetDef.T).init() DO
        WHILE ap < asteps.size() AND asteps.get(ap) <= end DO
          EVAL set.insert(asteps.get(ap)); INC(ap)
        END;
        WHILE bp < bsteps.size() AND bsteps.get(bp) <= end DO
          EVAL set.insert(bsteps.get(bp)); INC(bp)
        END;
        
        WITH arr = NEW(REF ARRAY OF LONGREAL, set.size()),
             iter = set.iterate() DO

          FOR i := FIRST(arr^) TO LAST(arr^) DO
            EVAL iter.next(arr[i])
          END;

          LongrealArraySort.Sort(arr^);
          
          bin.stepCache := NEW(LongRealSeq.T).init();
          FOR i := FIRST(arr^) TO LAST(arr^) DO
            bin.stepCache.addhi(arr[i])
          END
        END
      END
    END      
  END BinRecalcSteps;

PROCEDURE AddEval(b : BinOp; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN b.a.eval(at) + b.b.eval(at) END AddEval;

PROCEDURE Add(a, b : T) : T =
  BEGIN RETURN NEW(BinOp, a := a, b := b, eval := AddEval) END Add;

PROCEDURE Sub(a, b : T) : T =
  BEGIN RETURN Add(a, ScalarMul(-1.0d0, b)) END Sub;
  
PROCEDURE MulEval(b : BinOp; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN b.a.eval(at) * b.b.eval(at) END MulEval;

PROCEDURE Mul(a, b : T) : T =
  BEGIN RETURN NEW(BinOp, a := a, b := b, eval := MulEval) END Mul;
  
PROCEDURE DivEval(b : BinOp; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN b.a.eval(at) / b.b.eval(at) END DivEval;

PROCEDURE Div(a, b : T) : T =
  BEGIN RETURN NEW(BinOp, a := a, b := b, eval := DivEval) END Div;

(**********************************************************************)

TYPE
  UnOp = T OBJECT 
    b : T;
  OVERRIDES
    domain := ADomain;
    steps  := ASteps;
  END;

PROCEDURE ADomain(u : UnOp) : LongRealPair.T = 
  BEGIN RETURN u.b.domain() END ADomain;

PROCEDURE ASteps(u : UnOp) : LongRealSeq.T =
  BEGIN RETURN u.b.steps() END ASteps;

PROCEDURE ScalarMul(a : LONGREAL; b : T) : T =
  BEGIN 
    RETURN NEW(UnOp OBJECT a : LONGREAL END, a := a, b := b, eval := SMEval) 
  END ScalarMul;

PROCEDURE SMEval(u : UnOp OBJECT a : LONGREAL END; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN u.a * u.b.eval(at) END SMEval;

PROCEDURE G(f : Func; b : T) : T =
  BEGIN 
    RETURN NEW(UnOp OBJECT f : Func END, f := f, b := b, eval := GEval)
  END G;

PROCEDURE GEval(u : UnOp OBJECT f : Func END; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN u.f(u.b.eval(at)) END GEval;

PROCEDURE H(e : Evaluator; b : T) : T =
  BEGIN 
    RETURN NEW(UnOp OBJECT e : Evaluator END, e := e, b := b, eval := HEval)
  END H;

PROCEDURE HEval(u : UnOp OBJECT e : Evaluator END; at : LONGREAL) : LONGREAL RAISES { DomainError } =
  BEGIN RETURN u.e.eval(u.b.eval(at)) END HEval;

(**********************************************************************)

TYPE
  OneT = T OBJECT
    from, to : LONGREAL;
  END;

PROCEDURE One(from, to : LONGREAL) : T =
  BEGIN
    RETURN NEW(OneT, from := from, to := to,
               eval := OneEval, domain := PDomain, steps := OneSteps)
  END One;

PROCEDURE OneEval(t : T; at : LONGREAL) : LONGREAL RAISES { DomainError } = 
  BEGIN 
    WITH d = t.domain() DO
      IF at < d.k1 OR at > d.k2 THEN RAISE DomainError(Err{t,at}) END
    END;
    RETURN 1.0d0 
  END OneEval;

PROCEDURE OneSteps(t : OneT) : LongRealSeq.T =
  VAR res := NEW(LongRealSeq.T).init(); BEGIN
    res.addhi(t.from);
    res.addhi(t.to);
    RETURN res
  END OneSteps;

(**********************************************************************)

REVEAL
  Default = PubDefault BRANDED Brand OBJECT
    data : LongRealSeq.T;
    vals : REF ARRAY OF ARRAY OF LONGREAL := NIL;
  METHODS
    finalize() := DefFinalize;
  OVERRIDES
    init := DefInit;
    add  := DefAdd;
    recalcSteps := DefRecalcSteps;
    eval := DefEval;
  END;

PROCEDURE DefInit(def : Default) : Default =
  BEGIN def.data := NEW(LongRealSeq.T).init(); RETURN def END DefInit;

PROCEDURE DefAdd(def : Default; x, y : LONGREAL) =
  BEGIN def.data.addhi(x); def.data.addhi(y) END DefAdd;

PROCEDURE DefFinalize(def : Default) =
  BEGIN
    def.vals := NEW(REF ARRAY OF ARRAY OF LONGREAL, 
                    2,
                    def.data.size() DIV 2);

    FOR i := FIRST(def.vals[1]) TO LAST(def.vals[1]) DO
      def.vals[0,i] := def.data.get(2*i);
      def.vals[1,i] := def.data.get(2*i+1)
    END;

    def.data := NIL
  END DefFinalize;

PROCEDURE DefRecalcSteps(def : Default) =
  BEGIN
    IF def.vals = NIL THEN def.finalize() END;

    def.stepCache := NEW(LongRealSeq.T).init();
    FOR i := FIRST(def.vals[0]) TO LAST(def.vals[0]) DO
      def.stepCache.addhi(def.vals[0,i])
    END
  END DefRecalcSteps;

PROCEDURE Search(READONLY a : ARRAY OF LONGREAL;
                 x : LONGREAL) : CARDINAL =
  (* returns greatest lower bound + 1 of indices of array with abscissa x *)
  VAR
    lo := FIRST(a);
    hi := LAST(a);
  BEGIN
    WHILE lo <= hi DO
      WITH mid = (lo+hi) DIV 2 DO
        IF x <= a[mid] THEN
          hi := mid-1
        ELSE
          lo := mid+1
        END
      END
    END;
    RETURN lo
  END Search;
  
PROCEDURE DefEval(def : Default; at : LONGREAL) : LONGREAL  
  RAISES { DomainError } =

  VAR idx : INTEGER; BEGIN
    IF def.vals = NIL THEN def.finalize() END;

    idx := Search(def.vals[0],at)-1;

    IF idx + 1 <= LAST(def.vals[0]) AND at = def.vals[0,idx+1] THEN
      idx := idx+1
    END;

    (* this is mathematically somewhat sloppy.  The function is defined
       over the range first, last INCLUSIVE *)
    IF idx < 0 OR (idx > LAST(def.vals[1])-1 AND at # def.vals[0,idx]) THEN 
      RAISE DomainError(Err { def, at } ) 
    END;

    RETURN def.vals[1,idx]
  END DefEval;

(**********************************************************************)
PROCEDURE ShiftSteps(a : T; by : LONGREAL) : T =
  <*FATAL DomainError*>
  VAR
    res := NEW(Default).init();
    iter := a.iterateSteps();
    x : LONGREAL;
  BEGIN
    WHILE iter.next(x) DO res.add(x+by,a.eval(x)) END;
    RETURN res
  END ShiftSteps;

PROCEDURE Project(a, on : T) : T =
  <*FATAL DomainError*>
  VAR
    res := NEW(Default).init();
    iter := on.iterateSteps();
    x : LONGREAL;
  BEGIN
    WHILE iter.next(x) DO
      IF x >= a.domain().k1 AND x <= a.domain().k2 THEN
        res.add(x,a.eval(x))
      END
    END;
    RETURN res
  END Project;

BEGIN END StepFunction.

