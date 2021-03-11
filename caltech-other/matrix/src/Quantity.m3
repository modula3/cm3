(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
MODULE Quantity;
IMPORT RefSet,RefSetList AS RefSetDef ,Fmt;
IMPORT Math; (* exp, log, ... *)
IMPORT Debug;

TYPE
  Op = { Sum, Product, Difference, Quotient, Value,  Constant,
         Logarithm, Exponential, Sqrt,   Sqr };
CONST
  OpName = ARRAY Op OF TEXT 
       { "+", "*",     "-",        "/",      "eval", "const", 
         "log",     "exp",       "sqrt", "sqr" }; 

REVEAL 
  T = Public BRANDED "quantity" OBJECT
    mark : BOOLEAN := FALSE;
    type : Op;
    l , r : T; (* tree structure here, either REF LONGREAL or T *)
    base : REF LONGREAL;
    name : TEXT := NIL; (* only valid for Op.Value *)
  METHODS
    init(type : Op; l : T := NIL; r : T := NIL; 
         base : REF LONGREAL := NIL ; name : TEXT := NIL) : T      := Init;
  OVERRIDES
    value := Value;
    derivative := Derivative;
    variables := Variables;
    format := Format;
    set := Set;
  END;

(* should do generic Set, and specific LongRealSet *)

PROCEDURE Variables(self : T) : RefSet.T RAISES { Recursive } =
  BEGIN
    TRY
      Lock(self);
      (* extract all the variables in an expression *)
      IF self = NIL THEN RETURN NEW(RefSetDef.T).init() END;
      
      CASE self.type OF
      | Op.Sum,Op.Product,Op.Difference,Op.Quotient,
        Op.Exponential,Op.Logarithm, Op.Sqrt, Op.Sqr =>            
        RETURN self.l.variables().union(self.r.variables())
      | Op.Value =>          
        VAR 
          res := NEW(RefSetDef.T).init(); 
        BEGIN 
          EVAL res.insert(self.base); 
          RETURN res 
        END
      | Op.Constant =>       RETURN NEW(RefSetDef.T).init()
      ELSE
        <* ASSERT FALSE *>
      END;
    FINALLY
      Unlock(self);
    END
  END Variables;

PROCEDURE Lock(self : T) RAISES { Recursive } =
  BEGIN IF self.mark THEN RAISE Recursive END; self.mark := TRUE END Lock;

PROCEDURE Unlock(self : T) =
  BEGIN <* ASSERT self.mark *> self.mark := FALSE END Unlock;

PROCEDURE Value(self: T) : LONGREAL RAISES { Recursive, IllegalOperands } = 
  BEGIN
    TRY
      Lock(self);
      CASE self.type OF
      | Op.Sum =>            RETURN self.l.value() + self.r.value();
      | Op.Product =>        RETURN self.l.value() * self.r.value();
      | Op.Difference =>     RETURN self.l.value() - self.r.value();
      | Op.Quotient =>       
        IF self.r.value() = 0.0d0 THEN
          RAISE IllegalOperands(self.r)
        ELSE
          (* 
          Debug.Out("Quotient ((" & self.l.format() & ")=" & 
            Fmt.LongReal(self.l.value())
          & "/ (" & self.r.format() & ")=" & Fmt.LongReal(self.r.value()) & 
            ") = " & Fmt.LongReal(self.l.value() / 
                                  self.r.value()));
          *)
          RETURN self.l.value() / self.r.value() 
        END
      | Op.Sqr => RETURN self.l.value() * self.l.value()
      | Op.Exponential =>    RETURN Math.exp(self.l.value())
      | Op.Logarithm =>      
        IF self.l.value() <= 0.0d0 THEN RAISE IllegalOperands(self.l)
        ELSE RETURN Math.log(self.l.value()) END
      | Op.Sqrt =>
        IF self.l.value() <= 0.0d0 THEN RAISE IllegalOperands(self.l)
        ELSE RETURN Math.exp(0.5d0*Math.log(self.l.value())) END
      | Op.Value =>          RETURN self.base^
      | Op.Constant =>       RETURN self.base^
      ELSE
        <* ASSERT FALSE *>
      END;
    FINALLY
      Unlock(self)
    END
  END Value;

PROCEDURE Format(self : T; printValues : BOOLEAN) : TEXT RAISES { Recursive } =
  BEGIN
    TRY
      Lock(self);
      CASE self.type OF
      | Op.Constant => RETURN Fmt.LongReal(self.base^);
      | Op.Value => 
        IF printValues THEN RETURN self.name & "(=" & 
          Fmt.LongReal(self.base^, style := Fmt.Style.Sci, prec := 3) & ")"
        ELSE 
          RETURN self.name;
        END;
      ELSE
        VAR 
          res := "(" & OpName[self.type] & " " & self.l.format(printValues);
        BEGIN
          IF self.r # NIL THEN 
            res := res & " " & self.r.format(printValues);
          END;
          res := res & ")";
          RETURN res;
        END;
      END; (* CASE self.type *)
    FINALLY
      Unlock(self);
    END;
  END Format;

PROCEDURE Set(self : T; value : LONGREAL) =
  BEGIN
    <* ASSERT self.type = Op.Value *>
    self.base^ := value
  END Set;

PROCEDURE Init(self: T; type : Op; l : T; r : T; base : REF LONGREAL := NIL ;
               name : TEXT) 
  : T =
  BEGIN
    self.type := type;
    self.l := l;
    self.r := r;
    self.base := base; (* not used for anything but Op.Value *)
    self.name := name; (* ditto *)
    RETURN self
  END Init;

PROCEDURE Derivative(self: T; wrt : REF LONGREAL) : Public RAISES { Recursive } =
  BEGIN
    TRY
      Lock(self);
      CASE self.type OF
      | Op.Sum =>            RETURN Add(self.l.derivative(wrt),
                                        self.r.derivative(wrt));
      | Op.Product =>        RETURN Add(Mul(self.l.derivative(wrt),self.r),
                                        Mul(self.l,self.r.derivative(wrt)));
      | Op.Difference =>     RETURN Sub(self.l.derivative(wrt),
                                        self.r.derivative(wrt));
      | Op.Quotient =>       RETURN Div(Sub(Mul(self.l.derivative(wrt),self.r),
                                            Mul(self.l,self.r.derivative(wrt))),
                                        Square(self.r));
      | Op.Sqrt =>           RETURN Div(self.l.derivative(wrt),
                                        Mul(Constant(2.0d0),self))
      | Op.Exponential =>    RETURN Mul(self,self.l.derivative(wrt));
      | Op.Sqr =>            RETURN Mul(Mul(Constant(2.0d0),self.l),
                                        self.l.derivative(wrt))
      | Op.Logarithm =>      RETURN Mul(Div(One,self.l),self.l.derivative(wrt))
      | Op.Value =>         
        IF self.base = wrt THEN RETURN One ELSE RETURN Zero END
      | Op.Constant =>       RETURN Zero
      END;
    FINALLY
      Unlock(self);
    END;
  END Derivative;
      
PROCEDURE Exp(x:T) : T =
  BEGIN RETURN NEW(T).init(Op.Exponential,x) END Exp;

PROCEDURE Log(x:T) : T =
  BEGIN RETURN NEW(T).init(Op.Logarithm,x) END Log;

PROCEDURE Sqrt(x:T) : T =
  BEGIN 
    IF x.type = Op.Sqr THEN RETURN x.l ELSE RETURN NEW(T).init(Op.Sqrt,x) END
  END Sqrt;

PROCEDURE Mul(x,y:T) : T =
  BEGIN 
    IF Equivalent(Zero,x) OR Equivalent(Zero,y) THEN RETURN Zero
    ELSIF Equivalent(x,y) THEN RETURN Square(x)
    ELSIF Equivalent(One,x) THEN RETURN y
    ELSIF Equivalent(One,y) THEN RETURN x
    ELSE RETURN NEW(T).init(Op.Product,x,y) 
    END
  END Mul;

PROCEDURE Add(x,y:T) : T =
  BEGIN  
    IF    Equivalent(Zero,x) THEN RETURN y
    ELSIF Equivalent(Zero,y) THEN RETURN x
    ELSE  RETURN NEW(T).init(Op.Sum,x,y) 
    END
  END Add;

PROCEDURE Neg(x : T) : T = BEGIN RETURN Sub(Zero,x) END Neg;

PROCEDURE Sub(x,y:T) : T =
  BEGIN 
    IF Equivalent(x,y) THEN RETURN Zero
    ELSIF Equivalent(y,Zero) THEN RETURN x
    ELSE RETURN NEW(T).init(Op.Difference,x,y) 
    END
  END Sub;

PROCEDURE Div(x,y:T) : T =
  BEGIN 

    (* 0/x *)
    IF Equivalent(x,Zero) THEN RETURN Zero

    (* x/x *)
    ELSIF Equivalent(x,y) THEN RETURN One
      
    (* 1/(x/y) *)
    ELSIF Equivalent(x,One) AND y.type = Op.Quotient THEN 
      Debug.Out("Simplifying div 1/(x/y)...");
      RETURN NEW(T).init(Op.Quotient,y.r,y.l)

    ELSE RETURN NEW(T).init(Op.Quotient,x,y) END 
  END Div;

PROCEDURE Equivalent(x,y : T) : BOOLEAN =
  (* returns true if x and y are ALWAYS the same *)
  <* FATAL Recursive, IllegalOperands *>
  BEGIN
    IF x = y OR 
      x.type = Op.Constant AND y.type = Op.Constant AND 
      y.value() = x.value() THEN
      RETURN TRUE
    ELSE RETURN FALSE
    END
  END Equivalent;

VAR
  anonymousVars := 0;

PROCEDURE Variable(x : LONGREAL; name : TEXT := NIL) : T =
  VAR
    storage := NEW(REF LONGREAL);
  BEGIN
    storage^ := x;
    IF name = NIL THEN
      name := "anon" & Fmt.Int(anonymousVars);
      anonymousVars := anonymousVars + 1;
    END;
    RETURN NEW(T).init(Op.Value,base := storage,name := name);
  END Variable;

PROCEDURE Constant(x : LONGREAL) : T =
  VAR
    refLongreal := NEW(REF LONGREAL);
  BEGIN
    refLongreal^ := x;
    RETURN NEW(T).init(Op.Constant,base := refLongreal) 
  END Constant;

(* compound ops *)

PROCEDURE Square(x:T) : T = BEGIN RETURN NEW(T).init(Op.Sqr,x) END Square;

PROCEDURE Pow(x,p:T) : T = BEGIN RETURN Exp(Mul(p,Log(x))) END Pow;

BEGIN
  Zero := Constant(0.0d0);
  One  := Constant(1.0d0);
END Quantity.













