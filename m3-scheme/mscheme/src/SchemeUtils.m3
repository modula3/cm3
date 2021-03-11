(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeUtils;
IMPORT Scheme, SchemeInputPort, SchemeClass, SchemeSymbol;
IMPORT Wr, Fmt, Wx, Stdio;
FROM Scheme IMPORT Object, E, Symbol, Vector;
FROM SchemeChar IMPORT Char;
IMPORT SchemeLongReal, SchemeChar, SchemePair;
IMPORT AL;
IMPORT Thread;
IMPORT SchemeBoolean;
IMPORT SchemeProcedure,SchemeProcedureClass;
IMPORT Debug;
IMPORT RefSeq, RefPair, RefPairSeq;
IMPORT Scan;
IMPORT Lex, FloatMode;
IMPORT SchemeEnvironmentBinding;
IMPORT SchemeConvertHooks;

TYPE Boolean = SchemeBoolean.T;
     LongReal = SchemeLongReal.T;

<* FATAL Thread.Alerted *>

(* the return Str(Error...) is really quite bizarre; also in the Sym
   and Vec routines below... *)

PROCEDURE StringifyT(x : Object) : TEXT RAISES { E } =
  BEGIN RETURN StringifyQ(x, FALSE) END StringifyT;

PROCEDURE CheckNonNil(x : Object) : Object RAISES { E } =
  BEGIN
    IF x # NIL THEN RETURN x 
    ELSE RETURN Error("NIL object in context requiring non-NIL object")
    END
  END CheckNonNil;

PROCEDURE Str(x : Object) : String RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,String) THEN RETURN x 
    ELSE RETURN Str(Error("expected a string, got: " & StringifyT(x)))
    END
  END Str;

PROCEDURE Sym(x : Object) : Symbol RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,Symbol) THEN RETURN x 
    ELSE RETURN Sym(Error("expected a symbol, got: " & StringifyT(x)))
    END
  END Sym;

PROCEDURE Vec(x : Object) : Vector RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,Vector) THEN RETURN x 
    ELSE RETURN Vec(Error("expected a vector, got: " & StringifyT(x)))
    END
  END Vec;

PROCEDURE InPort(x : Object; interp : Scheme.T) : SchemeInputPort.T RAISES { E } =
  BEGIN
    IF x = NIL THEN RETURN interp.input END;
    IF ISTYPE(x,SchemeInputPort.T) THEN RETURN x 
    ELSE 
      RETURN InPort(Error("expected a schemeInputPort, got: " & 
                          StringifyT(x)), interp)
    END
  END InPort;

PROCEDURE OutPort(x : Object; interp : Scheme.T) : Wr.T RAISES { E } =
  BEGIN
    IF x = NIL THEN RETURN interp.output END;
    IF ISTYPE(x,Wr.T) THEN RETURN x 
    ELSE 
      RETURN OutPort(Error("expected an output port, got: " & 
                           StringifyT(x)), interp)
    END
  END OutPort;

PROCEDURE Error(message : TEXT) : Object RAISES { E } =
  BEGIN
(*    TRY Wr.PutText(Stdio.stderr, "**** ERROR: " & message) EXCEPT ELSE END;*)
    RAISE E(message)
  END Error;

PROCEDURE Warn(message : TEXT) : Object RAISES { E } =
  BEGIN
    IF warningsAreErrors THEN
      RAISE E("WARNING: " & message)
    ELSE
      Debug.Warning("**** " & message & "\n") ;
      RETURN "<warn>"
    END
  END Warn;

VAR warningsAreErrors := FALSE;

PROCEDURE SetWarningsAreErrors(to : BOOLEAN) = 
  BEGIN warningsAreErrors := to END SetWarningsAreErrors;

PROCEDURE First(x : Object) : Object =
  BEGIN
    TYPECASE x OF 
      NULL => RETURN NIL
    |
      Pair(p) => RETURN p.first 
    ELSE 
      RETURN NIL 
    END
  END First;

PROCEDURE Rest(x : Object) : Object =
  BEGIN
    IF x = NIL THEN RETURN NIL END;

    TYPECASE x OF Pair(p) => RETURN p.rest ELSE RETURN NIL END
  END Rest;

PROCEDURE Second(x : Object) : Object =
  BEGIN RETURN First(Rest(x)) END Second;

PROCEDURE Third(x : Object) : Object = 
  BEGIN RETURN First(Rest(Rest(x))) END Third;

PROCEDURE Fourth(x : Object) : Object = 
  BEGIN RETURN First(Rest(Rest(Rest(x)))) END Fourth;

PROCEDURE Fifth(x : Object) : Object = 
  BEGIN RETURN First(Rest(Rest(Rest(Rest(x))))) END Fifth;

PROCEDURE PedanticFirst(x : Object) : Object RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      RETURN NARROW(x,Pair).first
    ELSE 
      RETURN Error("Attempt to car of a non-Pair:" & Stringify(x)) 
    END
  END PedanticFirst;

PROCEDURE PedanticRest(x : Object) : Object RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      RETURN NARROW(x,Pair).rest
    ELSE 
      RETURN Error("Attempt to cdr of a non-Pair:" & Stringify(x)) 
    END
  END PedanticRest;

PROCEDURE SetFirst(x, y : Object) : Object RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      NARROW(x,Pair).first := y; RETURN y
    ELSE 
      RETURN Error("Attempt to set-car of a non-Pair:" & Stringify(x)) 
    END
  END SetFirst;

PROCEDURE SetRest(x, y : Object) : Object RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, Pair) THEN 
      NARROW(x,Pair).rest := y; RETURN y
    ELSE 
      RETURN Error("Attempt to set-cdr of a non-Pair:" & Stringify(x)) 
    END
  END SetRest;

PROCEDURE List1(x : Object; t : Scheme.T := NIL) : Pair =
  VAR res : Pair; BEGIN 
    IF t = NIL THEN res := NEW(Pair) ELSE res := SchemeClass.GetCons(t) END;
    res.first := x; res.rest := NIL;
    RETURN res
  END List1;

PROCEDURE MakeList(READONLY a : ARRAY OF Object; t : Scheme.T := NIL) : Pair =
  VAR res : Pair := NIL;
  BEGIN
    IF t = NIL THEN
      FOR i := LAST(a) TO FIRST(a) BY -1 DO
        res := NEW(Pair, first := a[i], rest := res)
      END
    ELSE
      FOR i := LAST(a) TO FIRST(a) BY -1 DO
        WITH pair = SchemeClass.GetCons(t) DO
          pair.first := a[i]; pair.rest := res;
          res := pair
        END
      END
    END;
    RETURN res
  END MakeList;

PROCEDURE List2(x, y : Object; t : Scheme.T := NIL) : Pair =
  VAR
    p1, p2 : Pair;
  BEGIN 
    IF t = NIL THEN 
      p1 := NEW(Pair);
      p2 := NEW(Pair)
    ELSE
      p1 := SchemeClass.GetCons(t);
      p2 := SchemeClass.GetCons(t)
    END;
    p1.first := x; p2.first := y; 
    p1.rest := p2; p2.rest := NIL;
    
    RETURN p1
  END List2;

PROCEDURE List3(x, y, z : Object; t : Scheme.T := NIL) : Pair =
  VAR
    p1, p2, p3 : Pair;
  BEGIN 
    IF t = NIL THEN 
      p1 := NEW(Pair);
      p2 := NEW(Pair);
      p3 := NEW(Pair)
    ELSE
      p1 := SchemeClass.GetCons(t);
      p2 := SchemeClass.GetCons(t);
      p3 := SchemeClass.GetCons(t)
    END;
    p1.first := x; p2.first := y; p3.first := z;
    p1.rest := p2; p2.rest := p3; p3.rest := NIL;
    
    RETURN p1
  END List3;

PROCEDURE List4(x, y, z, u : Object; t : Scheme.T := NIL) : Pair =
  VAR
    p1, p2, p3, p4 : Pair;
  BEGIN 
    IF t = NIL THEN 
      p1 := NEW(Pair);
      p2 := NEW(Pair);
      p3 := NEW(Pair);
      p4 := NEW(Pair)
    ELSE
      p1 := SchemeClass.GetCons(t);
      p2 := SchemeClass.GetCons(t);
      p3 := SchemeClass.GetCons(t);      
      p4 := SchemeClass.GetCons(t)
    END;
    p1.first := x; p2.first := y; p3.first := z; p4.first := u;
    p1.rest := p2; p2.rest := p3; p3.rest := p4; p4.rest := NIL;
    
    RETURN p1
  END List4;

PROCEDURE ListStar(x : Object; t : Scheme.T := NIL) : Object =
  BEGIN
    IF Rest(x) = NIL THEN RETURN First(x) 
    ELSE RETURN Cons(First(x), ListStar(Rest(x), t), t)
    END
  END ListStar;
  
PROCEDURE Cons(a, b : Object; t : Scheme.T := NIL) : Pair = 
  VAR pair : Pair;
  BEGIN 
    IF t = NIL THEN pair := NEW(Pair) ELSE pair := SchemeClass.GetCons(t) END;
    pair.first := a;
    pair.rest := b;
    RETURN pair
  END Cons;

PROCEDURE Reverse(x : Object) : Object =
  VAR result : Object := NIL;
  BEGIN
    WHILE x # NIL AND ISTYPE(x,Pair) DO
      result := Cons(First(x), result); 
      x := Rest(x)
    END;
    RETURN result
  END Reverse;

PROCEDURE Equal(x, y : Object; stack : RefPairSeq.T := NIL) : BOOLEAN =

  PROCEDURE HaveSeen(x : Object; VAR y : Object) : BOOLEAN =
    BEGIN
      FOR i := 0 TO stack.size()-1 DO
        WITH entry = stack.get(i) DO
          IF entry.k1 = x THEN y := entry.k2; RETURN TRUE END
        END
      END;
      RETURN FALSE
    END HaveSeen;

  PROCEDURE Push(x, y : Object) =
    BEGIN
      stack.addhi(RefPair.T {x, y})
    END Push;

  PROCEDURE Pop() = BEGIN EVAL stack.remhi() END Pop;

  BEGIN
    IF stack = NIL THEN stack := NEW(RefPairSeq.T).init() END;

    IF x = y THEN RETURN TRUE END;
    
    VAR seenY : Object;
    BEGIN
      IF HaveSeen(x, seenY) THEN 
        RETURN y = seenY 
      ELSE
        Push(x, y)
      END
    END;

    (* if we get here, last thing was Push *)
    TRY
      IF x = NIL OR y = NIL THEN 
        RETURN x = y
      ELSE
        TYPECASE x OF
          Pair(p) =>
          IF NOT ISTYPE(y,Pair) THEN RETURN FALSE END;
          WITH that = NARROW(y,Pair) DO
            RETURN Equal(p.first,that.first,stack) AND Equal(p.rest,that.rest,stack)
          END
        |
          Vector(vx) =>
          TYPECASE y OF 
            Vector(vy) =>
            IF NUMBER(vx^) # NUMBER(vy^) THEN RETURN FALSE END;
            FOR i := FIRST(vx^) TO LAST(vx^) DO
              IF Equal(vx[i],vy[i],stack) THEN RETURN FALSE END
            END;
            RETURN TRUE
          ELSE (* y not vector *)
            RETURN FALSE
          END
        ELSE
          RETURN EqualLeaf(x, y)
        END
      END
    FINALLY
      Pop()
    END
  END Equal;

PROCEDURE EqualLeaf(x, y : Object) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      LongReal(lr) =>
      IF NOT ISTYPE(y,LongReal) THEN RETURN FALSE END;
      RETURN lr^ = NARROW(y,LongReal)^
    |
      String(sx) =>
      TYPECASE y OF 
        String(sy) =>
        IF NUMBER(sx^) # NUMBER(sy^) THEN RETURN FALSE END;
        FOR i := FIRST(sx^) TO LAST(sx^) DO
          IF sx[i] # sy[i] THEN RETURN FALSE END
        END;
        RETURN TRUE
      ELSE (* y not string *)
        RETURN FALSE
      END
    ELSE
      RETURN x = y (* right? *)
    END
  END EqualLeaf;

PROCEDURE Eqv(x, y : Object) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      NULL => RETURN x = y
    |
      SchemeLongReal.T(lx) => 
      TYPECASE y OF SchemeLongReal.T(ly) => RETURN lx^ = ly^ ELSE RETURN FALSE END
      (* chars are shared in our system, no need to check values here *)
    ELSE
      RETURN x = y
    END
  END Eqv;

PROCEDURE Length(x : Object) : CARDINAL =
  VAR len := 0;
  BEGIN
    WHILE x # NIL AND ISTYPE(x,Pair) DO INC(len); x := Rest(x) END;
    RETURN len
  END Length;

PROCEDURE Nth(x : Object; n : CARDINAL) : Object =
  BEGIN
    WHILE n > 0 DO x := Rest(x); DEC(n) END;
    RETURN First(x)
  END Nth;

PROCEDURE ListToString(chars: Object) : String RAISES { E } =
  VAR str := NEW(String, Length(chars));
      i := 0;
  BEGIN
    WHILE chars # NIL AND ISTYPE(chars,Pair) DO
      str[i] := Char(First(chars));
      chars := Rest(chars);
      INC(i)
    END;
    RETURN str
  END ListToString;

PROCEDURE ListToVector(objs : Object) : Vector =
  VAR vec := NEW(Vector, Length(objs));
      i := 0;
  BEGIN
    WHILE objs # NIL AND ISTYPE(objs,Pair) DO
      vec[i] := First(objs);
      objs := Rest(objs);
      INC(i)
    END;
    RETURN vec
  END ListToVector;

PROCEDURE Write(x : Object; 
                port : Wr.T; 
                quoted : BOOLEAN;
                interp : Scheme.T := NIL;
                flush := TRUE) : Object RAISES { E } =
  BEGIN
    TRY
      IF port = NIL OR Wr.Closed(port) THEN
        RETURN Error("Write: port NIL or closed")
      END;

      IF interp = NIL OR interp.wx = NIL THEN
        Wr.PutText(port, StringifyQ(x, quoted))
      ELSE
        StringifyB(x, quoted, interp.wx, interp.refseq);
        Wr.PutText(port, Wx.ToText(interp.wx))
      END;
      IF flush THEN
        Wr.Flush(port)
      END;
      RETURN x
    EXCEPT
      Wr.Failure(err) => RETURN Error("Write: Wr.Failure: " & AL.Format(err))
    END
  END Write;

PROCEDURE VectorToList(x : Object; t : Scheme.T := NIL) : Pair RAISES { E } =
  BEGIN
    TYPECASE x OF
      Vector(vec) =>
      VAR result : Pair := NIL; BEGIN
        FOR i := LAST(vec^) TO FIRST(vec^) BY -1 DO
          result := Cons(vec[i],result,t)
        END;
        RETURN result
      END
    ELSE
      EVAL Error("expected a vector, got: " & StringifyT(x));
      RETURN NIL
    END
  END VectorToList;

PROCEDURE P(msg : TEXT; x : Object) : Object =
  (* for debugging *)
  BEGIN
    TRY Wr.PutText(Stdio.stdout, msg & ": " & Stringify(x))
    EXCEPT ELSE END;
    RETURN x
  END P;

PROCEDURE Stringify(x : Object) : TEXT RAISES { E } =
  BEGIN RETURN StringifyQ(x,TRUE) END Stringify;

PROCEDURE StringifyQ(x : Object; quoted : BOOLEAN) : TEXT RAISES { E } =
  BEGIN
    WITH buf = Wx.New() DO
      StringifyB(x, quoted, buf, NIL);
      RETURN Wx.ToText(buf)
    END
  END StringifyQ;

PROCEDURE StringifyB(x      : Object; 
                     quoted : BOOLEAN; 
                     buf    : Wx.T;
                     seen   : RefSeq.T) RAISES { E } =

  PROCEDURE Put(txt : TEXT) = BEGIN Wx.PutText(buf,txt) END Put;
  PROCEDURE PutC(c : CHAR) = BEGIN Wx.PutChar(buf,c) END PutC;

  BEGIN
    EVAL SchemeConvertHooks.AttemptConvertToScheme(x);

    IF seen = NIL THEN seen := NEW(RefSeq.T).init() END;

    FOR i := 0 TO seen.size()-1 DO
      IF seen.get(i) = x THEN Put("<...>"); RETURN END
    END;
    
    seen.addhi(x);
    TRY
      IF x = NIL THEN 
        Put("()")
      ELSE
        TYPECASE x OF
          TEXT(txt) => (* really should not normally happen *)
          Wx.PutText(buf, "<Modula-3 TEXT \"");
          Wx.PutText(buf, txt);
          Wx.PutText(buf, "\">")
        |
          SchemeLongReal.T(lr) =>
          IF FLOAT(ROUND(lr^),LONGREAL) = lr^ THEN
            Wx.PutInt(buf,(ROUND(lr^)))
          ELSIF FLOAT(LAST(CARDINAL),LONGREAL) = lr^ THEN
            (* tricky special case for 64-bit machines.  Possible loss
               of precision! *)
            Wx.PutInt(buf,LAST(CARDINAL))
          ELSIF ABS(lr^) > 1.0d10 AND 
                lr^ >= FLOAT(FIRST(INTEGER), LONGREAL) AND  
                lr^ <= FLOAT(LAST(INTEGER), LONGREAL) THEN
            <*FATAL FloatMode.Trap, Lex.Error*> (* must be able to parse Fmt *)
            BEGIN WITH o  = Fmt.LongReal(ABS(lr^)),
                 s  = Scan.LongReal(o),
                 o1 = Fmt.LongReal(ABS(lr^)-1.0d0),
                 s1 = Scan.LongReal(o1) DO
              IF s = s1 THEN
                Wx.PutInt(buf, ROUND(lr^))
              ELSE
                Put(Fmt.LongReal(lr^))
              END
            END END (* BEGIN WITH *)
          ELSE
            Put(Fmt.LongReal(lr^))
          END
        |
          SchemeEnvironmentBinding.T(b) =>
          PutC('<'); Put(SchemeSymbol.ToText(b.name())); PutC('>')
        |
          SchemeProcedure.T(p) =>
          PutC('{'); Put(p.name); PutC('}')
        |
          SchemeSymbol.T(sym) =>
          Put(SchemeSymbol.ToText(sym))
        |
          SchemeChar.T(c) =>
          IF quoted THEN Put("#" & BS) END;
          PutC(Char(c))
        |
          Pair(p) => SchemePair.StringifyPair(p,quoted,buf,seen)
        |
          String(s) =>
          IF quoted THEN 
            PutC(DQC) ;
            FOR i := FIRST(s^) TO LAST(s^) DO
              IF s[i] = DQC THEN PutC(BSC) END;
              PutC(s[i])
            END;
            PutC(DQC)
          ELSE
            Wx.PutStr(buf, s^)
          END
        |
          Vector(v) =>
          Put("#(");
          FOR i := FIRST(v^) TO LAST(v^) DO
            StringifyB(v[i], quoted, buf, seen);
            IF i # LAST(v^) THEN PutC(' ') END
          END;
          PutC(')')
        |
          Boolean(b) =>
          CASE SchemeBoolean.TruthO(b) OF
            TRUE => Put("#t")
          |
            FALSE => Put("#f")
          END
        ELSE
          Put(DebugFormat(x)) (* quoting? *)
        END
      END
    FINALLY
      WITH s = seen.remhi() DO
        <*ASSERT s = x *>
      END
    END
  END StringifyB;

(* special characters below, they mess up auto-indentation in emacs... *)
CONST BS = "\\"; DQC = '"'; BSC= '\\';

BEGIN END SchemeUtils.
