UNSAFE MODULE StackTest EXPORTS Main;
IMPORT IO, Fmt;
IMPORT SchemeSymbol, SchemeEnvironment;

TYPE
  Frame = ARRAY OF REFANY;

  Getter = PROCEDURE (up, pos : CARDINAL) : REFANY;

  Setter = PROCEDURE (up, pos : CARDINAL; to : REFANY);

  Copier = PROCEDURE() : REF Frame;

  Adapter = PROCEDURE() : SchemeEnvironment.T;

PROCEDURE GetFrame(frame : REF Frame; up, pos : CARDINAL) : REFANY = 
  BEGIN
    WHILE up > 0 DO
      frame := frame[0]
    END;
    RETURN frame[pos]
  END GetFrame;

PROCEDURE SetFrame(frame : REF Frame; up, pos : CARDINAL; to : REFANY) =
  BEGIN
    WHILE up > 0 DO
      frame := frame[0]
    END;
    frame[pos] := to
  END SetFrame;

(**********************************************************************)

(* 
   (define (f x)
     (define (g y)
        (+ x y)
     )

     (define (h z)
        (set! x z)
     )
     (h 111)
     (g 4)
    )
*)

VAR PfArgs := ARRAY [0..0] OF SchemeSymbol.T { SchemeSymbol.FromText("x") };

PROCEDURE Pf(VAR args : ARRAY OF REFANY; 
             g : Getter; s : Setter; c : Copier; a : Adapter) : REFANY =

  PROCEDURE Get(up, pos : CARDINAL) : REFANY =
    BEGIN
      IF frame # NIL THEN
        RETURN GetFrame(frame, up, pos)
      ELSIF up > 0 THEN 
        RETURN g(up-1, pos) 
      ELSE 
        RETURN args[pos]
      END
    END Get;

  PROCEDURE Set(up, pos : CARDINAL; to : REFANY) =
    BEGIN
      IF frame # NIL THEN
        SetFrame(frame, up, pos, to)
      ELSIF up > 0 THEN 
        s(up-1, pos,to) 
      ELSE 
        args[pos] := to
      END
    END Set;

  PROCEDURE Copy() : REF Frame =
    BEGIN
      IF frame # NIL THEN RETURN frame END;
      frame := NEW(REF Frame, NUMBER(args));
      frame^ := args;
      IF c # NIL THEN
        frame[0] := c()
      END;
      RETURN frame
    END Copy;

  PROCEDURE Adapt() : SchemeEnvironment.T =
    VAR env : SchemeEnvironment.T;
        dummy : BOOLEAN;
    BEGIN
      IF a # NIL THEN
        env := NEW(SchemeEnvironment.T).init(NIL, NIL, a(), dummy) 
      ELSE
        env := NEW(SchemeEnvironment.T).init(NIL, NIL, NIL, dummy)
      END;
      FOR i := FIRST(PfArgs) TO LAST(PfArgs) DO
        EVAL env.define(PfArgs[i], args[i+1])
      END;
      RETURN env
    END Adapt;

  VAR
    frame : REF Frame := NIL;
    gargs := ARRAY [0..1] OF REFANY { NIL,   Make(4.0d0) };
    hargs := ARRAY [0..1] OF REFANY { NIL, Make(111.0d0) };
  BEGIN
    EVAL   Ph(hargs, Get, Set, Copy, Adapt);
    RETURN Pg(gargs, Get, Set, Copy, Adapt)
  END Pf;

VAR PgArgs := ARRAY [0..0] OF SchemeSymbol.T { SchemeSymbol.FromText("y") };

PROCEDURE Pg(VAR args : ARRAY OF REFANY; 
             g : Getter; s : Setter; c : Copier; a : Adapter) : REFANY =
  VAR 
    x := g(0, 1);
    y := args[1];
  BEGIN
    RETURN Add(x, y)
  END Pg;

VAR PhArgs := ARRAY [0..0] OF SchemeSymbol.T { SchemeSymbol.FromText("z") };

PROCEDURE Ph(VAR args : ARRAY OF REFANY; 
             g : Getter; s : Setter; c : Copier; a : Adapter) : REFANY =
  BEGIN
    s(0, 1, args[1]);
    RETURN NIL
  END Ph;

(**********************************************************************)

PROCEDURE Make(lr : LONGREAL) : REFANY =
  VAR 
    res : REF LONGREAL;
  BEGIN 
    IF lr > -1.0d0 AND lr <= FLOAT(LAST(cache),LONGREAL) AND FLOAT(ROUND(lr),LONGREAL) = lr THEN
      RETURN cache[ROUND(lr)]
    END;
    res := NEW(REF LONGREAL); 
    res^ := lr; 
    RETURN res 
  END Make;

PROCEDURE Add(x , y : REF LONGREAL) : REF LONGREAL =
  BEGIN RETURN Make(x^ + y^) END Add;

PROCEDURE Format(x : REF LONGREAL) : TEXT = 
  BEGIN RETURN Fmt.LongReal(x^) END Format;

VAR cache : ARRAY [0..1000] OF REFANY;

VAR 
  sf : PROCEDURE(frame : REF Frame; up, pos : CARDINAL; to : REFANY) 
     := SetFrame;
    
  re : REFANY := LOOPHOLE(sf,REFANY); (* will this work???? *)
BEGIN 

  FOR i := FIRST(cache) TO LAST(cache) DO
    cache[i] := NEW(REF LONGREAL);
    NARROW(cache[i],REF LONGREAL)^ := FLOAT(i, LONGREAL);
  END;

  VAR args := ARRAY [0..1] OF REFANY { NIL, Make(11.0d0) };
  BEGIN
    FOR i := 0 TO 10000000 DO
      EVAL Pf(args, NIL, NIL, NIL, NIL)
    END;

    IO.Put("f(11) = " & Format(Pf(args, 
                                  NIL, 
                                  NIL,
                                  NIL,
                                  NIL)) & "\n")
  END
END StackTest.
