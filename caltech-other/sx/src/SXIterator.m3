(* $Id$ *)

MODULE SXIterator;
IMPORT SXRoot;

PROCEDURE NullIterator() : T =
  BEGIN RETURN NEW(T, next := NullNext) END NullIterator;

PROCEDURE NullNext(<*UNUSED*>t : T; <*UNUSED*>VAR a : SXRoot.T) : BOOLEAN = 
  BEGIN RETURN FALSE END NullNext;

TYPE 
  Binary = T OBJECT 
    x    : ARRAY [0..1] OF SXRoot.T; 
    p    : [0..2];
  OVERRIDES
    next := BinNext;
  END;

PROCEDURE BinNext(t : Binary; VAR a : SXRoot.T) : BOOLEAN =
  BEGIN
    CASE t.p OF
      0,1 => a := t.x[t.p]; INC(t.p); RETURN TRUE
    |
      2 => RETURN FALSE
    END
  END BinNext;

PROCEDURE One(a : SXRoot.T) : T =
  BEGIN 
    RETURN NEW(Binary, x := ARRAY [0..1] OF SXRoot.T { NIL, a }, p := 1)
  END One;

PROCEDURE Two(a, b : SXRoot.T) : T =
  BEGIN 
    RETURN NEW(Binary, x := ARRAY [0..1] OF SXRoot.T { a, b }, p := 0)
  END Two;


TYPE 
  Nary = T OBJECT 
    x    : REF ARRAY OF SXRoot.T; 
    p    : CARDINAL := 0;
  OVERRIDES
    next := NNext;
  END;

PROCEDURE NNext(t : Nary; VAR a : SXRoot.T) : BOOLEAN =
  BEGIN
    IF t.p = NUMBER(t.x^) THEN
      RETURN FALSE
    ELSE
      a := t.x[t.p]; INC(t.p); RETURN TRUE
    END
  END NNext;

PROCEDURE Many(READONLY a : ARRAY OF SXRoot.T) : T =
  VAR
    aa := NEW(REF ARRAY OF SXRoot.T, NUMBER(a));
  BEGIN 
    aa^ := a;
    RETURN NEW(Nary, x := aa, p := 0)
  END Many;

PROCEDURE NullNull(<*UNUSED*>a : SXRoot.T) : T =
  BEGIN RETURN NullIterator() END NullNull;

BEGIN END SXIterator.


