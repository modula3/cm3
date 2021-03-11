(* $Id$ *)

MODULE GridPoint;
IMPORT Word;
IMPORT Fmt;
IMPORT Text, TextReader, Scan, FloatMode, Lex;

CONST
  BigPrime = 1000305463; (* courtesy of primes(6) *)
  OtherBigPrime = 1060043;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    x := Word.Times(a.x,OtherBigPrime);
    y := Word.Times(a.y,63691);
    z := Word.Times(a.l,BigPrime);
  BEGIN 
    RETURN Word.Plus(x,Word.Plus(y,z))
  END Hash;

PROCEDURE Format(READONLY p : T) : TEXT =
  BEGIN 
    RETURN "(" & 
           Fmt.Int(p.x) & "," & 
           Fmt.Int(p.y) & "," & 
           Fmt.Int(p.l) & ")" 
  END Format;

PROCEDURE Parse(t : TEXT) : T RAISES { ParseError } =
  VAR
    reader : TextReader.T;
    l := Text.Length(t);
  BEGIN
    IF Text.GetChar(t,0) # '(' OR Text.GetChar(t,l-1) # ')' THEN
      RAISE ParseError
    END;

    t := Text.Sub(t, 1, l-2);
    
    reader := NEW(TextReader.T).init(t);
    TRY
      VAR
        xt, yt, lt := reader.nextE(",");
        x := Scan.Int(xt);
        y := Scan.Int(yt);
        l := Scan.Int(lt);
      BEGIN
        IF NOT reader.isEmpty() THEN
          RAISE ParseError
        END;
        IF l < FIRST(Layer) OR l > LAST(Layer) THEN
          RAISE ParseError
        END;
        RETURN T { x, y, l }
      END
    EXCEPT
      TextReader.NoMore, FloatMode.Trap, Lex.Error => RAISE ParseError
    END
  END Parse;

PROCEDURE ComparX(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF a.x < b.x THEN RETURN -1
    ELSIF a.x > b.x THEN RETURN 1
    ELSE RETURN 0
    END
  END ComparX;

PROCEDURE ComparY(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF a.y < b.y THEN RETURN -1
    ELSIF a.y > b.y THEN RETURN 1
    ELSE RETURN 0
    END
  END ComparY;

PROCEDURE ComparL(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF a.l < b.l THEN RETURN -1
    ELSIF a.l > b.l THEN RETURN 1
    ELSE RETURN 0
    END
  END ComparL;

BEGIN END GridPoint.
