MODULE Example;
IMPORT IO;

TYPE
  UU = T OBJECT 
  OVERRIDES
  END;

PROCEDURE TTT(x : TT) =
  BEGIN
    T.hello(x)
  END TTT;

PROCEDURE TTHello(tt : TT) = BEGIN END TTHello;

PROCEDURE THello(t : T) = 
  BEGIN 
    IO.Put("hi there!\n")
  END THello;

PROCEDURE Sum(a : ARRAY OF LONGREAL) : LONGREAL =
  VAR sum := 0.0d0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      sum := sum + a[i]
    END ;
    RETURN sum
  END Sum;

BEGIN 
  EVAL NEW(UU, hello := THello)
END Example.
