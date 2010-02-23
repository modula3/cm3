UNSAFE MODULE Main;
IMPORT RTIO;

TYPE S1 = SET OF [0..500];

VAR
  a := S1{0};
  b := S1{1};
  c: CARDINAL;
  d := S1{c};

PROCEDURE Put(VAR a: S1) =
  BEGIN
    RTIO.Flush();
    RTIO.PutBytes(ADR(a), BYTESIZE(S1));
    RTIO.Flush();
    RTIO.PutText("\n");
    RTIO.Flush();
  END Put;

BEGIN
  Put(a);
  Put(b);
  Put(d);
  a := S1{2};
  Put(a);
  a := b;
  Put(a);
  a := S1{c};
  Put(a);
  INC(c);
  a := S1{c};
  Put(a);
  INC(c, 4);
  a := S1{c};
  Put(a);
  INC(c, 40);
  a := S1{c};
  Put(a);
  INC(c, 400);
  a := S1{c};
  Put(a);
  a := S1{c} + S1{0} + b + d;
  Put(a);
  FOR i := 10 TO 400 BY 10 DO
    a := a + S1{i};
  END;
  Put(a);
END Main.
