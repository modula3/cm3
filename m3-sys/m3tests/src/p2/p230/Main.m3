UNSAFE MODULE Main;
IMPORT RTIO;

TYPE S1 = SET OF [0..512];
TYPE S2 = SET OF [0..5000];

VAR
  a := S1{0};
  b := S1{1};
  c: CARDINAL;
  d := S1{c};
  A := S2{0};
  B := S2{1};
  D := S2{c};

PROCEDURE Put1(VAR a: S1) =
  BEGIN
    RTIO.Flush();
    RTIO.PutBytes(ADR(a), BYTESIZE(a));
    RTIO.Flush();
    RTIO.PutText("\n");
    RTIO.Flush();
  END Put1;

PROCEDURE Put2(VAR a: S2) =
  BEGIN
    RTIO.Flush();
    RTIO.PutBytes(ADR(a), BYTESIZE(a));
    RTIO.Flush();
    RTIO.PutText("\n");
    RTIO.Flush();
  END Put2;

BEGIN
  <* ASSERT a = d *>
  <* ASSERT A = D *>

  Put1(a);
  Put1(b);
  Put1(d);
  a := S1{2};
  Put1(a);
  a := b;
  Put1(a);
  a := S1{c};
  Put1(a);
  INC(c);
  a := S1{c};
  <* ASSERT a = S1{1} *>
  Put1(a);
  INC(c, 4);
  a := S1{c};
  <* ASSERT a = S1{5} *>
  Put1(a);
  INC(c, 40);
  a := S1{c};
  <* ASSERT a = S1{45} *>
  Put1(a);
  INC(c, 400);
  a := S1{c};
  <* ASSERT a = S1{445} *>
  Put1(a);
  a := S1{c} + S1{0} + b + d;
  <* ASSERT a = S1{445} + S1{1} + S1{0} *>
  Put1(a);
  FOR i := 10 TO 400 BY 10 DO
    a := a + S1{i};
  END;
  Put1(a);

  Put2(A);
  Put2(B);
  Put2(D);
  A := S2{2};
  Put2(A);
  A := B;
  Put2(A);
  A := S2{c};
  Put2(A);
  INC(c);
  A := S2{c};
  Put2(A);
  INC(c, 4);
  A := S2{c};
  Put2(A);
  INC(c, 40);
  A := S2{c};
  Put2(A);
  INC(c, 400);
  A := S2{c};
  Put2(A);
  A := S2{c} + S2{0} + B + D;
  Put2(A);
  FOR i := 10 TO 400 BY 10 DO
    A := A + S2{i};
  END;
  Put2(A);

END Main.
