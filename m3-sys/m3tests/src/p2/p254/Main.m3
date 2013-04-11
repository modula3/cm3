MODULE Main;
IMPORT RTIO;

TYPE P1 = PROCEDURE();
TYPE R1 = RECORD a := 123; END;

PROCEDURE F2(p: P1) =
BEGIN
    p();
END F2;

PROCEDURE Main(VAR a:INTEGER; r1: R1; VAR r2: R1) =
VAR r3 := R1{a:=400};
PROCEDURE F1() =
    BEGIN
    INC(a); INC(r1.a); INC(r2.a); INC(r3.a);
    END F1;
BEGIN
    r3.a := 400;
    INC(a); INC(r1.a); INC(r2.a); INC(r3.a);
    <* ASSERT a = 102 *>
    F1();
    <* ASSERT a = 103 *>
    F2(F1);
    <* ASSERT a = 104 *>
END Main;

VAR xa := 100;
xr1 := R1{a := 200};
xr2 := R1{a := 300};
BEGIN
    Main(xa, xr1, xr2);
    RTIO.PutInt(xa); RTIO.PutText("\n");
    RTIO.PutInt(xr1.a); RTIO.PutText("\n");
    RTIO.PutInt(xr2.a); RTIO.PutText("\n");
    RTIO.Flush();
END Main.
