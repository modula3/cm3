MODULE Main;
IMPORT RTIO;

TYPE P1 = PROCEDURE();
TYPE R1 = RECORD a := 100; END;

PROCEDURE F2(p: P1) =
BEGIN
    p();
END F2;

PROCEDURE Main(VAR a:INTEGER; r1: R1; VAR r2: R1) =
    PROCEDURE F1() =
    BEGIN
        INC(a);
        INC(r1.a);
        INC(r2.a);
    END F1;
BEGIN
    INC(a);
    <* ASSERT a = 2 *>
    F1();
    <* ASSERT a = 3 *>
    F2(F1);
    <* ASSERT a = 4 *>
END Main;

VAR xa := 1;
xr1, xr2: R1;
BEGIN
    Main(xa, xr1, xr2);
    RTIO.PutInt(xa); RTIO.PutText("\n");
    RTIO.PutInt(xr1.a); RTIO.PutText("\n");
    RTIO.PutInt(xr2.a); RTIO.PutText("\n");
    RTIO.Flush();
END Main.
