MODULE Main;
IMPORT RTIO;

TYPE P1 = PROCEDURE();
TYPE R1 = RECORD a := 999; END;

PROCEDURE F2(p: P1) =
BEGIN
    p();
END F2;

PROCEDURE Main(VAR a1:INTEGER; r2: R1; VAR r3: R1) =
VAR r4 := R1{a:=400};
    r5 := R1{a:=500};
PROCEDURE F1() =
    BEGIN
    INC(a1); INC(r2.a); INC(r3.a); INC(r4.a);
    END F1;
BEGIN
    INC(r5.a);
    INC(a1); INC(r2.a); INC(r3.a); INC(r4.a);
    <* ASSERT a1 = 101 *>
    <* ASSERT r2.a = 201 *>
    <* ASSERT r3.a = 301 *>
    <* ASSERT r4.a = 401 *>
    <* ASSERT r5.a = 501 *>
    F1();
    <* ASSERT a1 = 102 *>
    <* ASSERT r2.a = 202 *>
    <* ASSERT r3.a = 302 *>
    <* ASSERT r4.a = 402 *>
    <* ASSERT r5.a = 501 *>
    F2(F1);
    <* ASSERT a1 = 103 *>
    <* ASSERT r2.a = 203 *>
    <* ASSERT r3.a = 303 *>
    <* ASSERT r4.a = 403 *>
    <* ASSERT r5.a = 501 *>
END Main;

VAR xa1 := 100;
xr2 := R1{a := 200};
xr3 := R1{a := 300};
BEGIN
    Main(xa1, xr2, xr3);
    RTIO.PutInt(xa1); RTIO.PutText("\n");
    RTIO.PutInt(xr2.a); RTIO.PutText("\n");
    RTIO.PutInt(xr3.a); RTIO.PutText("\n");
    RTIO.Flush();
END Main.
