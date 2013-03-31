UNSAFE MODULE Public;
IMPORT M3toC;
CONST cstr = M3toC.FlatTtoS;

PROCEDURE F1(a:T) =
BEGIN
    PutT(cstr("Public.F1.a"), a);
    PutI(cstr("Public.F1.a.a"), a.a);
END F1;

BEGIN
END Public.
