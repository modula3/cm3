UNSAFE MODULE Private;
IMPORT Public, M3toC;
CONST cstr = M3toC.FlatTtoS;

PROCEDURE F2(a:Public.Private) =
BEGIN
    Public.PutT(cstr("Private.F2:a"), a);
    Public.PutI(cstr("Private.F2:a.a"), NARROW(a, Public.T).a);
    Public.PutI(cstr("Private.F2:a.b"), a.b);
END F2;

BEGIN
END Private.
