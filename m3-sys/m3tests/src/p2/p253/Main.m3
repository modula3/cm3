UNSAFE MODULE Main;
IMPORT Public, Private;

PROCEDURE Main() =
VAR a:= NEW(Public.T);
BEGIN
    a.F1();
    NARROW(a, Public.Private).F2();
END Main;

BEGIN
    Main();
END Main.
