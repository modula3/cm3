MODULE B;
IMPORT A;

PROCEDURE Unused() = BEGIN A.Exported2();  END Unused;
PROCEDURE Used() = BEGIN END Used;
PROCEDURE Exported() = BEGIN Used(); A.Exported1(); END Exported;

BEGIN
END B.
