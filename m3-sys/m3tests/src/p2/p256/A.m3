MODULE A;

PROCEDURE Unused() = BEGIN END Unused;
PROCEDURE Used() = BEGIN END Used;
PROCEDURE Exported1() = BEGIN Used(); END Exported1;
PROCEDURE Exported2() = BEGIN Used(); END Exported2;

BEGIN
END A.
