MODULE Temp;

PROCEDURE SmallArr(a : SArray)=
BEGIN
EVAL a;
END SmallArr;

<*NOWARN*>PROCEDURE BigArr(a : BArray)=
BEGIN
EVAL a;
END BigArr;

<*NOWARN*>PROCEDURE OpenArr(a : OpenArrType) =
BEGIN
EVAL a;
END OpenArr;

PROCEDURE SmallRec(r : SRec)=
BEGIN
EVAL r;
END SmallRec;

PROCEDURE BigRec(r : BRec)=
BEGIN
EVAL r;
END BigRec;

PROCEDURE SmallPackRec(r : SPackRec) =
BEGIN
EVAL r;
END SmallPackRec;

PROCEDURE BigPackRec(r : BPackRec) =
BEGIN
EVAL r;
END BigPackRec;

PROCEDURE SmallSet(s : SSet)=
BEGIN
EVAL s;
END SmallSet;

PROCEDURE BigSet(s : BSet)=
BEGIN
EVAL s;
END BigSet;

BEGIN
END Temp.
