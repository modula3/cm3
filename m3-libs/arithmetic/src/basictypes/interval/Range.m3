MODULE Range;

<* INLINE *>
PROCEDURE New (first: INTEGER; number: CARDINAL; ): T =
  BEGIN
    RETURN T{first, number};
  END New;

<* INLINE *>
PROCEDURE First (READONLY x: T; ): INTEGER =
  BEGIN
    RETURN x.first;
  END First;

<* INLINE *>
PROCEDURE Last (READONLY x: T; ): INTEGER =
  BEGIN
    RETURN x.first + x.number - 1;
  END Last;

<* INLINE *>
PROCEDURE Number (READONLY x: T; ): CARDINAL =
  BEGIN
    RETURN x.number;
  END Number;



<* INLINE *>
PROCEDURE Add (READONLY x, y: T; ): T =
  BEGIN
    RETURN T{x.first + y.first, x.number + y.number - 1};
  END Add;

<* INLINE *>
PROCEDURE Scale (READONLY x: T; y: CARDINAL; ): T =
  BEGIN
    RETURN T{x.first * y, x.number * y + 1 - y};
  END Scale;

BEGIN
END Range.
