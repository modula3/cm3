MODULE Range;

<* INLINE *>
PROCEDURE New (first: INTEGER; number: CARDINAL; ): T =
  BEGIN
    RETURN T{first, number};
  END New;

<* INLINE *>
PROCEDURE First (x: T; ): INTEGER =
  BEGIN
    RETURN x.first;
  END First;

<* INLINE *>
PROCEDURE Last (x: T; ): INTEGER =
  BEGIN
    RETURN x.first + x.number - 1;
  END Last;

<* INLINE *>
PROCEDURE Number (x: T; ): CARDINAL =
  BEGIN
    RETURN x.number;
  END Number;

BEGIN
END Range.
