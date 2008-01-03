(* copied from LongrealType.m3 *)

UNSAFE MODULE ExtendedType;

IMPORT Word, Int32;

PROCEDURE Equal (a, b: T; ): BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;


CONST Int32Sz = BYTESIZE(T) DIV BYTESIZE(Int32.T);

TYPE Int32Arr = ARRAY [0 .. Int32Sz - 1] OF Int32.T;

PROCEDURE Hash (a: T; ): Word.T =
  VAR
    arr := LOOPHOLE(a, Int32Arr);
    res := 0;
  BEGIN
    FOR i := 0 TO LAST(arr) DO
      res := Word.Xor(res, Int32.Hash(arr[i]))
    END (* FOR *);
    RETURN res
  END Hash;

PROCEDURE Compare (a, b: T; ): [-1 .. 1] =
  BEGIN
    IF a < b THEN
      RETURN -1
    ELSIF a > b THEN
      RETURN 1
    ELSE
      RETURN 0
    END                          (* IF *)
  END Compare;

BEGIN
END ExtendedType.
