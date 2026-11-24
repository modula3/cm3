(* An empty array constructor of open arrays, ARRAY OF ARRAY OF T {}, has
   no elements to give its inner levels a length.  Passing one to a VALUE
   or READONLY open-array formal must work, and have no elements. *)

MODULE Main;
IMPORT IO, Fmt;

PROCEDURE N2 (VALUE x: ARRAY OF ARRAY OF INTEGER): INTEGER =
  BEGIN RETURN NUMBER (x) END N2;

PROCEDURE R2 (READONLY x: ARRAY OF ARRAY OF INTEGER): INTEGER =
  BEGIN RETURN NUMBER (x) END R2;

PROCEDURE N3 (VALUE x: ARRAY OF ARRAY OF ARRAY OF CHAR): INTEGER =
  BEGIN RETURN NUMBER (x) END N3;

VAR r := NEW (REF ARRAY OF ARRAY OF INTEGER, 0, 5);

BEGIN
  IO.Put (Fmt.Int (N2 (ARRAY OF ARRAY OF INTEGER {})) & " "
          & Fmt.Int (R2 (ARRAY OF ARRAY OF INTEGER {})) & " "
          & Fmt.Int (N3 (ARRAY OF ARRAY OF ARRAY OF CHAR {})) & " "
          & Fmt.Int (N2 (r^)) & " "
          & Fmt.Int (N2 (ARRAY OF ARRAY OF INTEGER {ARRAY OF INTEGER {1, 2}}))
          & "\n");
END Main.
