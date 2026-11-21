(* The C backend must parenthesise the address of a global at a non-zero
   offset: compared directly, it once rendered as off+(ADDRESS)&seg inside
   a cast, which adds two pointers and does not compile. *)

UNSAFE MODULE Main;
IMPORT Test;

VAR a := 1;  z := 0;

PROCEDURE Left (p: ADDRESS): BOOLEAN =
  BEGIN
    RETURN p = ADR (z);
  END Left;

PROCEDURE Right (p: ADDRESS): BOOLEAN =
  BEGIN
    RETURN ADR (z) # p;
  END Right;

BEGIN
  Test.check (Left (ADR (z)));
  Test.check (NOT Left (ADR (a)));
  Test.check (NOT Right (ADR (z)));
  Test.check (Right (ADR (a)));
  Test.done ();
END Main.
