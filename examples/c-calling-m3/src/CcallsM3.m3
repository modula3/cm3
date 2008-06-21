
MODULE CcallsM3 EXPORTS Main;

IMPORT IO, Cstuff;

VAR
  x: INTEGER := 33;
  i: INTEGER;

PROCEDURE Foo (): INTEGER =
  BEGIN
    INC (x);
    RETURN x;
  END Foo;

BEGIN
  IO.Put ("calling add_one.\n");
  i := Cstuff.add_one (Foo);
  IO.Put ("add_one () => ");  IO.PutInt (i);  IO.Put ("\n");

  IO.Put ("calling add_one_again.\n");
  Cstuff.m3_proc := Foo;
  i := Cstuff.add_one_again ();
  IO.Put ("add_one_again () => ");  IO.PutInt (i);  IO.Put ("\n");

END CcallsM3.

