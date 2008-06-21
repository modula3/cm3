
UNSAFE MODULE Unsafe EXPORTS Main;
FROM Clib IMPORT abs;
IMPORT IO, Fmt;

CONST
  an_integer = 10;

BEGIN

  IO.Put    ("Absolute value of ");
  IO.PutInt (an_integer); 
  IO.Put    (" is " );
  IO.PutInt (abs(an_integer));
  IO.Put    (".\n");

  IO.Put ("Absolute value of " & Fmt.Int(-an_integer) &
          " is " & Fmt.Int(abs(-an_integer)) & ".\n");

END Unsafe.
