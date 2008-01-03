(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 

(* more elaborate SCANF tests *)

MODULE Main;

IMPORT Test, TextRd, Text;

PROCEDURE TestEqualSub (t: TEXT;  READONLY x: ARRAY OF CHAR) =
  BEGIN
    FOR i := 0 TO Text.Length (t)-1 DO
      Test.checkC (Text.GetChar (t, i), x[i]);
    END;
  END TestEqualSub;

PROCEDURE DoIt () =
VAR
    i1, i2: INTEGER;
    r1, r2: REAL;
    c: CHAR;
    a: ARRAY [0..5] OF CHAR;
    t: Text.T;
BEGIN
    SCANF (TextRd.New (" 31 "), "%d", i1);
    Test.checkI (i1, 31);
    SCANF (TextRd.New (" 31    32"), "%d %d", i1, i2);
    Test.checkI (i1, 31);
    Test.checkI (i2, 32);
    SCANF (TextRd.New (" 31    32"), "%*d %d", i1);
    Test.checkI (i1, 32); 
    SCANF (TextRd.New (" 3104"), "%2d", i1);
    Test.checkI (i1, 31); 
    SCANF (TextRd.New (" 3 "), "%2d", i1);
    Test.checkI (i1, 3); 

    SCANF (TextRd.New (" 31 "), "%o", i1);
    Test.checkI (i1, 25);
    SCANF (TextRd.New (" 31    32"), "%o %o", i1, i2);
    Test.checkI (i1, 25);
    Test.checkI (i2, 26);
    SCANF (TextRd.New (" 31    32"), "%*o %o", i1);
    Test.checkI (i1, 26); 
    SCANF (TextRd.New (" 3104"), "%2o", i1);
    Test.checkI (i1, 25); 
    SCANF (TextRd.New (" 3 "), "%2o", i1);
    Test.checkI (i1, 3); 

    SCANF (TextRd.New (" f3 "), "%x", i1);
    Test.checkI (i1, 243);
    SCANF (TextRd.New (" f3    f4"), "%x %x", i1, i2);
    Test.checkI (i1, 243);
    Test.checkI (i2, 244);
    SCANF (TextRd.New (" f3    f4"), "%*x %x", i1);
    Test.checkI (i1, 244); 
    SCANF (TextRd.New (" f304"), "%2x", i1);
    Test.checkI (i1, 243); 
    SCANF (TextRd.New (" f "), "%2x", i1);
    Test.checkI (i1, 15); 

    SCANF (TextRd.New (" 3.1e1 "), "%e", r1);
    Test.checkR (r1, 31.0);
    SCANF (TextRd.New (" 31    32.0"), "%e %f", r1, r2);
    Test.checkR (r1, 31.0);
    Test.checkR (r2, 32.0);
    SCANF (TextRd.New (" 31    32.0"), "%*e %f", r1);
    Test.checkR (r1, 32.0); 
    SCANF (TextRd.New (" 31.1"), "%2e", r1);
    Test.checkR (r1, 31.0); 
    SCANF (TextRd.New (" 3 "), "%2e", r1);
    Test.checkR (r1, 3.0); 

    SCANF (TextRd.New (" ab"), "%c", c);
    Test.checkC (c, ' ');
    SCANF (TextRd.New (" ab"), "%*c%c", c);
    Test.checkC (c, 'a');
   
    SCANF (TextRd.New (" ab"), "%2c", a);
    Test.checkC (a[ 0 ], ' ');
    Test.checkC (a[ 1 ], 'a');
    SCANF (TextRd.New (" a3a"), "%*2c%d", i1);
    Test.checkI (i1, 3);
    
    SCANF (TextRd.New (" hello world"), "%t", t);
    Test.check (Text.Equal (t, "hello"));
    SCANF (TextRd.New (" hello world"), "%*t%t", t);
    Test.check (Text.Equal (t, "world"));
    
    SCANF (TextRd.New (" hello "), "%s", a);
    TestEqualSub ("hello", a);
    Test.checkC (a[ 5 ], VAL (0, CHAR));
    SCANF (TextRd.New (" helloworld "), "%s%c", a, c);
    TestEqualSub ("hellow", a);
    Test.checkC (c, 'o');
    SCANF (TextRd.New (" helloworld "), "%4s%c", a, c);
    TestEqualSub ("hell", a);
    Test.checkC (c, 'o');
    SCANF (TextRd.New (" helloworld 3"), "%*s%d", i1);
    Test.checkI (i1, 3);

    SCANF (TextRd.New ("a b c31"), "%[abc ]%d", t, i1);
    Test.check  (Text.Equal (t, "a b c"));
    Test.checkI (i1, 31);
    SCANF (TextRd.New ("a b c31"), "%[^abc ]", t);
    Test.check  (Text.Equal (t, ""));
    SCANF (TextRd.New ("a b c31"), "%[^0123456789]%d", t, i1);
    Test.check  (Text.Equal (t, "a b c"));
    Test.checkI (i1, 31);
    SCANF (TextRd.New ("a b c31"), "%[abc ]%d", a, i1);
    TestEqualSub ("a b c", a);
    Test.checkI (i1, 31);
    SCANF (TextRd.New ("a b c31"), "%3[abc ]%c", t, c);
    Test.check  (Text.Equal (t, "a b"));
    Test.checkC (c, ' ');
    SCANF (TextRd.New ("a b c31"), "%3[abc ]%c", a, c);
    TestEqualSub ("a b", a);
    Test.checkC (c, ' ');
    SCANF (TextRd.New ("a b c31"), "%*[abc ]%d", i1);
    Test.checkI (i1, 31);
    SCANF (TextRd.New ("a b c31"), "%*3[abc ]%c", c);
    Test.checkC (c, ' ');
    
    SCANF (TextRd.New ("Hello 31% world"), "Hello%d%% world", i1);
    Test.checkI (i1, 31);

    TRY
      SCANF (TextRd.New ("32 Hello 31"), "%d%d", i1, i2);
      Test.check (FALSE);
    EXCEPT Rd.ScanFailed =>
      Test.checkI (i1, 32);
    END;

    TRY
      SCANF (TextRd.New ("   \n 33"), "%d%d", i1, i2);
      Test.check (FALSE);
    EXCEPT Rd.EndOfFile =>
      Test.checkI (i1, 33);
    END;
  END DoIt;

BEGIN
  DoIt ();
END Main.
