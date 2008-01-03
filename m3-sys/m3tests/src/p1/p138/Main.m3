(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

VAR
    x: RECORD
        a: BITS 2 FOR [0..3];
        b: BITS 2 FOR [0..3];
        c: BITS 16 FOR [0..65535];
        d: BITS 4 FOR [0..15];
        e: BITS 14 FOR [0..16383];
       END;

BEGIN
  FOR i := 1 TO 2 DO
    x.a := 0;
    x.b := 0;
    x.c := 0;
    x.d := 0;
    x.e := 0;
    Test.checkI (x.a, 0);
    Test.checkI (x.b, 0);
    Test.checkI (x.c, 0);
    Test.checkI (x.d, 0);
    Test.checkI (x.e, 0);

    x.a := 3;
    x.b := 3;
    x.c := 65535;
    x.d := 15;
    x.e := 16383;
    Test.checkI (x.a, 3);
    Test.checkI (x.b, 3);
    Test.checkI (x.c, 65535);
    Test.checkI (x.d, 15);
    Test.checkI (x.e, 16383);

    x.a := 2;
    x.b := 2;
    x.c := 5;
    x.d := 5;
    x.e := 5;
    Test.checkI (x.a, 2);
    Test.checkI (x.b, 2);
    Test.checkI (x.c, 5);
    Test.checkI (x.d, 5);
    Test.checkI (x.e, 5);
  END;
  Test.done ();
END Main.

