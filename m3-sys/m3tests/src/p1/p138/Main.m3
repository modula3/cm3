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
        d: BITS 5 FOR [0..15];
        e: BITS 7 FOR [0..127];
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
    x.e := 127;
    Test.checkI (x.a, 3);
    Test.checkI (x.b, 3);
    Test.checkI (x.c, 65535);
    Test.checkI (x.d, 15);
    Test.checkI (x.e, 127);

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

    x.a := 1;
    x.b := 1;
    x.c := 32767;
    x.d := 7;
    x.e := 63;
    Test.checkI (x.a, 1);
    Test.checkI (x.b, 1);
    Test.checkI (x.c, 32767);
    Test.checkI (x.d, 7);
    Test.checkI (x.e, 63);

    x.c := 1;
    x.d := 1;
    x.e := 1;
    Test.checkI (x.a, 1);
    Test.checkI (x.b, 1);
    Test.checkI (x.c, 1);
    Test.checkI (x.d, 1);
    Test.checkI (x.e, 1);

  END;
  Test.done ();
END Main.

