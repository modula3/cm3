(* Copyright (C) 2026 Modula-3 contributors. All rights reserved.       *)
(* See file COPYRIGHT-CMASS for details.                                *)

(* Set constructors of one word with non-constant ranges.  The mask of
   {a..b} is (-1 shift_right (N-1-b)) AND (-1 shift_left a), and
   shift_right is a logical shift; the C back end shifted -1 arithmetically,
   so every such range ran to the top of the set. *)

MODULE Main;

IMPORT Test;

TYPE
  Small = SET OF [0 .. 15];           (* a part of a word *)
  Whole = SET OF [0 .. 63];           (* a whole word on 64-bit targets *)
  Shift = SET OF [10 .. 25];          (* a non-zero lower bound *)

PROCEDURE CardS (READONLY s: Small): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 0 TO 15 DO IF i IN s THEN INC (n) END END;
    RETURN n;
  END CardS;

PROCEDURE CardW (READONLY s: Whole): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 0 TO 63 DO IF i IN s THEN INC (n) END END;
    RETURN n;
  END CardW;

PROCEDURE CardH (READONLY s: Shift): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 10 TO 25 DO IF i IN s THEN INC (n) END END;
    RETURN n;
  END CardH;

VAR lo, hi: INTEGER;
BEGIN
  FOR a := 0 TO 15 DO
    FOR b := a TO 15 DO
      lo := a;  hi := b;
      Test.checkI (CardS (Small {lo .. hi}), b - a + 1);
      Test.check (Small {lo .. hi} = Small {a} + Small {lo .. hi} - Small {});
      Test.check ((a = 0 OR NOT (a - 1) IN Small {lo .. hi})
                  AND (b = 15 OR NOT (b + 1) IN Small {lo .. hi}));
    END;
  END;
  lo := 3;  hi := 4;
  Test.check (Small {lo .. hi} = Small {3, 4});
  Test.check (Small {0, lo .. hi, 9} = Small {0, 3, 4, 9});
  Test.check (Small {hi .. lo} = Small {});
  lo := 5;  hi := 40;
  Test.checkI (CardW (Whole {lo .. hi}), 36);
  Test.check (NOT 41 IN Whole {lo .. hi} AND 40 IN Whole {lo .. hi});
  lo := 0;  hi := 62;
  Test.checkI (CardW (Whole {lo .. hi}), 63);
  Test.check (NOT 63 IN Whole {lo .. hi});
  lo := 12;  hi := 20;
  Test.checkI (CardH (Shift {lo .. hi}), 9);
  Test.check (Shift {lo .. hi} = Shift {12, 13, 14, 15, 16, 17, 18, 19, 20});
  Test.done ();
END Main.
