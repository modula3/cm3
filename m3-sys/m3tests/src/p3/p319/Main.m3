(* Copyright (C) 2026 Modula-3 contributors. All rights reserved.       *)
(* See file COPYRIGHT-CMASS for details.                                *)

(* A constant array of multi-word sets, read by subscript and copied: the
   sets are data in the constant's image (sgml's SymSet; MSIR crashed in
   MSIR.addInsn building them as instructions with no block). *)

MODULE Main;

IMPORT Test;

TYPE
  Sym  = [0 .. 199];
  Syms = SET OF Sym;                  (* four words *)
  Pair = SET OF [0 .. 127];           (* two words *)
  Tri  = SET OF [0 .. 191];           (* three words *)

CONST
  Named = Syms {3, 150};
  Table = ARRAY [0 .. 3] OF Syms {
            Syms {0},
            Syms {1, 64, 65, 127, 128, 199},
            Syms {10 .. 120},
            Named };
  Pairs = ARRAY [0 .. 2] OF Pair {
            Pair {}, Pair {0, 63, 64, 127}, Pair {5 .. 70} };
  Tris  = ARRAY [0 .. 2] OF Tri {
            Tri {191}, Tri {0, 100}, Tri {64 .. 128} };

PROCEDURE Count (READONLY s: Syms): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := FIRST (Sym) TO LAST (Sym) DO IF i IN s THEN INC (n) END END;
    RETURN n;
  END Count;

VAR
  k: INTEGER;
  s: Syms;
  t: Tri;
BEGIN
  Test.checkI (Count (Table[0]), 1);
  Test.checkI (Count (Table[1]), 6);
  Test.checkI (Count (Table[2]), 111);
  Test.checkI (Count (Table[3]), 2);
  FOR i := 0 TO 3 DO
    k := i;                           (* a non-constant subscript *)
    s := Table[k];
    Test.checkI (Count (s), Count (Table[i]));
  END;
  Test.check (199 IN Table[1] AND NOT 198 IN Table[1]);
  Test.check (150 IN Table[3] AND 3 IN Table[3]);
  k := 1;
  Test.check (127 IN Pairs[k] AND 64 IN Pairs[k] AND NOT 65 IN Pairs[k]);
  Test.check (Pairs[0] = Pair {});
  Test.check (Pairs[2] = Pair {5 .. 70});
  FOR i := 0 TO 2 DO
    k := i;
    t := Tris[k];
    Test.check (t = Tris[i]);
  END;
  Test.check (191 IN Tris[0] AND 100 IN Tris[1] AND 128 IN Tris[2]
              AND NOT 129 IN Tris[2]);
  Test.done ();
END Main.
