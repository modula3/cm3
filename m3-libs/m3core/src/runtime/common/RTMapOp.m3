(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Apr 27 11:59:01 PDT 1993 by kalsow     *)

UNSAFE MODULE RTMapOp;

IMPORT Word;

PROCEDURE GetInt (VAR pc: ADDRESS;  size: [1..8]): INTEGER =
  VAR
    p : BP       := pc;
    n : INTEGER  := p^;
    s : CARDINAL := size-1;
    k : CARDINAL := 8;
  BEGIN
    INC (p, ADRSIZE (Byte));
    WHILE (s > 0) DO
      n := Word.Or (n, Word.LeftShift (p^, k));
      INC (p, ADRSIZE (Byte));
      INC (k, 8);
      DEC (s);
    END;
    pc := p;
    RETURN n;
  END GetInt;

PROCEDURE Push (VAR stack: Stack;  a: ADDRESS;  b: INTEGER) =
  BEGIN
    WITH z = stack.data [stack.top] DO  z.pc := a;  z.count := b;  END;
    INC (stack.top);
  END Push;

BEGIN
END RTMapOp.

