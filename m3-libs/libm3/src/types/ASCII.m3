(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 16:56:47 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:56:56 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:28 1989 by muller         *)
(*      modified on Fri Sep 29 15:43:49 1989 by kalsow         *)
(*      modified on Wed May 27 23:11:56 1987 by mbrown         *)

MODULE ASCII;

IMPORT Word;

BEGIN
  FOR c := FIRST(Upper) TO LAST(Upper) DO Upper[c] := c END;
  FOR c := 'a' TO 'z' DO
    Upper[c] := VAL (ORD (c) - ORD ('a') + ORD ('A'), CHAR);
  END;

  FOR c := FIRST(Lower) TO LAST(Lower) DO Lower[c] := c;  END;
  FOR c := 'A' TO 'Z' DO
    Lower[c] := VAL (ORD (c) - ORD ('A') + ORD ('a'), CHAR);
  END;

  FOR c := FIRST(Control) TO LAST(Control) DO
    IF (c IN Graphics)
      THEN Control[c] := VAL (Word.And (ORD (c), 31), CHAR);
      ELSE Control[c] := c;
    END;
  END;
END ASCII.
