(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Oct  6 11:33:00 PDT 1994 by ericv         *)
(*      modified on Sat Jun 27 15:47:41 PDT 1992 by muller        *)

MODULE Utypes;

IMPORT Word;

FROM Ctypes IMPORT int;

PROCEDURE howmany (x, y: int): int =
  BEGIN
    RETURN (x + (y - 1)) DIV y;
  END howmany;

PROCEDURE FD_SET (n: int; p: UNTRACED REF fd_set): int =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      l := Word.Or (l, Word.Shift (1, n MOD NFDBITS)); 
      RETURN l; END;
  END FD_SET;

PROCEDURE FD_CLEAR (n: int; p: UNTRACED REF fd_set): int =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      l := Word.And (l, Word.Not (Word.Shift (1, n MOD NFDBITS)));
      RETURN l; END;
  END FD_CLEAR;

PROCEDURE FD_ISSET (n: int; p: UNTRACED REF fd_set): int =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      RETURN Word.And (l, Word.Shift (1, n MOD NFDBITS)); END;
  END FD_ISSET;

PROCEDURE FD_ZERO (p: UNTRACED REF fd_set) =
  BEGIN 
    WITH a = p.fds_bits DO
      FOR i := FIRST (a) TO LAST (a) DO
        a[i] := 0; END; END;
  END FD_ZERO;

BEGIN
END Utypes.
