(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Sep 24 09:53:11 PDT 1993 by kalsow    *)
(*      modified on Tue Nov 19 11:20:46 PST 1991 by muller    *)

MODULE Utypes;

IMPORT Word;
FROM Ctypes IMPORT int;

PROCEDURE major (x: int): int =
  BEGIN 
    RETURN Word.And (Word.Shift (x, -8), 8_0377);
  END major;

PROCEDURE minor (x: int): int =
  BEGIN 
    RETURN Word.And (x, 8_0377);
  END minor;

PROCEDURE makedev (x, y: int): dev_t =
  BEGIN 
    RETURN Word.Or (Word.Shift (x, 8), y);
  END makedev;

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
