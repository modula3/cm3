(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Aug 20 12:19:37 PDT 1993 by kalsow    *)
(*      modified on Sat Jun 27 15:47:41 PDT 1992 by muller    *)

MODULE Utypes;

IMPORT Word;

PROCEDURE major (x: dev_t): major_t =
  BEGIN 
    RETURN Word.And (Word.Shift (x, -20), 8_07777);
  END major;

PROCEDURE minor (x: dev_t): minor_t =
  BEGIN 
    RETURN Word.And (x, 8_03777777);
  END minor;

PROCEDURE makedev (x: major_t;   y: minor_t): dev_t =
  BEGIN 
    RETURN Word.Or (Word.Shift (x, 20), y);
  END makedev;

PROCEDURE howmany (x, y: INTEGER): INTEGER =
  BEGIN
    RETURN (x + (y - 1)) DIV y;
  END howmany;

PROCEDURE FD_SET (n: INTEGER; p: UNTRACED REF fd_set): INTEGER =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      l := Word.Or (l, Word.Shift (1, n MOD NFDBITS)); 
      RETURN l;
    END;
  END FD_SET;

PROCEDURE FD_CLEAR (n: INTEGER; p: UNTRACED REF fd_set): INTEGER =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      l := Word.And (l, Word.Not (Word.Shift (1, n MOD NFDBITS)));
      RETURN l;
    END;
  END FD_CLEAR;

PROCEDURE FD_ISSET (n: INTEGER; p: UNTRACED REF fd_set): INTEGER =
  BEGIN 
    WITH l = p.fds_bits [n DIV NFDBITS] DO
      RETURN Word.And (l, Word.Shift (1, n MOD NFDBITS));
    END;
  END FD_ISSET;

PROCEDURE FD_ZERO (p: UNTRACED REF fd_set) =
  BEGIN 
    WITH a = p.fds_bits DO
      FOR i := FIRST (a) TO LAST (a) DO  a[i] := 0;  END;
    END;
  END FD_ZERO;

BEGIN
END Utypes.
