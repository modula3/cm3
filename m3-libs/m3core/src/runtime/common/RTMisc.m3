(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:27:05 PST 1994 by kalsow     *)
(*      modified on Tue May 11 16:49:30 PDT 1993 by mjordan    *)
(*      modified on Tue Apr 20 14:44:12 PDT 1993 by mcjones    *)
(*      modified on Sun Feb 21 14:59:10 PST 1993 by jdd        *)
(*      modified on Sat Jun 27 22:22:30 PDT 1992 by muller     *)

UNSAFE MODULE RTMisc;

IMPORT Cstring;

(*------------------------------- byte copying ------------------------------*)

PROCEDURE Copy (src, dest: ADDRESS;  len: INTEGER) =
  BEGIN
    EVAL Cstring.memcpy (dest, src, len);
  END Copy;

PROCEDURE Zero (dest: ADDRESS;  len: INTEGER) =
  BEGIN
    EVAL Cstring.memset (dest, 0, len);
  END Zero;

(*------------------------------- rounded arithmetic ------------------------*)

PROCEDURE Align (a: ADDRESS; y: INTEGER): ADDRESS =
  BEGIN 
    RETURN LOOPHOLE (Upper (LOOPHOLE (a, INTEGER), y), ADDRESS);
  END Align;

PROCEDURE Upper (x, y: INTEGER): INTEGER =
  BEGIN
    RETURN ((x + y - 1) DIV y) * y;
  END Upper;

BEGIN
END RTMisc.
