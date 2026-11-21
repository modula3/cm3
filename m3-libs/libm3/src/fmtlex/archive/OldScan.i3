(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Feb 18 13:16:31 PST 1994 by kalsow     *)
(*      modified on Fri Jan  5 08:28:16 1990 by muller         *)

INTERFACE OldScan;

IMPORT Text, Word;

EXCEPTION
  BadFormat;

<*OBSOLETE*>
PROCEDURE Bool (t: Text.T): BOOLEAN RAISES {BadFormat};

<*OBSOLETE*>
PROCEDURE Int (t: Text.T): INTEGER RAISES {BadFormat};

<*OBSOLETE*>
PROCEDURE Unsigned (t: Text.T): Word.T RAISES {BadFormat};

<*OBSOLETE*>
PROCEDURE Real (t: Text.T): REAL RAISES {BadFormat};

<*OBSOLETE*>
PROCEDURE LongReal (t: Text.T): LONGREAL RAISES {BadFormat};

<*OBSOLETE*>
PROCEDURE Char (t: Text.T): CHAR RAISES {BadFormat};

END OldScan.
