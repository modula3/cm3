(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE CharCodes;
IMPORT Rd;

<*OBSOLETE*> PROCEDURE ParseChar(t: TEXT; VAR pos: INTEGER): CHAR;
PROCEDURE GetChar(rd: Rd.T): CHAR RAISES {Rd.EndOfFile};
(* get char, interpreting \ codes, and advance pos *)

PROCEDURE StripDelims(t: TEXT): TEXT; (* remove first and last character *)
PROCEDURE ParseString(t: TEXT): TEXT; (* "expand" \ codes *)

PROCEDURE FmtChar(c: CHAR): TEXT; (* add \ if needed *)
PROCEDURE Q(t: TEXT): TEXT; (* put string in quotes, and add backslashes *)
PROCEDURE QC(c: CHAR): TEXT; (* put char in single quotes, and add \ *)

END CharCodes.
