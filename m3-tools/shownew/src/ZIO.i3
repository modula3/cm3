(* Copyright (C) 1992, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Mon Feb 14 16:53:37 PST 1994 by kalsow *)

INTERFACE ZIO;
IMPORT Rd;

PROCEDURE GetInt (): INTEGER RAISES {Rd.EndOfFile};
PROCEDURE GetText (): TEXT;
PROCEDURE PutI (i: INTEGER);
PROCEDURE PutT (t: TEXT);
END ZIO.
