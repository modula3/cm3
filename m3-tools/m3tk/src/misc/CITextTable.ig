(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:55:42 PST 1994 by detlefs   *)

GENERIC INTERFACE CITextTable(Tbl);
(* Where "Tbl" equals "Table(Text, Value)" *)

TYPE 
  T = Tbl.T;
  Iterator = Tbl.Iterator;
  Default <: Tbl.Default;

(* A "CITextTable(Tbl)" is a "Table(Text.T, Value).T", with the
"keyEqual" and "keyHash" methods overriden with case-insensitive
procedures. *)

END CITextTable.
 
