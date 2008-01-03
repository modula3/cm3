(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AST_Name;

IMPORT Fmt;

PROCEDURE Null(n: NODE): TEXT RAISES {}=
  BEGIN
    RETURN "no name for node with typecode " & Fmt.Int(TYPECODE(n));
  END Null;

BEGIN

END AST_Name.
