(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CTextcatTrans;

IMPORT M3Context, M3AST_AS;

PROCEDURE Set(c: M3Context.T; cu: M3AST_AS.Compilation_Unit);
(* Transform all "&" operators into calls on "Text.Cat". 
   The "Text" interface must be in "c", else a check runtime error *)

END M3CTextcatTrans.
