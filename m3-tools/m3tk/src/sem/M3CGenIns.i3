INTERFACE M3CGenIns;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


IMPORT M3AST_AS;

PROCEDURE Set(
    cu_ins, cu_def: M3AST_AS.Compilation_Unit
    ): M3AST_AS.Compilation_Unit
    RAISES {};
(* Given that 'cu_ins' is a generic instance and that 'cu_def'
   is the corresponding generic definition, perform the instantiation
   and return the resulting AST. The instantation is achieved by
   copying the generic definition and then inserting 
   "IMPORT Ai AS Fi,...", where 1 <= 'i' <= N and 'N' is the number
   of formal generic parameters. It is a static error if the
   number of actual parameters does not equal 'N', and this is
   reported as an error on 'cu_ins', and a result of NIL. 
*)

END M3CGenIns.
