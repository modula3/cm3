(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3LMethodTable;

IMPORT M3Context;

PROCEDURE Set(c: M3Context.T) RAISES {};
(* Compute the method tables (pl_method_table) for each object type in 'c'.
   This depends on the M3ASTNext.ObjectMethod iterator, which means that
   M3CConcTypeSpec.CurrentReveal must return the concrete revelation, which,
   in turn, requires the 'tmp_rev_type_spec' field of opaque types to be
   set to the value of 'sm_concrete_type_spec'.
 *)

END M3LMethodTable.
