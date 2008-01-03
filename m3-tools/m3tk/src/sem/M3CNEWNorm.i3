INTERFACE M3CNEWNorm;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST, M3AST_AS;

PROCEDURE Set(n: M3AST.NODE; unit_id: M3AST_AS.UNIT_ID) RAISES {};
(* If 'n' is a call to NEW creates an auxiliary attribute
that implements the method bindings in terms of the OVERRIDES
desugaring. 'unit_id' is used to set 'tmp_unit_id' on the new
created Object_type - it should be the unit containing 'n'. 
This computation depends on 'sm_def' and 'sm_type_spec' (pass1). *)

END M3CNEWNorm.
