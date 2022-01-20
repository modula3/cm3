(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Sep 27 11:02:16 PDT 1994 by kalsow     *)

INTERFACE M3Backend;

IMPORT Wr, M3CG, M3ID;
IMPORT Target;

PROCEDURE Open (library (* or program *): TEXT;
                source_base_name (* lacks .m3 or .i3 *): M3ID.T;
                target_wr: Wr.T;
                target_base_name (* Has suffix*): TEXT;
                f_ir_name (* Has suffix .ic or .mc *): TEXT;
                backend_mode: Target.M3BackendMode_t
               ): M3CG.T;

PROCEDURE Close (cg: M3CG.T);

END M3Backend.
