(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3LInitOrder;

IMPORT M3Context, M3AST_AS;

TYPE T <: Public;
  Public = OBJECT
  METHODS
    init(c: M3Context.T; main: M3AST_AS.Compilation_Unit;
         md: MDCallback := NIL): T;
    next(VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN;
    reset();
  END;

  MDCallback = OBJECT
  METHODS
    callback(m1, m2: M3AST_AS.Module);
  END;

(* The "init" method computes the initialisation order of the modules
in "c", given that the "tmp_dependson_s" attribute has already been
computed, and that"main" has been designated the main program module.
If "md # NIL", then "md.callback" will be called for any modules that
mutually depend on each other.  

The "next" method will return the modules in the initialisation order
computed by "init".

The "reset" method resets the iterator to the beginning. *)


END M3LInitOrder.
