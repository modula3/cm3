INTERFACE M3LDepends;

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

IMPORT M3AST_AS, M3Context;

TYPE
  Closure <: Closure_public;
  Closure_public = OBJECT
    context: M3Context.T;
  METHODS
    callback(
      m: M3AST_AS.Module;
      i: M3AST_AS.Interface): BOOLEAN RAISES{};
  END;

PROCEDURE Set(c: M3Context.T; cl: Closure) RAISES {};
(* Compute the 'depends-on' relation for all the units in 'c', which
then defines the initialisation order of the modules in a program.
cl.callback is called for each unit in the context.  cl.context is set
to c before any callbacks. cl.callback should return TRUE if 'm' uses 'i'.  
'Default' returns a default closure whose callback returns TRUE if 'm' 
"uses" 'i', as defined in section 5.4 of the report.
However, since it is the "use" of another module from the initialisation
code of a module that is really significant, a clever client can do better. *)

PROCEDURE Default(): Closure RAISES {};

END M3LDepends.
