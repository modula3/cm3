INTERFACE M3LTypeSpecToText;

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

IMPORT Wr, Thread, M3AST_AS;

PROCEDURE TypeSpec(wr: Wr.T; t: M3AST_AS.TYPE_SPEC)
    RAISES {Wr.Failure, Thread.Alerted};
(* Write a representation of "t" to "wr" *)

PROCEDURE Initialize();
(* This must be called before the first call to "TypeSpec".  It is ok
   to call it multiple times. *)
  
END M3LTypeSpecToText.
