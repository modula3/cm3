(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar 18 21:56:47 PST 1996 by detlefs                  *)

(* An "PropVar.T" is a proposition variable. *)

INTERFACE PropVar;

IMPORT AF;
IMPORT Atom;
IMPORT RefList;

TYPE
  T <: AF.T;

PROCEDURE Init();
(* Must be called before any other procedure of this interface. *)

PROCEDURE New(sym: Atom.T): T;
(* Return the "PropVar" representing the propositional variable "sym". *)

PROCEDURE Push();
(* Save the state. *)

PROCEDURE Pop();
(* Undo the creation of any "T"'s since the last "Push". *)

PROCEDURE Top(): RefList.T;
(* Returns a list representing a conjunction of asserted or denied
   propositional variables. *)

END PropVar.
