INTERFACE M3LTypeEquiv;

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

IMPORT M3AST_AS;


PROCEDURE Initialize() RAISES {};
(* Initialise the type equivalence algorithm. *)

PROCEDURE Add(t: M3AST_AS.TYPE_SPEC) RAISES {};
(* Use 'Add' to notify the algorithm of all type specs making up the the set
which is to be processed. This set must be complete in the sense that if the
user adds type spec 't' to the set all component types of 't' must also be
added (e.g. if 't' is a ref type its referent type must be added as well).
  One way to 'Add' all type specs in a program is to use the tree walker. If
the tree walker is used there are some special cases to look out for:
1) Array types. 'Add' must be given arrays in normalised form. As the
normalised form of an array type is a semantic attribute the tree walker will
not walk it directly - the user will have to write a little extra code. *)


TYPE
  TypeSpecArray = ARRAY OF M3AST_AS.TYPE_SPEC;

PROCEDURE Partition(): REF TypeSpecArray RAISES {};
(* When 'Add' has been used to build a full set of type specs 'Partition' can
be called to partition this set into sub sets of equivalent type specs. When
'Partition' completes all type specs in the set will have their type code
fields set. Type specs with the same type code represent equivalent types.
  The type codes will all be in the range 0..n-1 where n is the number of
different types in the given set. The result of 'Partition' is an array in
which each entry is a different type. The typecode of entry n in the array
will be n.
  Opaque types are special. Both the opaque type and its concrete type will
be given the same type code. If an opaque type has type code n then the entry
n in the result array will be the opaque type; to get to the concrete type it
is necessary to look at the opaque types 'sm_concrete_type_spec' field. *)

END M3LTypeEquiv.
