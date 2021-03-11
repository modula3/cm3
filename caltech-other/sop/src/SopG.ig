(*                                                                           *)
(*  Sop.i3                                                                   *)
(*                                                                           *)
(*  S-O-P expressions.                                                       *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

GENERIC INTERFACE SopG(SopLiteral, Bool, BoolTextTable);
IMPORT SopFormatStyle;
IMPORT Word;

(* boolean sum-of-products manipulations *)
(* more specific than Bools *)

(* we can do all the usual manipulations on Sop.T's, and they maintain *)
(* their sum-of-products structure (except for trivial simplifications) *)

(* The format() method returns a string suitable for printing *)
(* the literal names are found in a symbol (really a name) table *)
(* of type BoolTextTable.T --- provided to map from Bool.T's to TEXT names *)
TYPE
  Literal = SopLiteral.T;
  T <: Public;

  (* a MapProc is a procedure that can map each literal to a Sop expression *)
  (* used, e.g., to convert a Sop on bits to a Sop on rails *)

  MapProc = PROCEDURE ( context : REFANY; literal : Literal ) : T;
  SimpleMapProc = PROCEDURE ( literal : Literal ) : T;

  (* final stage is mapping aliases, just a text to text mapping *)
  AliasMapper = OBJECT METHODS canon(txt : TEXT) : TEXT END;

  Public = OBJECT
  METHODS
    init( from : Bool.T ) : T; (* initialize from a --literal-- *)
    format(symTab : BoolTextTable.T;
           READONLY style := SopFormatStyle.C;
           pfx := "";
           inQuotes := FALSE;
           aliasMapper : AliasMapper := NIL) : TEXT;
    toBool() : Bool.T;

    (* the context is a REFANY interpreted by the map procedure as it
       wishes.  this could also be done with a global in the map procedure's
       module and a mutex, but this seems better... *)

    map( context : REFANY; mapProc : MapProc ) : T;

    mapSimple(mapProc : SimpleMapProc) : T; (* no context *)

    (* we may carry out any operations we please under the invariant, *)
    (* but we may only weaken under the disjunctiveInvariant *)
    invariantSimplify( invariant,
                       disjunctiveInvariant, 
                       eventualInvariant : Bool.T) : T;
  END;

VAR IdentityMapper (* CONST *) : AliasMapper;

PROCEDURE Simplify(aOld : T) : T;
PROCEDURE And(READONLY a, b : T) : T;
PROCEDURE Or(READONLY a, b : T) : T;
PROCEDURE Not(READONLY a : T) : T;
PROCEDURE False() : T;
PROCEDURE True() : T;

(* convert a Bool.T to a Sop.T without losing the meaning *)
PROCEDURE ConvertBool(bool : Bool.T) : T;

CONST
  Brand = "Sop Expression";

(* check two s-o-p expressions for boolean equivalence (uses BDDs) *)
PROCEDURE Equivalent(a, b : T) : BOOLEAN;

(* equal just checks for pointer equivalence, although it could be *)
(* made a bit better, e.g., by sorting the entries lexicographically *)
PROCEDURE Equal(a, b : T) : BOOLEAN;

(* hash is guaranteed to be the same for two that are Equal *)
PROCEDURE Hash(a : T) : Word.T;

TYPE
  Vector = REF ARRAY OF T;

END SopG.
