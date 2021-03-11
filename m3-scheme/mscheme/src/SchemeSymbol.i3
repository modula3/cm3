(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeSymbol;
IMPORT Atom;
IMPORT SchemeObject;

TYPE T = Atom.T;

PROCEDURE SymEq(a : SchemeObject.T; b : TEXT) : BOOLEAN;
  (* check if a symbol (is a symbol and) is equal to a TEXT *)

(* check is in Scheme because of the import order *)

CONST Symbol = Atom.FromText;

CONST FromText = Symbol;

CONST ToText = Atom.ToText;

CONST Brand = "SchemeSymbol";

END SchemeSymbol.
