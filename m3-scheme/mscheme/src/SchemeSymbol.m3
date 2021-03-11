(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
MODULE SchemeSymbol;
FROM Scheme IMPORT Object;

PROCEDURE SymEq(a : Object; b : TEXT) : BOOLEAN =
  BEGIN RETURN a = Symbol(b) END SymEq;

BEGIN END SchemeSymbol.
