(* $Id: Example2.i3,v 1.2 2011/02/19 20:49:28 mika Exp $ *)

INTERFACE Example2;

TYPE 
  T = OBJECT METHODS
    xyz(x : LONGREAL := LAST(LONGREAL));

(*
    uvw(x : INTEGER := FIRST(INTEGER)+1); (* doesnt work with AMD64 *)
*)
  END;

END Example2.
