(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Thu Sep 10 15:02:17 PDT 1992 by mhb *)

MODULE Bundle EXPORTS Bundle, BundleRep;

PROCEDURE Get (bundle: T; element: TEXT): TEXT =
  BEGIN
    RETURN bundle.get(element)
  END Get;

PROCEDURE Elts (<* UNUSED *> bundle: T): REF ARRAY OF TEXT =
  BEGIN
    <* ASSERT FALSE *>
  END Elts;

BEGIN
END Bundle.

