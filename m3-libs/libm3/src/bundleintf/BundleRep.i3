(* Copyright (C) 1992, Digital Equipment Corporation                     *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description.                        *)
(*                                                                       *)
(* Last modified on Thu Sep 10 15:00:38 PDT 1992 by mhb                  *)

(* This interface reveals the representation of a "Bundle.T". *)

INTERFACE BundleRep;

IMPORT Bundle;

REVEAL Bundle.T = BRANDED OBJECT
    METHODS
      get  (element: TEXT): TEXT;
    END;

END BundleRep.
