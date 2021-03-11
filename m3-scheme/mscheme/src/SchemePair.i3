(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemePair;
IMPORT Wx;
IMPORT SchemeObject;
FROM Scheme IMPORT E;
IMPORT RefSeq;

TYPE
  T = BRANDED Brand REF RECORD
    first, rest : SchemeObject.T;
  END;

PROCEDURE StringifyPair(t : T; quoted : BOOLEAN; buf : Wx.T; seen : RefSeq.T := NIL)  RAISES { E };

PROCEDURE Pair(t : SchemeObject.T) : T RAISES { E };
  (* unlike most of the coercion functions, Pair does NOT check for NIL.
     NIL is considered a legal "Pair", as it is a legal list. *)

CONST Brand = "SchemePair";

END SchemePair.
