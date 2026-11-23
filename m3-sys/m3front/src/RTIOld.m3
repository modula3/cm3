(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Mon Nov 21 08:21:27 PST 1994 by kalsow  *)
(*      modified on Fri Apr  9 09:54:23 PDT 1993 by muller  *)

(* Support for using older RTIO. *)

UNSAFE MODULE RTIOld;
IMPORT RTIO;

PROCEDURE PutRef (r: REFANY) =
  BEGIN
    RTIO.PutAddr (LOOPHOLE (r, ADDRESS), 0);
  END PutRef;

BEGIN
END RTIOld.
