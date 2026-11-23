(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Nov 18 15:37:18 PST 1994 by kalsow  *)
(*      modified on Sat Oct  6 02:01:36 1990 by muller      *)

(* Support for using older RTIO. *)

INTERFACE RTIOld;

PROCEDURE PutRef (a: REFANY);
(* == PutAddr (LOOPHOLE (a, ADDRESS)) *)

END RTIOld.
