INTERFACE M3CBackEnd_C_cc;

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

(* This interface defines the size and alignment information that is needed
   by the C back-end.  Units are in BITS.
*)

VAR
  (* alignment of generic things of the given size with typical defaults *)

  a64: INTEGER; (* := 32 *)
  a32: INTEGER; (* := 32 *)
  a16: INTEGER; (* := 16 *)
  a8: INTEGER;  (* := 8 *)

  minAlignment: INTEGER; (* := 8 *)
  recAlignment: INTEGER; (* := 8 *)
  arrayAlignment: INTEGER; (* := 8 *)

  (* type-dependent size/alignment, with typical defaults *)

  ptrA: INTEGER; (*  = a32; *)
  ptrS: INTEGER; (* = 32; *)
  realA: INTEGER; (* = a32; *)
  realS: INTEGER; (* = 32; *)
  longRealA: INTEGER; (* = a64; *)
  longRealS: INTEGER; (* = 64; *)
  intA: INTEGER; (* = a32; *)
  intS: INTEGER; (* = 32; *)

  target: TEXT;  (* name of current target *)

(* Support for registering/looking up targets *)

TYPE
  TargetInitProc = PROCEDURE() RAISES {};
  (* one of these sets the above variables appropriately *)

PROCEDURE RegisterTarget(t: TEXT; p: TargetInitProc) RAISES {};
(* associate 'p' with target named 't'. *)

PROCEDURE LookupTarget(
    t: TEXT; 
    VAR (*out*) rp: REF TargetInitProc): BOOLEAN RAISES {};
(* look up 't' and return TRUE and set 'p' if found, else return FALSE. *)

TYPE
  Iter <: REFANY;

PROCEDURE NewIter(): Iter RAISES {};
(* Create an iterator on the registered targets. *)

PROCEDURE Next(
    iter: Iter; 
    VAR (*out*) t: TEXT; 
    VAR (*out*) rp: REF TargetInitProc
    ): BOOLEAN RAISES {};
(* Return the next target, or FALSE if exhausted. *)

END M3CBackEnd_C_cc.
