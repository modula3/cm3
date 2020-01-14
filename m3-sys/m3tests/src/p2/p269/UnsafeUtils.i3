(* ----------------------------------------------------------------------1- *)
(* File UnsafeUtils.i3 for Modula3 compiler test p269                       *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils 

; PROCEDURE Init
    ( VAR LocVar : INTEGER
      (* This should be located omewhere near the bottom of the stack. *)
    ) 

; PROCEDURE IsInStackArea ( A : ADDRESS ) : BOOLEAN

; PROCEDURE ConstAreaOf ( Addr : ADDRESS ) : TEXT
  (* NIL means Addr not in a know Const area. *) 

; TYPE Boolish = { False , True , Unknown } 

(* EXPORTED *) 
; PROCEDURE IsDoped
   ( VAR TestAddr : ADDRESS ; READONLY KnownAddr : ADDRESS ; Length : INTEGER )
  : Boolish

; END UnsafeUtils
.



