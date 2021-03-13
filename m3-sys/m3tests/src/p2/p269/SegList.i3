(* ----------------------------------------------------------------------1- *)
(* File SegList.i3 for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE SegList 

; TYPE SegDescrTyp
  = RECORD
      ConstLo , ConstHi : ADDRESS
    ; VarLo , VarHi : ADDRESS
    ; Name : TEXT (* Name of owning module. *)
    ; IsInteresting : BOOLEAN 
    END

; TYPE T = SegDescrTyp (* For instantiating things. *) 

; SegListTyp = ARRAY OF SegDescrTyp

; PROCEDURE Compare ( a , b : T ) : [ - 1 .. 1 ] 

; END SegList
.



