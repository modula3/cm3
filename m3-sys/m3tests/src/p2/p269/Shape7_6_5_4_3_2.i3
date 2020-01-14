(* ----------------------------------------------------------------------1- *)
(* File Shape7_6_5_4_3_2.i3 for Modula3 compiler test p269                  *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Shape7_6_5_4_3_2

; CONST Number1 = 2 
; CONST Number2 = 3 
; CONST Number3 = 4 
; CONST Number4 = 5 
; CONST Number5 = 6 
; CONST Number6 = 7

; CONST Shape6 = ARRAY OF INTEGER 
    { Number6 , Number5 , Number4 , Number3 , Number2 , Number1 } 

; CONST Shape5 = ARRAY OF INTEGER 
    { Number5 , Number4 , Number3 , Number2 , Number1 } 

; CONST Shape4 = ARRAY OF INTEGER 
    { Number4 , Number3 , Number2 , Number1 } 

; CONST Shape3 = ARRAY OF INTEGER 
    { Number3 , Number2 , Number1 } 

; CONST Shape2 = ARRAY OF INTEGER 
    { Number2 , Number1 } 

; CONST Shape1 = ARRAY OF INTEGER 
    { Number1 } 

; CONST Shape0 = ARRAY OF INTEGER { }

; CONST FullShape = Shape6 

; END Shape7_6_5_4_3_2
.
