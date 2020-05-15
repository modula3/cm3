(* ----------------------------------------------------------------------1- *)
(* File Common.i3 for Modula3 compiler test p280                            *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Common

; PROCEDURE Init ( )

; PROCEDURE Case ( )

; PROCEDURE Failure ( Msg : TEXT ) 

; PROCEDURE Message ( Msg : TEXT ) 

; PROCEDURE Report ( )

; TYPE Array1DFixed = ARRAY [ 0 .. 2 ] OF INTEGER
; TYPE Array2DFixed = ARRAY [ 0 .. 2 ] OF Array1DFixed
; TYPE Array3DFixed = ARRAY [ 0 .. 2 ] OF Array2DFixed

; TYPE Array1DOpen = ARRAY OF INTEGER  
; TYPE Array2DOpen = ARRAY OF Array1DOpen 
; TYPE Array3DOpen = ARRAY OF Array2DOpen 

; PROCEDURE ArrayImage3D ( READONLY Val : Array3DOpen ) : TEXT

; PROCEDURE Equal3D ( READONLY Left , Right : Array3DOpen ) : BOOLEAN 

; PROCEDURE StripDope ( Value : ADDRESS ; ExpLen : INTEGER ; Label : TEXT )
  : ADDRESS 

(* The zero-t element value of these must not be the same as any open
   array length, to avoid confusing StripDope.
*)
; CONST Array3DVal = Array3DOpen 
    {
      Array2DOpen 
        {
          Array1DOpen { 1476332 , 11 , 12 }
        , Array1DOpen { 15 , 20 , 23 }
        , Array1DOpen { 37 , 51 , 72 }
        }
    , Array2DOpen 
        {
          Array1DOpen { 6 , 24 , 90 }
        , Array1DOpen { 87 , 39 , 14 }
        , Array1DOpen { 21 , 1945 , 1776 }
        }
    , Array2DOpen 
        {
          Array1DOpen { 1999 , 332 , 497 }
        , Array1DOpen { 100 , 10 , 0 }
        , Array1DOpen { 1919 , 3768 , 1401 }
        }
    }

; CONST Array3DVal2 = Array3DOpen 
    {
      Array2DOpen 
        {
          Array1DOpen { 9993764 , 11 , 12 }
        , Array1DOpen { 17 , 20 , 23 }
        , Array1DOpen { 37 , 51 , 72 }
        }
    , Array2DOpen 
        {
          Array1DOpen { 6 , 24 , 90 }
        , Array1DOpen { 87 , 1620 , 14 }
        , Array1DOpen { 21 , 1945 , 1776 }
        }
    , Array2DOpen 
        {
          Array1DOpen { 1999 , 332 , 497 }
        , Array1DOpen { 100 , 10 , 0 }
        , Array1DOpen { 1919 , 3768 , 1410 }
        }
    }

; END Common
.

