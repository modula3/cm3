(* ----------------------------------------------------------------------1- *)
(* File ArrayModes.ig for Modula3 compiler test p280                        *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC INTERFACE ArrayModes ( Actuals , Formals )  

; CONST Label = "Open depths Actual: " & Actuals . Label
                & ", Formal: " & Formals . Label 

; PROCEDURE TestVALUE ( )

; PROCEDURE TestVAR ( )

; PROCEDURE TestREADONLY ( )

; PROCEDURE TestAll ( )

; END ArrayModes
.

