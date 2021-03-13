(* ----------------------------------------------------------------------1- *)
(* File Tests.ig for Modula3 compiler test p269                             *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Tests ( Checks , Values )

; IMPORT Globals 

; PROCEDURE Work ( )
  = BEGIN
      Globals . Name := Values . Name 
    ; Checks . CheckConst 
        ( Values . Value
        , Values . ShapeVal
        , Values . FlatEltVals
        , Values . ExpReps
        )
    END Work

; BEGIN
  END Tests
.

