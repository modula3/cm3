(* ----------------------------------------------------------------------1- *)
(* File Dim3.mg for Modula3 compiler test p269                              *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Dim3 ( InnerDim )

(* EXPORTED*) 
; PROCEDURE EltVal ( READONLY A : OpenTyp ; Subs : DimSubsTyp ) : EltTyp
  
    = BEGIN
        RETURN A
          [ Subs [ Depth ]
          , Subs [ Depth - 1 ]
          , Subs [ Depth - 2 ]
          ]  
      END EltVal

(* EXPORTED*) 
; PROCEDURE SetEltVal
    ( VAR A : OpenTyp ; Subs : DimSubsTyp ; EltVal : EltTyp ) 
  
    = BEGIN
        A [ Subs [ Depth ]
          , Subs [ Depth - 1 ]
          , Subs [ Depth - 2 ]
          ]
          := EltVal
      END SetEltVal

; BEGIN
  END Dim3
.


      
