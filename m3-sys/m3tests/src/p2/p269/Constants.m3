(* ----------------------------------------------------------------------1- *)
(* File Constants.m3 for Modula3 compiler test p269                         *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Constants

; IMPORT Checks2_EI4_S4_4_4_4_4_4 AS Checks2
; IMPORT Common 
; IMPORT EltsI4 
; IMPORT Globals

; FROM Common IMPORT RepTyp , RepSetTyp

; TYPE A4PC4 = ARRAY [ 0 .. 3 ] OF EltsI4 . EltTyp 

; TYPE PA4PC4 = BITS 16 FOR A4PC4
; TYPE A4PA4PC4 = ARRAY [ 0 .. 3 ] OF PA4PC4

; CONST ModuleName = "Constants"

; CONST ShapeVal = ARRAY OF INTEGER { 4 , 4 } 

; CONST FactoredFixedValue = 
      A4PA4PC4 { Inner0 , Inner1 , Inner2 , Inner3 }

; CONST Inner0 = PA4PC4 { 0 , 4 , 8 ,  12 }
      ; Inner1 = PA4PC4 { 1 , 5 , 9 ,  13 }
      ; Inner2 = PA4PC4 { 2 , 6 , 10 , 14 }
      ; Inner3 = PA4PC4 { 3 , 7 , 11 , 15 }

(* Constant declared with a fixed type: *)
; CONST FixedValue = 
      A4PA4PC4
        { PA4PC4 { 0 , 4 , 8 ,  12 }
        , PA4PC4 { 1 , 5 , 9 ,  13 }
        , PA4PC4 { 2 , 6 , 10 , 14 }
        , PA4PC4 { 3 , 7 , 11 , 15 }
        }

; TYPE AOAOPC4 = ARRAY OF ARRAY OF EltsI4 . EltTyp 

(* Constant declared with an open type: *)
; CONST OpenValue = 
      AOAOPC4
        { PA4PC4 { 0 , 4 , 8 ,  12 }
        , PA4PC4 { 1 , 5 , 9 ,  13 }
        , PA4PC4 { 2 , 6 , 10 , 14 }
        , PA4PC4 { 3 , 7 , 11 , 15 }
        }

; CONST FactoredOpenValue = 
      AOAOPC4 { Inner0 , Inner1 , Inner2 , Inner3 }


; CONST FlatEltVals =
    ARRAY OF INTEGER 
      { 0 , 4 , 8 ,  12 
      , 1 , 5 , 9 ,  13 
      , 2 , 6 , 10 , 14 
      , 3 , 7 , 11 , 15
      } 

; CONST ExpRepsFixed = RepSetTyp { RepTyp . Fixed } 
; CONST ExpRepsOpen = RepSetTyp { RepTyp . OpenContig , RepTyp . OpenRemote } 

; PROCEDURE Variables ( )

  = VAR LVarInitFixed : A4PA4PC4 := FixedValue
  ; VAR LVarInitOpen : A4PA4PC4 := OpenValue
  ; VAR LVar : A4PA4PC4 
  ; VAR Junk: INTEGER
  ; BEGIN
      Junk := 23
    ; Globals . Name := "Local variable, assigned fixed"
    ; LVar := FixedValue 
    ; Checks2 . CheckConstOpen
        ( LVar
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    ; Globals . Name := "Local variable, initialized fixed"
    ; Checks2 . CheckConstOpen
        ( LVarInitFixed
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    ; Globals . Name := "Local variable, initialized open"
    ; Checks2 . CheckConstOpen
        ( LVarInitOpen
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    ; Globals . Name := "Global variable, assigned open"
    ; GVar := OpenValue 
    ; Checks2 . CheckConstOpen
        ( GVar
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    ; Globals . Name := "Global variable, initialized fixed"
    ; Checks2 . CheckConstOpen
        ( GVarInitFixed
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    END Variables 

(* Constructors as named constants: *)

; PROCEDURE Named ( )
  = BEGIN
      Globals . Name := "FixedNamedFactoredToOpenFormal"
    ; Checks2 . CheckConstOpen 
        ( FactoredFixedValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "FixedNamedToOpenFormal" 
    ; Checks2 . CheckConstOpen 
        ( FixedValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "OpenNamedToOpenFormal" 
    ; Checks2 . CheckConstOpen 
        ( OpenValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsOpen
        )
        
    ; Globals . Name := "OpenNamedFactoredToOpenFormal"
    ; Checks2 . CheckConstOpen 
        ( FactoredOpenValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsOpen
        )
        
    ; Globals . Name := "FixedNamedToFixedFormal" 
    ; Checks2 . CheckConstFixed 
        ( FixedValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "OpenNamedToFixedFormal" 
    ; Checks2 . CheckConstFixed 
        ( OpenValue
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    END Named 
 (* Constructors directly as actual parameters: *)
 
; PROCEDURE Params ( )
  = BEGIN
      Globals . Name := "FixedLitToFixedFormal" 
    ; Checks2 . CheckConstFixed 
        ( A4PA4PC4 (* Fixed. *)
            { PA4PC4 { 0 , 4 , 8 ,  12 }
            , PA4PC4 { 1 , 5 , 9 ,  13 }
            , PA4PC4 { 2 , 6 , 10 , 14 }
            , PA4PC4 { 3 , 7 , 11 , 15 }
            }
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "FixedLitToOpenFormal" 
    ; Checks2 . CheckConstOpen 
        ( A4PA4PC4 (* Fixed. *)
            { PA4PC4 { 0 , 4 , 8 ,  12 }
            , PA4PC4 { 1 , 5 , 9 ,  13 }
            , PA4PC4 { 2 , 6 , 10 , 14 }
            , PA4PC4 { 3 , 7 , 11 , 15 }
            }
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsOpen
        )

    ; Globals . Name := "OpenLitToFixedFormal" 
    ; Checks2 . CheckConstFixed 
        ( AOAOPC4 (* Open *) 
            { PA4PC4 { 0 , 4 , 8 ,  12 }
            , PA4PC4 { 1 , 5 , 9 ,  13 }
            , PA4PC4 { 2 , 6 , 10 , 14 }
            , PA4PC4 { 3 , 7 , 11 , 15 }
            }
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "OpenLitToOpenFormal" 
    ; Checks2 . CheckConstOpen 
        ( AOAOPC4 (* Open *) 
            { PA4PC4 { 0 , 4 , 8 ,  12 }
            , PA4PC4 { 1 , 5 , 9 ,  13 }
            , PA4PC4 { 2 , 6 , 10 , 14 }
            , PA4PC4 { 3 , 7 , 11 , 15 }
            }
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsOpen
        )
    END Params

(* Constructor as function return value. *)

; PROCEDURE ReturnFixed ( ) : A4PA4PC4 
  = BEGIN
      RETURN
        A4PA4PC4
          { PA4PC4 { 0 , 4 , 8 ,  12 }
          , PA4PC4 { 1 , 5 , 9 ,  13 }
          , PA4PC4 { 2 , 6 , 10 , 14 }
          , PA4PC4 { 3 , 7 , 11 , 15 }
          }
    END ReturnFixed

; PROCEDURE Returns ( )
  = VAR ReturnedVal : A4PA4PC4
  ; BEGIN
      Globals . Name := "ReturnedFixed"
    ; Checks2 . CheckConstFixed 
             ( ReturnFixed ( ) 
             , ShapeVal
             , FlatEltVals
             , ModuleName & "_M3"
             , ExpRepsFixed
             )
    ; ReturnedVal := ReturnFixed ( ) 
    ; Globals . Name := "ReturnedVariableFixed"
    ; Checks2 . CheckConstFixed 
             ( ReturnedVal
             , ShapeVal
             , FlatEltVals
             , ModuleName & "_M3"
             , ExpRepsFixed
             )
    END Returns

(* WITH-bound constructor. *)

; PROCEDURE Withs ( )
  = BEGIN
      Globals . Name := "WithFixed"
    ; WITH WithIdFixed = 
        A4PA4PC4
          { PA4PC4 { 0 , 4 , 8 ,  12 }
          , PA4PC4 { 1 , 5 , 9 ,  13 }
          , PA4PC4 { 2 , 6 , 10 , 14 }
          , PA4PC4 { 3 , 7 , 11 , 15 }
          }
      DO Checks2 . CheckConstFixed 
           ( WithIdFixed
           , ShapeVal
           , FlatEltVals
           , ModuleName & "_M3"
           , ExpRepsFixed
           )
      END
    ; Globals . Name := "WithOpen"
    ; WITH WithIdOpen = 
        AOAOPC4 (* Open *) 
          { PA4PC4 { 0 , 4 , 8 ,  12 }
          , PA4PC4 { 1 , 5 , 9 ,  13 }
          , PA4PC4 { 2 , 6 , 10 , 14 }
          , PA4PC4 { 3 , 7 , 11 , 15 }
          }
      DO Checks2 . CheckConstOpen 
           ( WithIdOpen
           , ShapeVal
           , FlatEltVals
           , ModuleName & "_M3"
           , ExpRepsOpen
           )
      END
    END Withs

(* Constructors as exception argument. *)

; EXCEPTION ExFixed ( A4PA4PC4 )

; PROCEDURE RaiseFixed ( ) RAISES { ExFixed } 
  = BEGIN
      RAISE
        ExFixed
          ( A4PA4PC4
              { PA4PC4 { 0 , 4 , 8 ,  12 }
              , PA4PC4 { 1 , 5 , 9 ,  13 }
              , PA4PC4 { 2 , 6 , 10 , 14 }
              , PA4PC4 { 3 , 7 , 11 , 15 }
              }
          ) 
    END RaiseFixed

; PROCEDURE ExceptArg ( )
  = BEGIN
      Globals . Name := "RaiseFixed"
    ; TRY
        RaiseFixed ( )
      EXCEPT
        ExFixed ( caught )
        => Checks2 . CheckConstFixed 
             ( caught
             , ShapeVal
             , FlatEltVals
             , ModuleName & "_M3"
             , ExpRepsFixed
             )
      END
    END ExceptArg 

(* Constructors as record fieldd. *)

; TYPE RecTypDefaultFixed = RECORD
                  RecField : A4PA4PC4 := FixedValue 
                END

; TYPE RecTypDefaultOpen = RECORD
                  RecField : A4PA4PC4 := OpenValue 
                END

; TYPE RecTypNoDefault = RECORD
                  RecField : A4PA4PC4
                END
; TYPE RecRefTyp = REF RecTypNoDefault 

; PROCEDURE RecFields ( )
  = VAR LRecDefFixed : RecTypDefaultFixed 
  ; VAR LRecDefOpen : RecTypDefaultOpen 
  ; VAR LRecNoDef : RecTypNoDefault
  ; VAR LRef : RecRefTyp 
  
  ; BEGIN
      Globals . Name := "Record variable with fixed default" 
    ; Checks2 . CheckConstFixed
        ( LRecDefFixed . RecField   
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
      
    ; Globals . Name := "Record variable with open default" 
    ; Checks2 . CheckConstFixed
        ( LRecDefOpen . RecField  
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )

    ; Globals . Name := "Record variable with assigned fixed field" 
    ; LRecNoDef . RecField := FixedValue 
    ; Checks2 . CheckConstFixed
        ( LRecNoDef . RecField  
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
      
    ; Globals . Name
        := "Record constructor with fixed embedded array constructor" 
    ; Checks2 . CheckConstFixed
        ( RecTypNoDefault { RecField := FixedValue } . RecField 
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
      
    ; Globals . Name
        := "Record variable assigned record constructor" 
    ; LRecNoDef := RecTypNoDefault { RecField := FixedValue } 
    ; Checks2 . CheckConstFixed
        ( LRecNoDef . RecField 
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
      
    ; Globals . Name
        := "Record heap object"
    ; LRef := NEW ( RecRefTyp , RecField := FixedValue )
    ; Checks2 . CheckConstFixed
        ( LRef ^ . RecField 
        , ShapeVal
        , FlatEltVals
        , ModuleName & "_M3"
        , ExpRepsFixed
        )
    END RecFields

; VAR GVar : A4PA4PC4
; VAR GVarInitFixed : A4PA4PC4 := FixedValue

; PROCEDURE Work ( )
  = BEGIN
      Named ( )
    ; Params ( )
    ; Returns ( )
    ; Withs ( ) 
    ; RecFields ( )
    ; Variables ( )
    ; ExceptArg ( ) 
    END Work

; PROCEDURE Init ( Marker : Common . MarkerTyp )
  (* Marker is only here as recognizable plant in the compiled module,
     for finding the beginning of its global constant area.  A use of
     it has to stay in this module, for this to work. *) 
  = VAR LMarker : Common . MarkerTyp 
  ; BEGIN
      LMarker := Marker
    ; Common . NoteModName ( ModuleName & "_M3" ) 
    END Init

; BEGIN
    Init ( Common . BegOfConstMarker ) 
  ; Init ( Common . BegOfConstMarker ) 
  END Constants
. 
