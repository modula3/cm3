(* ----------------------------------------------------------------------1- *)
(* File Dynamic.m3 for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Dynamic

; IMPORT Checks6_EI16_S7_6_5_4_3_2 AS Checks6
; IMPORT Checks5_EI16_S7_6_5_4_3_2 AS Checks5
; IMPORT Checks4_EI16_S7_6_5_4_3_2 AS Checks4
; IMPORT Checks3_EI16_S7_6_5_4_3_2 AS Checks3
; IMPORT Checks2_EI16_S7_6_5_4_3_2 AS Checks2
; IMPORT Common 
; IMPORT EltsI16 
; IMPORT Globals
; IMPORT Support 

; FROM Common IMPORT RepTyp , RepSetTyp

; CONST ModuleName = "Dynamic"

; CONST ShapeVal = ARRAY OF INTEGER { 7 , 6 , 5 , 4 , 3 , 2 }

; VAR FlatNumber : INTEGER 
; FlatValsRef : REF ARRAY OF INTEGER 
; VAR Arr : Checks6 . FixedTyp 

(* ---------------- OLD -------------------------------- **

; TYPE A4PC4 = ARRAY [ 0 .. 3 ] OF EltsI4 . EltTyp 

; TYPE PA4PC4 = BITS 16 FOR A4PC4
; TYPE A4PA4PC4 = ARRAY [ 0 .. 3 ] OF PA4PC4

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

(* Naming convention for dimensions:
     D: Open representation, dynamic size
     S: Open representation, static size
     F: Fixed representation (which implies static size.)
*)

; TYPE REF SSDDFF = REF Dim6 . OpenType

; PROCEDURE HeapSSDDFF ( ) : REF Dim6 . OpenType 
  = VAR V4 := 4 
  ; VAR V5 := 5
  ; Result : Dim6 . OpenType 
  
  ; BEGIN
      Result := NEW ( Dim6 . OpenType ) 
    END SSDDFF

; PROCEDURE Variables ( )
  = VAR
  ; BEGIN
    END Variables 

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

** --------------------- End OLD ---------------------------- *)
; CONST BaseBias = 100 

; TYPE Typ32 = Checks2 . FixedTyp
; TYPE TypOO32 = ARRAY OF ARRAY OF Typ32
; TYPE TypROO32 = REF TypOO32
; TYPE Typ76ROO32 = ARRAY Checks6 . SsTyp OF ARRAY Checks5 . SsTyp OF TypROO32
; VAR  Var76ROO32 : Typ76ROO32

; PROCEDURE FillArrRefs ( VAR V : Typ76ROO32 )

  = VAR Bias , Number4_1 : INTEGER
  ; VAR FlatVals4_1 : Checks4 . FixedTyp
  ; VAR Number5 : INTEGER := Checks5 . Number
  
  ; BEGIN
      WITH WShape4_1 = SUBARRAY ( ShapeVal , 2 , 4 )
      DO 
        Number4_1 := Common . IntProduct ( WShape4_1 ) 
      ; FOR I6 := FIRST ( Checks6 . SsTyp ) TO LAST ( Checks6 . SsTyp ) 
        DO FOR I5 := FIRST ( Checks5 . SsTyp ) TO LAST ( Checks5 . SsTyp )
          DO WITH W = V [ I6 , I5 ]
            DO
              W := NEW ( TypROO32 , Checks4 . Number , Checks3 . Number )
            ; Bias := ( ( I6 * Number5 + I5 ) * Number4_1 ) 
            ; Checks4 . FillEltVals
                ( (*VAR*) W ^
                , WShape4_1
                , SUBARRAY ( FlatValsRef ^ , Bias , Number4_1 )
                , Bias := 0  
                ) 
            END (*WITH*) 
          END (*FOR*) 
        END (*FOR*)
      END (*WITH WShape4_1*)
    END FillArrRefs 

; PROCEDURE OpenSSDD32_a ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp

  ; BEGIN
      Globals . Name := "Dynamic, depth 2 & 3."
    ; Checks6 . Check
        ( T6 { T5 { V [0,0]^,V [0,1]^,V [0,2]^,V [0,3]^,V [0,4]^,V [0,5]^ } 
             , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
             , T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ } 
             , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
             , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
             , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
             , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
             }
        , ShapeVal
        , FlatValsRef ^ 
        )
    END OpenSSDD32_a 

; TYPE TypOOO32 = ARRAY OF TypOO32 (* OOO32 *)
; TYPE TypROOO32 = REF TypOOO32
; TYPE Typ6ROO32 = ARRAY Checks5 . SsTyp OF TypROO32
; VAR  Var6ROO32 : Typ6ROO32

; PROCEDURE OpenSSDD32_b ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp
  ; VAR O5 : TypROOO32 

  ; BEGIN
      Globals . Name := "Dynamic, depth 2 & 3, One at level 2."
    ; O5 := NEW ( TypROOO32 , 6 , 5 , 4 ) 
    ; O5 ^ := T5 { V [0,0]^,V [0,1]^,V [0,2]^,V [0,3]^,V [0,4]^,V [0,5]^ }
    ; Checks6 . Check
        ( T6 { O5 ^  
             , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
             , T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ } 
             , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
             , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
             , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
             , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
             }
        , ShapeVal
        , FlatValsRef ^ 
        )
        
    END OpenSSDD32_b 

; PROCEDURE OpenSSDD32_c ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp
  ; VAR O5 : TypROOO32 

  ; BEGIN
      Globals . Name := "Dynamic, depth 2 & 3, One at level 2."
    ; O5 := NEW ( TypROOO32 , 6 , 5 , 4 ) 
    ; O5 ^ := T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ }
    ; Checks6 . Check
        ( T6 { T5 { V [0,0]^,V [0,1]^,V [0,2]^,V [0,3]^,V [0,4]^,V [0,5]^ } 
             , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
             , O5 ^ 
             , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
             , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
             , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
             , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
             }
        , ShapeVal
        , FlatValsRef ^ 
        )
        
    END OpenSSDD32_c

; TYPE Shape5Typ = ARRAY [ 0 .. 4 ] OF INTEGER 

; PROCEDURE OpenSSDDFFSubsShape_a ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp
  ; VAR O5 , O5Changed: TypROOO32

  ; PROCEDURE MustFail ( )
    = BEGIN
        Checks6 . JustAccept 
          ( T6 { T5 { V [0,0]^,V [0,1]^,V [0,2]^,V [0,3]^,V [0,4]^,V [0,5]^ } 
               , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
               , O5Changed ^ 
               , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
               , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
               , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
               , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
               }
          )
      END MustFail 

  ; BEGIN
      O5 := NEW ( TypROOO32 , 6 , 5 , 4 ) 
    ; O5Changed := NEW ( TypROOO32 , 6 , 5 , 3 ) 
    ; O5 ^ := T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ }
    ; Checks5 . ChangeShape
       ( O5 ^
       , O5Changed ^
       , Shape5Typ { 6 , 5 , 4 , 3 , 2 }
       , Shape5Typ { 6 , 5 , 3 , 3 , 2 }
       )
    ; Support . StartTest
        ( "Dynamic shape mismatch, deepest dynamic, "  & Globals . Name )
    ; Support . TestMustFail ( MustFail , " RT error not detected." )
    ; Support . EndTest ( ) 
    END OpenSSDDFFSubsShape_a 

; PROCEDURE OpenSSDDFFSubsShape_b ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp
  ; VAR O5 , O5Changed: TypROOO32

  ; PROCEDURE MustFail ( )
    = BEGIN
        Checks6 . JustAccept 
          ( T6 { T5 { V [0,0]^,V [0,1]^,V [0,2]^,V [0,3]^,V [0,4]^,V [0,5]^ } 
               , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
               , O5Changed ^ 
               , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
               , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
               , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
               , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
               }
          )
      END MustFail 

  ; BEGIN
      O5 := NEW ( TypROOO32 , 6 , 5 , 4 ) 
    ; O5Changed := NEW ( TypROOO32 , 5 , 5 , 4 ) 
    ; O5 ^ := T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ }
    ; Checks5 . ChangeShape
       ( O5 ^
       , O5Changed ^
       , Shape5Typ { 6 , 5 , 4 , 3 , 2 }
       , Shape5Typ { 5 , 5 , 4 , 3 , 2 }
       )
    ; Support . StartTest
        ( "Dynamic shape mismatch, deepest static, "  & Globals . Name )
    ; Support . TestMustFail ( MustFail , " RT error not detected." )
    ; Support . EndTest ( ) 
    END OpenSSDDFFSubsShape_b 

; TYPE Typ432 = ARRAY [ 0 .. 3 ] OF Typ32 
; TYPE TypOO432 = ARRAY OF ARRAY OF Typ432 
; TYPE TypROO432 = REF TypOO432

; PROCEDURE OpenSSDFFFSubsShape ( VAR V : Typ76ROO32 )
  = TYPE T6 = Checks6 . OpenTyp 
  ; TYPE T5 = Checks5 . OpenTyp
  ; VAR O5 : TypROOO32
  ; VAR O5Fixed: TypROO432
  ; VAR O5Changed: TypROOO32

  ; PROCEDURE MustFail ( )
    = BEGIN
        Checks6 . JustAccept 
          ( T6 { O5Fixed ^ 
               , T5 { V [1,0]^,V [1,1]^,V [1,2]^,V [1,3]^,V [1,4]^,V [1,5]^ } 
               , O5Changed ^ 
               , T5 { V [3,0]^,V [3,1]^,V [3,2]^,V [3,3]^,V [3,4]^,V [3,5]^ } 
               , T5 { V [4,0]^,V [4,1]^,V [4,2]^,V [4,3]^,V [4,4]^,V [4,5]^ } 
               , T5 { V [5,0]^,V [5,1]^,V [5,2]^,V [5,3]^,V [5,4]^,V [5,5]^ } 
               , T5 { V [6,0]^,V [6,1]^,V [6,2]^,V [6,3]^,V [6,4]^,V [6,5]^ }
               }
          )
      END MustFail 

  ; BEGIN
      O5 := NEW ( TypROOO32 , 6 , 5 , 4 ) 
    ; O5Changed := NEW ( TypROOO32 , 6 , 5 , 3 ) 
    ; O5Fixed := NEW ( TypROO432 , 6 , 5 ) 
    ; O5 ^ := T5 { V [2,0]^,V [2,1]^,V [2,2]^,V [2,3]^,V [2,4]^,V [2,5]^ }
    ; Checks5 . ChangeShape
       ( O5 ^
       , O5Changed ^
       , Shape5Typ { 6 , 5 , 4 , 3 , 2 }
       , Shape5Typ { 6 , 5 , 3 , 3 , 2 }
       )
    ; O5Fixed ^ := O5 ^ 
    ; Support . StartTest
        ( "Open-static shape mismatch, shallowest fixed, "  & Globals . Name )
    ; Support . TestMustFail ( MustFail , " RT error not detected." )
    ; Support . EndTest ( ) 
    END OpenSSDFFFSubsShape 

; PROCEDURE Init ( )
  = BEGIN
      FlatNumber := Common . IntProduct ( ShapeVal )
    ; FlatValsRef := NEW ( REF ARRAY OF INTEGER , FlatNumber )
    ; Checks6 . FillFlatA ( FlatValsRef ^ , ShapeVal , BaseBias )
    END Init

(*EXPORTED*)
; PROCEDURE Work ( )
  = VAR Breakpoint : INTEGER := 0 
  ; BEGIN
      Init ( )
    ; Checks6 . FillEltVals ( Arr , ShapeVal , FlatValsRef ^ , Bias := 0 ) 
    ; Checks6 . CheckEltVals ( Arr , ShapeVal , FlatValsRef ^ )
    ; Support . Flush ( ) 
    ; Breakpoint := 17
    ; FillArrRefs ( Var76ROO32 ) 
    ; Breakpoint := 19
    ; OpenSSDD32_a ( Var76ROO32 ) 
    ; Breakpoint := 21
    ; OpenSSDD32_b ( Var76ROO32 )
    ; Breakpoint := 23 
    ; OpenSSDD32_c ( Var76ROO32 ) 
    ; Breakpoint := 29
    ; Globals . Name := "SSDDFF"
    ; OpenSSDDFFSubsShape_a ( Var76ROO32 )
    ; Breakpoint := 31 
    ; OpenSSDDFFSubsShape_b ( Var76ROO32 )
    ; Breakpoint := 37
    ; Globals . Name := "SSDFFF"
    ; OpenSSDFFFSubsShape ( Var76ROO32 )
    ; Breakpoint := 41
    END Work

; BEGIN
  END Dynamic
. 
