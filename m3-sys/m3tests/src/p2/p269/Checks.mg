(* ----------------------------------------------------------------------1- *)
(* File Checks.mg for Modula3 compiler test p269                            *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

GENERIC MODULE Checks ( Dim , InnerChecks )
(* Check procedures for Array constructors. *)

(* Dim must declare:
     Depth       : INTEGER  (* Number of dimensions. *)
     EltTyp      = Type of non-array elements,  Assignable to INTEGER
     SsTyp       = Type of subscripts. 
     OpenTyp     = Depth-dimensional open array type of EltTyp
     FixedTyp    = Depth-dimensional fixed array type of EltTyp
        When Depth = 0, OpenTyp and FixedTyp are EltTyp
*)

(* InnerChecks must declare:
     PROCEDURE CheckEltValsHelper
     PROCEDURE CheckNumber
   (* This is tricky.  InnerChecks is a different instantiation of Checks,
      with its Depth = (our Depth) - 1. *)
*)

; IMPORT Fmt
; IMPORT Text

; IMPORT Common
; IMPORT Globals 
; FROM Common IMPORT RepTyp , RepSetTyp , RepSetTypDescr 
; FROM Support IMPORT StartTest , NoteFailure , EndTest 
; FROM UnsafeUtils IMPORT IsInStackArea , ConstAreaOf  

; PROCEDURE Init ( Marker : Common . MarkerTyp )
  (* Marker is here, only as a recognizable plant in the compiled module,
     for finding the beginning of its global constant area.  A use of
     it has to stay in this module, for this to work. *) 
  = VAR LMarker : Common . MarkerTyp 
  ; BEGIN
      LMarker := Marker 
    END Init
    
(*EXPORTED:*)
; PROCEDURE JustAccept ( <*UNUSED*> READONLY Arr : OpenTyp )
  = VAR Breakpoint : INTEGER 
  ; BEGIN
      Breakpoint := 11 
    END JustAccept 

(*EXPORTED*) 
; PROCEDURE ChangeShapeHelper
    ( VAR FromSubs , ToSubs : Common . FullSubsTyp
    ; <*NOWARN*> FromShape , ToShape : ARRAY OF INTEGER 
    ; EltProc : CopyProcTyp
    ) 
  = VAR FI , FNum , TI , TNum : INTEGER
  ; BEGIN
      FI := 0
    ; TI := 0
    ; FNum := FromShape [ 0 ] 
    ; TNum := ToShape [ 0 ] 
    ; LOOP
        IF TI >= TNum THEN EXIT END
      ; FromSubs [ Dim . Depth ] := FI 
      ; ToSubs [ Dim . Depth ] := TI 
      ; InnerChecks . ChangeShapeHelper
          ( FromSubs , ToSubs
          , SUBARRAY ( FromShape , 1 , Dim . Depth - 1 ) 
          , SUBARRAY ( ToShape , 1 , Dim . Depth - 1 ) 
          , EltProc
          ) 
      ; IF FI < FNum - 1 THEN INC ( FI ) END 
      ; INC ( TI ) 
      END (*LOOP*)
    END ChangeShapeHelper

(*EXPORTED:*)
; PROCEDURE ChangeShape
    ( READONLY From : OpenTyp
    ; VAR To : OpenTyp
    ; <*NOWARN*> FromShape , ToShape : ARRAY OF INTEGER
    )
  = VAR FromSubs , ToSubs : Common . FullSubsTyp 

  ; PROCEDURE EltProc ( READONLY FromSubs , ToSubs : Common . FullSubsTyp ) 
    (* ^To provide From and To, whose type varies with Depth, for subscripting
       all in one place, once all subscript values are known. *)
        
    = VAR Elt : Dim . EltTyp
    ; BEGIN
        Elt := Dim . EltVal ( From , SUBARRAY ( FromSubs , 0 , Dim . Depth ) )
      ; Dim . SetEltVal ( To , SUBARRAY ( ToSubs , 0 , Dim . Depth ) , Elt )
      END EltProc

  ; BEGIN (* ChangeShape *) 
      IF Dim . Depth > 0
      THEN
        ChangeShapeHelper ( FromSubs , ToSubs , FromShape , ToShape , EltProc )
      END (*IF*)
    END ChangeShape 

(*EXPORTED:*)
; PROCEDURE CheckNumber
    ( READONLY Arr : Dim . OpenTyp ; READONLY Shape : ARRAY OF INTEGER )

  = VAR ThisDimNumber , Last , Exp : INTEGER

  ; BEGIN
      <* ASSERT NUMBER ( Shape ) = Dim . Depth *>
      StartTest ( "NUMBER values of " & Globals . Name )
    ; IF Dim . Depth > 0
      THEN
        ThisDimNumber := NUMBER ( Arr )
      ; Last := LAST ( Arr )
      ; <* ASSERT ThisDimNumber = Last + 1 *> 
        Exp := Shape [ FIRST ( Shape ) ] 
      ; IF ThisDimNumber # Exp
        THEN
          NoteFailure
            ( "Got " & Fmt . Int ( ThisDimNumber )
              & ", expected " & Fmt . Int ( Exp ) & "."
            ) 
        ELSE
          FOR RI := 0 TO Last 
          DO
(* This requires partial subscripting, which the compiler, as of 2019-06-12,
   refuses to do when the elements are sub-byte packed.  So we can't do this: 
            InnerChecks . CheckNumber
              ( Arr [ RI ] , SUBARRAY ( Shape , 1 , Dim . Depth - 1 ) )
*) 
          END (*FOR*)
        END (*IF*)
      END
    ; EndTest ( ) 
    END CheckNumber

(*EXPORTED:*)
; PROCEDURE CheckEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; EltProc : EltProcTyp 
    ) 

  = VAR ThisDimNumber , FlatNumber , DownFlatNumber  : INTEGER

  ; BEGIN
      <* ASSERT NUMBER ( Shape ) = Dim . Depth *>
      ThisDimNumber := Shape [ 0 ] 
    ; FlatNumber := NUMBER ( FlatVals )
    ; DownFlatNumber := FlatNumber DIV ThisDimNumber  
    ; FOR RI := 0 TO ThisDimNumber - 1
      DO
        Subs [ Dim . Depth ] := RI 
      ; InnerChecks . CheckEltValsHelper 
          ( Subs
          , TopDepth 
          , SUBARRAY ( Shape , 1 , Dim . Depth - 1 )
          , SUBARRAY ( FlatVals , RI * DownFlatNumber , DownFlatNumber )
          , EltProc 
          ) 
      END (*FOR*)
    END CheckEltValsHelper
    
(*EXPORTED:*)
; PROCEDURE CheckEltVals
    ( READONLY Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ) 

  = VAR Subs : Common . FullSubsTyp
  ; VAR FlatNumber : INTEGER 

  ; PROCEDURE EltValProc ( Subs : Common . FullSubsTyp ) : Dim . EltTyp
    (* ^To provide A, whose type varies with Depth, for subscripting All
        in one place, once all subscript values are known. *)
        
    = VAR Result : Dim . EltTyp
    
    ; BEGIN
        Result := Dim . EltVal ( Arr , SUBARRAY ( Subs , 0 , Dim . Depth ) )
      ; RETURN Result 
      END EltValProc

  ; BEGIN (* CheckEltVals *) 
      StartTest ( "Element values of " & Globals . Name ) 
    ; IF Dim . Depth > 0
      THEN
        <* ASSERT NUMBER ( Shape ) = Dim . Depth *>
        FlatNumber := Common . IntProduct ( Shape ) 
      ; <* ASSERT FlatNumber = NUMBER ( FlatVals ) *> 
        CheckEltValsHelper
          ( Subs , Dim . Depth , Shape , FlatVals , EltValProc )
      END (*IF*)
    ; EndTest ( ) 
    END CheckEltVals

(*EXPORTED:*)
; PROCEDURE FillFlatA
    ( VAR FlatVals : ARRAY OF INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; Bias : INTEGER
    )

  (* Boy, is this ever the hard way to generate nothing more than a
     simple arithmetic sequence with increment of one, in a flat array.
     But it's a good framework for generating other level-oriented values. *)

  = VAR InnerEltCt : INTEGER
  ; VAR EltVal : INTEGER
  ; VAR ThisDimNumber : INTEGER 
  
  ; BEGIN
      ThisDimNumber := Shape [ 0 ] 
    ; WITH WInnerShape = SUBARRAY ( Shape , 1 , Dim . Depth - 1 )
      DO 
        InnerEltCt := Common . IntProduct ( WInnerShape )
      ; <* ASSERT NUMBER ( FlatVals ) = ThisDimNumber * InnerEltCt *>
        FOR RI := 0 TO ThisDimNumber - 1  
        DO
          EltVal := RI * InnerEltCt + Bias
        ; InnerChecks . FillFlatA
            ( SUBARRAY
                ( FlatVals , RI * InnerEltCt , InnerEltCt ) , WInnerShape , EltVal ) 
        END (*FOR*)
      END (*WITH*)
    END FillFlatA 

(*EXPORTED:*)
; PROCEDURE FillEltValsHelper
    ( VAR Subs : Common . FullSubsTyp
    ; TopDepth : INTEGER 
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ; EltProc : AssignEltProcTyp 
    ) 

  = VAR ThisDimNumber , FlatNumber , DownFlatNumber  : INTEGER

  ; BEGIN
      <* ASSERT NUMBER ( Shape ) = Dim . Depth *>
      ThisDimNumber := Shape [ 0 ] 
    ; FlatNumber := NUMBER ( FlatVals )
    ; DownFlatNumber := FlatNumber DIV Number
    ; <* ASSERT ThisDimNumber * DownFlatNumber = FlatNumber *> 
      FOR RI := 0 TO ThisDimNumber - 1
      DO
        Subs [ Dim . Depth ] := RI 
      ; InnerChecks . FillEltValsHelper 
          ( Subs
          , TopDepth 
          , SUBARRAY ( Shape , 1 , Dim . Depth - 1 )
          , SUBARRAY ( FlatVals , RI * DownFlatNumber , DownFlatNumber )
          , RI * DownFlatNumber + Bias 
          , EltProc 
          ) 
      END (*FOR*)
    END FillEltValsHelper

(*EXPORTED:*)
; PROCEDURE FillEltVals
    ( VAR Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatVals : ARRAY OF INTEGER
    ; Bias : INTEGER 
    ) 

  = VAR Subs : Common . FullSubsTyp
  ; VAR FlatNumber : INTEGER 

  ; PROCEDURE AssignEltValProc
      ( Subs : Common . FullSubsTyp ; EltVal : Dim . EltTyp ) 
    (* ^To provide A, whose type varies with Depth, for subscripting all
        in one place, once all subscript values are known. *)
        
    = BEGIN
        Dim . SetEltVal ( Arr , SUBARRAY ( Subs , 0 , Dim . Depth ) , EltVal )
      END AssignEltValProc

  ; BEGIN (* FillEltVals *) 
      <* ASSERT Dim . Depth > 0 *>
      <* ASSERT NUMBER ( Shape ) = Dim . Depth *>
      FlatNumber := Common . IntProduct ( Shape ) 
    ; <* ASSERT FlatNumber = NUMBER ( FlatVals ) *> 
      FillEltValsHelper
        ( Subs , Dim . Depth , Shape , FlatVals , Bias , AssignEltValProc )
    END FillEltVals

; TYPE RefADRTyp = UNTRACED REF ADDRESS
; TYPE RefINTEGERTyp = UNTRACED REF INTEGER 

(*EXPORTED:*)
; PROCEDURE CheckDopeShape
    ( ShapeAddr : ADDRESS ; READONLY ExpShape : ARRAY OF INTEGER )

  = VAR ShapeIntRef : RefINTEGERTyp
  ; BEGIN
      <* ASSERT NUMBER ( ExpShape ) = Dim . Depth *>
      StartTest ( "Dope shape of " & Globals . Name ) 
    ; FOR RI := FIRST ( ExpShape ) TO LAST ( ExpShape )
      DO
        StartTest
          ( "Shape of " & Globals . Name
            & ", dimension " & Fmt . Int ( RI )
          ) 
      ; ShapeIntRef := LOOPHOLE ( ShapeAddr , RefINTEGERTyp )
      ; IF ShapeIntRef ^ # ExpShape [ RI ]
        THEN
          NoteFailure
            ( "Got " & Fmt . Int ( ShapeIntRef ^ )
               & ", expected " & Fmt . Int ( ExpShape [ RI ] ) & "."
            ) 
        END
      ; EndTest ( )

      ; INC ( ShapeAddr , BYTESIZE ( INTEGER ) )  
      END (*FOR*)
    ; EndTest ( ) 
    END CheckDopeShape 

(*EXPORTED:*)
; PROCEDURE CheckConstOpenness
    ( READONLY OpenValue : Dim . OpenTyp
    ; READONLY FixedValue : Dim . FixedTyp
    ; READONLY ExpShape : ARRAY OF INTEGER
    ; ExpModuleName : TEXT 
    ; ExpReps : RepSetTyp
    )

  = VAR DopeAddr , EltsAddr , DopeShapeAddr , DopeEltsAddr : ADDRESS
  ; VAR EltsRef : RefADRTyp
  ; VAR ConstModuleName : TEXT 
  
  ; BEGIN
      DopeAddr := ADR ( OpenValue )
    ; EltsAddr := ADR ( FixedValue )
    ; IF IsInStackArea ( DopeAddr ) 
      THEN (* Fixed representation *)
           (* The dope we got was constructed at the call site of CheckConst,
              to convert the actual array from fixed to open.  CheckConst
              passed it on to here. *)
        StartTest ( "Openness of " & Globals . Name )
      ; IF NOT RepTyp . Fixed IN ExpReps 
        THEN
       (* NoteFailure
            ( "Is Fixed"
               & ", expected " & RepSetTypDescr ( ExpReps ) & "."
            ) *)
(* NOTE 1: It seems very difficult to ascertain whether an address (in
           particular, of dope) is in the static constant area.  This is
           currently not reliable and causing false failures, so these
           are commented out.  In any case, it's only a performance issue. *)
        END
      ELSE (* The dope we got must be part of the original array constructor. *) 
        DopeShapeAddr := DopeAddr + BYTESIZE ( ADDRESS )
      ; CheckDopeShape ( DopeShapeAddr , ExpShape ) 
      ; StartTest ( "Openness of " & Globals . Name )
      ; IF DopeAddr + BYTESIZE ( ADDRESS ) + BYTESIZE ( INTEGER ) * Depth
           = EltsAddr
        THEN (* Contiguous, 2-D open representation. *)
          IF NOT RepTyp . OpenContig IN ExpReps
          THEN
            NoteFailure
              ( "Is OpenContig"
                 & ", expected " & RepSetTypDescr ( ExpReps ) & "."
              )
          END
        ELSE
          ConstModuleName := ConstAreaOf ( DopeAddr ) 
        ; IF ConstModuleName # NIL
          THEN (* Dope is in static constant area of module 'ConstModuleName' *)
            IF ExpModuleName # NIL AND NOT Text . Equal ( ExpModuleName , "" )
               AND NOT Text . Equal ( ConstModuleName , ExpModuleName  )
            THEN
              NoteFailure
                ( "Found dope in module " & ConstModuleName
                   & ", expected " & ExpModuleName
                )
            ELSE 
              EltsRef := LOOPHOLE ( DopeAddr , RefADRTyp )
            ; DopeEltsAddr := LOOPHOLE ( EltsRef ^ , ADDRESS )
            ; IF DopeEltsAddr = EltsAddr
              THEN (* Open 2-D representation with remote elements. *)
                IF NOT RepTyp . OpenRemote IN ExpReps
                THEN
                  NoteFailure
                    ( "Is OpenRemote"
                       & ", expected " & RepSetTypDescr ( ExpReps ) & "."
                    )
                END
              ELSE
                NoteFailure ( " Unrecognized representation." )
              END
            END
          ELSE (* Dope is in neither place? *)
            (* NoteFailure ( " Dope in unknown location." ) See NOTE 1 above. *)
          END
        END
      END
    ; EndTest ( ) 
    END CheckConstOpenness

(*EXPORTED:*)
; PROCEDURE Check
    ( READONLY Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    )
  = BEGIN
      <* ASSERT Shape [ 0 ] =  NUMBER ( Dim . FixedTyp ) *>
      CheckNumber ( Arr , Shape )
    ; CheckEltVals ( Arr , Shape , FlatEltVals )
    END Check 

(*EXPORTED:*)
; PROCEDURE CheckConstFixed
    ( READONLY Arr : Dim . FixedTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    ; <* UNUSED *> ExpModuleName : TEXT 
    ; <* UNUSED *> ExpReps : RepSetTyp
    )
  = BEGIN
      <* ASSERT Shape [ 0 ] =  NUMBER ( Dim . FixedTyp ) *>
      CheckNumber ( Arr , Shape )      
    ; CheckEltVals ( Arr , Shape , FlatEltVals )
    (* We really want to get the address of the original constant, which
       is in the static constant area. But that makes it a non-designator,
       so passing it VAR is illegal.  Passing it READONLY will skip the dope,
       if any, and pass the address of the elements, regardless.
       So we can't test the openness of the original constructor. *)
    END CheckConstFixed  

(*EXPORTED:*)
; PROCEDURE CheckConstOpen
    ( READONLY Arr : Dim . OpenTyp
    ; READONLY Shape : ARRAY OF INTEGER
    ; READONLY FlatEltVals : ARRAY OF INTEGER
    ; ExpModuleName : TEXT 
    ; ExpReps : RepSetTyp
    )
  = BEGIN
      <* ASSERT Shape [ 0 ] =  NUMBER ( Dim . FixedTyp ) *>
      CheckNumber ( Arr , Shape )      
    ; CheckEltVals ( Arr , Shape , FlatEltVals )
    ; CheckConstOpenness ( Arr , Arr , Shape , ExpModuleName , ExpReps ) 
    END CheckConstOpen  

; BEGIN
    Init ( Common . BegOfConstMarker ) 
  END Checks
. 
