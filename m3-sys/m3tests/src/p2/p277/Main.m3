(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p277                              *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test BITS FOR records and arrays inside record and array constructore. *)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; TYPE Rng = [ 0 .. 31 ]
; TYPE ORng = [ 0 .. 15 ]

; TYPE Rec = RECORD X , Y , Z , W : BITS 5 FOR Rng END

; TYPE Arr = ARRAY [ 0 .. 3 ] OF BITS 5 FOR Rng 
; TYPE Arr4 = ARRAY [ 0 .. 3 ] OF BITS 4 FOR ORng 

; TYPE OArr = ARRAY OF BITS 4 FOR ORng 

; TYPE IntArr = ARRAY OF INTEGER 

; CONST GRec = Rec { 10 , 32 , 16 , 9 } (* CT Warning. *)
; CONST GArr = Arr { 10 , 32 , 16 , 9 } (* CT Warning. *)

; CONST R0C = Rec { 0 , 0 , 0 , 0 }
; CONST R1C = Rec { 1 , 4 , 7 , 10 }
; CONST R1Exp = IntArr { 1 , 4 , 7 , 10 } 
; VAR R1V := R1C

; CONST R2C = Rec { 9 , 6 , 3 , 0 }
; CONST R2Exp = IntArr { 9 , 6 , 3 , 0 } 
; VAR R2V := R2C

; CONST R3C = Rec { 2 , 5 , 8 , 11 }
; CONST R3Exp = IntArr { 2 , 5 , 8 , 11 }
; VAR R3V := R3C

; CONST A0C = Arr { 0 , 0 , 0 , 0 }
; CONST A1C = Arr { 1 , 4 , 7 , 10 }
; CONST A1Exp = IntArr { 1 , 4 , 7 , 10 } 
; VAR A1V := A1C

; CONST A2C = Arr { 9 , 6 , 3 , 0 }
; CONST A2Exp = IntArr { 9 , 6 , 3 , 0 } 
; VAR A2V := A2C

; CONST A3C = Arr { 2 , 5 , 8 , 11 }
; CONST A3Exp = IntArr { 2 , 5 , 8 , 11 }
; VAR A3V := A3C

; CONST OAFlatExp
        = IntArr { 1 , 4 , 7 , 10 , 9 , 6 , 3 , 0 , 2 , 5 , 8 , 11
                 , 10 , 7 , 4 , 1 , 6 , 3
                 }

; CONST OOAFlatExp = IntArr { 1 , 4 , 7 , 10 , 9 , 6 , 3 , 0 , 2 , 5 , 8 , 11 }

; CONST OA0C = OArr { 0 , 0 , 0 , 0 }

(* =========================== Records in records  ========================= *)

; PROCEDURE CRecsInCRec ( ) : INTEGER 
  = CONST Loc = RECORD RF1 , RF2 , RF3 : Rec END
                  { R1C , R2C , R3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc . RF1 , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF2 , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF3 , R3Exp ) )
    ; RETURN LMismatchCt 
    END CRecsInCRec 

; PROCEDURE CRecsInVRec ( ) : INTEGER 
  = VAR Loc := RECORD RF1 , RF2 , RF3 : Rec END
                  { R1C , R2C , R3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc . RF1 , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF2 , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF3 , R3Exp ) )
    ; RETURN LMismatchCt 
    END CRecsInVRec 

; PROCEDURE VRecsInVRec ( ) : INTEGER 
  = VAR Loc := RECORD RF1 , RF2 , RF3 : Rec END
                  { R1V , R2V , R3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc . RF1 , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF2 , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc . RF3 , R3Exp ) )
    ; RETURN LMismatchCt 
    END VRecsInVRec 

(* =========================== Arrays in records  ========================= *)

; PROCEDURE CArrsInCRec ( ) : INTEGER 
  = CONST Loc = RECORD RF1 , RF2 , RF3 : Arr END { A1C , A2C , A3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc . RF1 , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF2 , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF3 , A3Exp ) )
    ; RETURN LMismatchCt 
    END CArrsInCRec 

; PROCEDURE CArrsInVRec ( ) : INTEGER 
  = VAR Loc := RECORD RF1 , RF2 , RF3 : Arr END { A1C , A2C , A3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc . RF1 , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF2 , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF3 , A3Exp ) )
    ; RETURN LMismatchCt 
    END CArrsInVRec 

; PROCEDURE VArrsInVRec ( ) : INTEGER 
  = VAR Loc := RECORD RF1 , RF2 , RF3 : Arr END { A1V , A2V , A3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc . RF1 , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF2 , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc . RF3 , A3Exp ) )
    ; RETURN LMismatchCt 
    END VArrsInVRec 

(* =========================== Records in arrays  ========================= *)

; PROCEDURE CRecsInCArr ( ) : INTEGER 
  = CONST Loc = ARRAY [ 1 .. 3 ] OF Rec { R1C , R2C , R3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc [ 1 ] , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 2 ] , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 3 ] , R3Exp ) )
    ; RETURN LMismatchCt 
    END CRecsInCArr 

; PROCEDURE CRecsInVArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 1 .. 3 ] OF Rec { R1C , R2C , R3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc [ 1 ] , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 2 ] , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 3 ] , R3Exp ) )
    ; RETURN LMismatchCt 
    END CRecsInVArr 

; PROCEDURE CRecsInOArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 2 ] OF Rec { R0C , R0C , R0C }
  ; VAR LMismatchCt : INTEGER 
  ; PROCEDURE Inner ( VAR FArr : ARRAY OF Rec )
    = BEGIN
        FArr [ 0 ] := R1C 
      ; FArr [ 1 ] := R2C 
      ; FArr [ 2 ] := R3C 
      ; LMismatchCt := RecMismatchCt ( FArr [ 0 ] , R1Exp ) 
      ; INC ( LMismatchCt , RecMismatchCt ( FArr [ 1 ] , R2Exp ) )  
      ; INC ( LMismatchCt , RecMismatchCt ( FArr [ 2 ] , R3Exp ) )
      END Inner 
  ; BEGIN (* CRecsInOArr *)
      Inner ( Loc )
    ; RETURN LMismatchCt 
    END CRecsInOArr 

; PROCEDURE VRecsInVArr ( ) : INTEGER 
  = VAR Loc
          := ARRAY [ 1 .. 3 ] OF Rec { R1V , R2V , R3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := RecMismatchCt ( Loc [ 1 ] , R1Exp ) 
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 2 ] , R2Exp ) )  
    ; INC ( LMismatchCt , RecMismatchCt ( Loc [ 3 ] , R3Exp ) )
    ; RETURN LMismatchCt 
    END VRecsInVArr

(* =========================== Arrays in arrays  ========================= *)

; PROCEDURE CArrsInCArr ( ) : INTEGER 
  = CONST Loc = ARRAY [ 1 .. 3 ] OF Arr { A1C , A2C , A3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc [ 1 ] , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 2 ] , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 3 ] , A3Exp ) )
    ; RETURN LMismatchCt 
    END CArrsInCArr 

; PROCEDURE CArrsInCArrInline ( ) : INTEGER 
  = CONST Loc = ARRAY [ 1 .. 3 ] OF Arr
      { Arr { 1 , 4 , 7 , 10 }
      , Arr { 9 , 6 , 3 , 0 }
      , Arr { 2 , 5 , 8 , 11 }
      }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc [ 1 ] , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 2 ] , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 3 ] , A3Exp ) )
    ; RETURN LMismatchCt 
    END CArrsInCArrInline 

; PROCEDURE CArrsInVArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 1 .. 3 ] OF Arr { A1C , A2C , A3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc [ 1 ] , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 2 ] , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 3 ] , A3Exp ) )
    ; RETURN LMismatchCt 
    END CArrsInVArr 

; PROCEDURE CArrsInOArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 2 ] OF Arr { A0C , A0C , A0C }
  ; VAR LMismatchCt : INTEGER 
  ; PROCEDURE Inner ( VAR FArr : ARRAY OF Arr )
    = BEGIN
        FArr [ 0 ] := A1C 
      ; FArr [ 1 ] := A2C 
      ; FArr [ 2 ] := A3C 
      ; LMismatchCt := ArrMismatchCt ( FArr [ 0 ] , A1Exp ) 
      ; INC ( LMismatchCt , ArrMismatchCt ( FArr [ 1 ] , A2Exp ) )  
      ; INC ( LMismatchCt , ArrMismatchCt ( FArr [ 2 ] , A3Exp ) )
      END Inner 
  ; BEGIN (* CArrsInOArr *)
      Inner ( Loc )
    ; RETURN LMismatchCt 
    END CArrsInOArr 

; PROCEDURE VArrsInVArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 1 .. 3 ] OF Arr { A1V , A2V , A3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc [ 1 ] , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 2 ] , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 3 ] , A3Exp ) )
    ; RETURN LMismatchCt 
    END VArrsInVArr

; PROCEDURE OArrsInOArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 2 ] OF Arr4 { OA0C , OA0C , OA0C }
  ; VAR LMismatchCt : INTEGER 
  ; PROCEDURE Inner ( VALUE FOArr : ARRAY OF OArr )
    = BEGIN
        FOArr := ARRAY OF OArr
          { OArr { 1 , 4 , 7 , 10 }
          , OArr { 9 , 6 , 3 , 0 }
          , OArr { 2 , 5 , 8 , 11 }
          } 
      ; LMismatchCt := OOArrMismatchCt ( FOArr , OOAFlatExp ) 
      END Inner 
  ; BEGIN (* OArrsInOArr *)
      Inner ( Loc )
    ; RETURN LMismatchCt 
    END OArrsInOArr

; PROCEDURE P4InArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 17 ] OF BITS 4 FOR ORng
          { 1 ,  4 , 7 , 10 
          , 9 ,  6 , 3 , 0 
          , 2 ,  5 , 8 , 11
          , 10 , 7 , 4 , 1
          , 6 ,  3
          } 
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN (* P4InOArr *)
      LMismatchCt := OArrMismatchCt ( Loc , OAFlatExp ) 
    ; RETURN LMismatchCt 
    END P4InArr 

; PROCEDURE P4InOArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 17 ] OF BITS 4 FOR ORng { 0 , .. }  
  ; VAR LMismatchCt : INTEGER 
  ; PROCEDURE Inner ( VALUE FOArr : OArr )
    = BEGIN
        FOArr := OArr
          { 1 ,  4 , 7 , 10 
          , 9 ,  6 , 3 , 0 
          , 2 ,  5 , 8 , 11
          , 10 , 7 , 4 , 1
          , 6 ,  3
          } 
      ; LMismatchCt := OArrMismatchCt ( FOArr , OAFlatExp ) 
      END Inner 
  ; BEGIN (* P4InOArr *)
      Inner ( Loc )
    ; RETURN LMismatchCt 
    END P4InOArr 

(* ============================= Test support ============================ *)

; TYPE ProcTyp = PROCEDURE ( ) : INTEGER 

; PROCEDURE RecMismatchCt
    ( Actual : Rec
    ; <*NOWARN*> VALUE Exp : IntArr
    )
  : INTEGER 

  = VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := 0
    ; INC ( LMismatchCt , ORD ( Actual . X # Exp [ 0 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . Y # Exp [ 1 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . Z # Exp [ 2 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . W # Exp [ 3 ] ) )   
    ; RETURN LMismatchCt 
    END RecMismatchCt

; PROCEDURE OOArrMismatchCt
    ( Actual : ARRAY OF OArr
    ; <*NOWARN*> VALUE Exp : IntArr
    )
  : INTEGER 

  = VAR LMismatchCt , LExpSs : INTEGER
  ; BEGIN
      LMismatchCt := 0
    ; LExpSs := 0 
    ; FOR RI := FIRST ( Actual ) TO LAST ( Actual )
      DO
        FOR RJ := 0 TO 3
          (* Would rather write FIRST ( Actual [ RI ] .. Last ( Actual [ RI ],
             but that would produce this error:
             CM3 restriction: Open array of non-byte-aligned elements cannot be partially subscripted.
             Maybe someday make a special case for FIRST, LAST, NUMBER of
             partially-subscripted, packed.
          *)
        DO
          INC ( LMismatchCt , ORD ( Actual [ RI , RJ ] # Exp [ LExpSs ] ) )
        ; INC ( LExpSs ) 
        END (*FOR*)
      END (*FOR*)
  
    ; RETURN LMismatchCt 
    END OOArrMismatchCt

; PROCEDURE OArrMismatchCt
    ( VALUE Actual : OArr 
    ; <*NOWARN*> VALUE Exp : IntArr
    )
  : INTEGER 

  = VAR LMismatchCt , LExpSs : INTEGER
  ; BEGIN
      LMismatchCt := 0
    ; LExpSs := 0 
    ; FOR RI := FIRST ( Actual ) TO LAST ( Actual )
      DO
        INC ( LMismatchCt , ORD ( Actual [ RI ] # Exp [ LExpSs ] ) )
      ; INC ( LExpSs ) 
      END (*FOR*)
  
    ; RETURN LMismatchCt 
    END OArrMismatchCt

; PROCEDURE ArrMismatchCt
    ( Actual : Arr
    ; <*NOWARN*> VALUE Exp : IntArr
    )
  : INTEGER 

  = VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := 0
    ; INC ( LMismatchCt , ORD ( Actual [ 0 ] # Exp [ 0 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual [ 1 ] # Exp [ 1 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual [ 2 ] # Exp [ 2 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual [ 3 ] # Exp [ 3 ] ) )   
    ; RETURN LMismatchCt 
    END ArrMismatchCt

; PROCEDURE TestVals ( Proc : ProcTyp ; Label : TEXT )
  = VAR LMismatchCt : INTEGER
  ; BEGIN
      LMismatchCt := Proc ( )
    ; IF LMismatchCt = 0
      THEN
        IF Verbose
        THEN
          WL ( "Succeeded, with no mismatched values: " , Label )
        END 
      ELSE
        WL ( "##### FAILED ##### With " 
           , Int ( LMismatchCt )
           , " mismatched values: " 
           , Label )
      ; INC ( FailureCt ) 
      END (*IF*)
    ; INC ( TestCt ) 
    END TestVals 

; <*UNUSED*> PROCEDURE NoRTError ( ) (*RAISES ANY*) 
  = BEGIN
    END NoRTError

(* ================================= Output ============================ *)

; VAR TestCt : INTEGER
; VAR FailureCt : INTEGER

; PROCEDURE Int ( I : INTEGER ) : TEXT
  = BEGIN
      RETURN Fmt . Int ( I ) 
    END Int

; VAR WrT : Wr . T
; VAR Verbose : BOOLEAN := FALSE 

; PROCEDURE EOL ( )
  = <*FATAL Thread.Alerted, Wr.Failure*>
    BEGIN
      Wr . PutText ( WrT , Wr . EOL )
    END EOL 

; PROCEDURE WL ( A , B , C , D : TEXT := NIL )
  = <*FATAL Thread.Alerted, Wr.Failure*>
    BEGIN
      IF A # NIL THEN Wr . PutText ( WrT , A ) END 
    ; IF B # NIL THEN Wr . PutText ( WrT , B ) END 
    ; IF C # NIL THEN Wr . PutText ( WrT , C ) END 
    ; IF D # NIL THEN Wr . PutText ( WrT , D ) END
    ; EOL ( ) 
    END WL

; PROCEDURE Report ( )
  = BEGIN
      WL ( Int ( TestCt ) , " tests performed." )
    ; IF FailureCt <= 0
      THEN
        WL ( "All succeeded." )
      ; WL ( "Overall test succeeded." )
      ELSE
        WL ( Int ( FailureCt ) , " of them failed." )
      ; WL ( "Overall test ##### FAILED #####." )
      END
    END Report

(* ================================== Main ============================= *)

; BEGIN
    TestCt := 0
  ; FailureCt := 0
  ; WrT := Stdio . stdout
  ; Verbose := TRUE

  ; TestVals ( CRecsInCRec , "constant records in constant record" ) 
  ; TestVals ( CRecsInVRec , "constant records in variable record" ) 
  ; TestVals ( VRecsInVRec , "variable records in variable record" ) 

  ; TestVals ( CArrsInCRec , "constant arrays in constant record" ) 
  ; TestVals ( CArrsInVRec , "constant arrays in variable record" ) 
  ; TestVals ( VArrsInVRec , "variable arrays in variable record" ) 

  ; TestVals ( CRecsInCArr , "constant records in constant array" ) 
  ; TestVals ( CRecsInVArr , "constant records in variable array" ) 
  ; TestVals ( CRecsInOArr , "constant records in open array" ) 
  ; TestVals ( VRecsInVArr , "variable records in variable array" ) 

  ; TestVals ( CArrsInCArr , "constant arrays in constant array" ) 
  ; TestVals ( CArrsInCArrInline , "constant arrays inline in constant array" ) 
  ; TestVals ( CArrsInVArr , "constant arrays in variable array" ) 
  ; TestVals ( CArrsInOArr , "constant arrays in open array" ) 
  ; TestVals ( VArrsInVArr , "variable arrays in variable array" ) 
  ; TestVals ( OArrsInOArr , "open arrays in open array" ) 

  ; TestVals ( P4InArr , "4-bit packed in fixed array" ) 
  ; TestVals ( P4InOArr , "4-bit packed in open array" ) 

  ; Report ( ) 
  END Main
.

