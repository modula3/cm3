(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p274                              *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test mixtures of set, record, and array constructors (when used as
   components of other constructors). *)

MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr 

; TYPE Rng = [ 0 .. 31 ]

; TYPE Rec = RECORD X , Y , Z , W : Rng END

; TYPE Arr = ARRAY [ 0 .. 3 ] OF Rng 

; TYPE Set = SET OF Rng

; TYPE IntArr = ARRAY OF INTEGER 

; CONST GRec = Rec { 10 , 25 , 16 , 9 } (* CT Warning. *)
; CONST GArr = Arr { 10 , 25 , 16 , 9 } (* CT Warning. *)
; CONST GSet = Set { 10 , 25 , 16 , 9 } (* CT Warning. *)

; CONST S1C = Set { 1 , 4 , 7 , 10 }
; CONST S1Exp = IntArr { 1 , 4 , 7 , 10 } 
; VAR S1V := S1C

; CONST S2C = Set { 9 , 6 , 3 , 0 }
; CONST S2Exp = IntArr { 9 , 6 , 3 , 0 } 
; VAR S2V := S2C

; CONST S3C = Set { 2 , 5 , 8 , 11 }
; CONST S3Exp = IntArr { 2 , 5 , 8 , 11 }
; VAR S3V := S3C


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

(* =========================== Sets in records  ========================= *)

; PROCEDURE CSetsInCRec ( ) : INTEGER 
  = CONST Loc
          = RECORD RF1 , RF2 , RF3 : Set END
               { S1C , S2C , S3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc . RF1 , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF2 , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF3 , S3Exp ) )
    ; RETURN LMismatchCt 
    END CSetsInCRec 

; PROCEDURE CSetsInVRec ( ) : INTEGER 
  = VAR Loc
          := RECORD RF1 , RF2 , RF3 : Set END
               { S1C , S2C , S3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc . RF1 , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF2 , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF3 , S3Exp ) )
    ; RETURN LMismatchCt 
    END CSetsInVRec 

; PROCEDURE VSetsInVRec ( ) : INTEGER 
  = VAR Loc
          := RECORD RF1 , RF2 , RF3 : Set END
               { S1V , S2V , S3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc . RF1 , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF2 , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc . RF3 , S3Exp ) )
    ; RETURN LMismatchCt 
    END VSetsInVRec 

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

(* =========================== Sets in arrays  ========================= *)

; PROCEDURE CSetsInCArr ( ) : INTEGER 
  = CONST Loc = ARRAY [ 1 .. 3 ] OF Set { S1C , S2C , S3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc [ 1 ] , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 2 ] , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 3 ] , S3Exp ) )
    ; RETURN LMismatchCt 
    END CSetsInCArr 

; PROCEDURE CSetsInVArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 1 .. 3 ] OF Set { S1C , S2C , S3C }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc [ 1 ] , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 2 ] , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 3 ] , S3Exp ) )
    ; RETURN LMismatchCt 
    END CSetsInVArr 

; PROCEDURE CSetsInOArr ( ) : INTEGER 
  = VAR Loc := ARRAY [ 0 .. 2 ] OF Set { Set { } , Set { } , Set { } }
  ; VAR LMismatchCt : INTEGER 
  ; PROCEDURE Inner ( VAR FArr : ARRAY OF Set )
    = BEGIN
        FArr [ 0 ] := S1C 
      ; FArr [ 1 ] := S2C 
      ; FArr [ 2 ] := S3C 
      ; LMismatchCt := SetMismatchCt ( FArr [ 0 ] , S1Exp ) 
      ; INC ( LMismatchCt , SetMismatchCt ( FArr [ 1 ] , S2Exp ) )  
      ; INC ( LMismatchCt , SetMismatchCt ( FArr [ 2 ] , S3Exp ) )
      END Inner 
  ; BEGIN (* CSetsInOArr *)
      Inner ( Loc )
    ; RETURN LMismatchCt 
    END CSetsInOArr 

; PROCEDURE VSetsInVArr ( ) : INTEGER 
  = VAR Loc
          := ARRAY [ 1 .. 3 ] OF Set { S1V , S2V , S3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := SetMismatchCt ( Loc [ 1 ] , S1Exp ) 
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 2 ] , S2Exp ) )  
    ; INC ( LMismatchCt , SetMismatchCt ( Loc [ 3 ] , S3Exp ) )
    ; RETURN LMismatchCt 
    END VSetsInVArr

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
  = VAR Loc
          := ARRAY [ 1 .. 3 ] OF Arr { A1V , A2V , A3V }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := ArrMismatchCt ( Loc [ 1 ] , A1Exp ) 
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 2 ] , A2Exp ) )  
    ; INC ( LMismatchCt , ArrMismatchCt ( Loc [ 3 ] , A3Exp ) )
    ; RETURN LMismatchCt 
    END VArrsInVArr

(* ============================= Test support ============================ *)

; TYPE ProcTyp = PROCEDURE ( ) : INTEGER 

; PROCEDURE SetMismatchCt
    ( Actual : Set ; <*NOWARN*> VALUE Exp : IntArr ) : INTEGER 

  = VAR LExpSet := Set { }
  ; VAR LMismatchCt : INTEGER 
  ; BEGIN
      FOR RExp := FIRST ( Exp ) TO LAST ( Exp )
      DO
        LExpSet := LExpSet + Set { Exp [ RExp ] } 
      END (*FOR*)
    ; LMismatchCt := 0
    ; FOR RElem := FIRST ( Rng ) TO LAST ( Rng )
      DO
        INC ( LMismatchCt
            , ORD ( ( RElem IN Actual ) # ( RElem IN LExpSet ) )
            ) 
      END (* FOR *)
    ; RETURN LMismatchCt 
    END SetMismatchCt

; PROCEDURE RecMismatchCt
    ( Actual : Rec ; <*NOWARN*> VALUE Exp : IntArr ) : INTEGER 

  = VAR LMismatchCt : INTEGER 
  ; BEGIN
      LMismatchCt := 0
    ; INC ( LMismatchCt , ORD ( Actual . X # Exp [ 0 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . Y # Exp [ 1 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . Z # Exp [ 2 ] ) )   
    ; INC ( LMismatchCt , ORD ( Actual . W # Exp [ 3 ] ) )   
    ; RETURN LMismatchCt 
    END RecMismatchCt

; PROCEDURE ArrMismatchCt
    ( Actual : Arr ; <*NOWARN*> VALUE Exp : IntArr ) : INTEGER 

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

  ; TestVals ( CSetsInCRec , "constant sets in constant record" ) 
  ; TestVals ( CSetsInVRec , "constant sets in variable record" ) 
  ; TestVals ( VSetsInVRec , "variable sets in variable record" ) 

  ; TestVals ( CRecsInCRec , "constant records in constant record" ) 
  ; TestVals ( CRecsInVRec , "constant records in variable record" ) 
  ; TestVals ( VRecsInVRec , "variable records in variable record" ) 

  ; TestVals ( CArrsInCRec , "constant arrays in constant record" ) 
  ; TestVals ( CArrsInVRec , "constant arrays in variable record" ) 
  ; TestVals ( VArrsInVRec , "variable arrays in variable record" ) 

  ; TestVals ( CSetsInCArr , "constant sets in constant array" ) 
  ; TestVals ( CSetsInVArr , "constant sets in variable array" ) 
  ; TestVals ( CSetsInOArr , "constant sets in open array" ) 
  ; TestVals ( VSetsInVArr , "variable sets in variable array" )
  
  ; TestVals ( CRecsInCArr , "constant records in constant array" ) 
  ; TestVals ( CRecsInVArr , "constant records in variable array" ) 
  ; TestVals ( CRecsInOArr , "constant records in open array" ) 
  ; TestVals ( VRecsInVArr , "variable records in variable array" ) 

  ; TestVals ( CArrsInCArr , "constant arrays in constant array" ) 
  ; TestVals ( CArrsInVArr , "constant arrays in variable array" ) 
  ; TestVals ( CArrsInOArr , "constant arrays in open array" ) 
  ; TestVals ( VArrsInVArr , "variable arrays in variable array" ) 

  ; Report ( ) 
  END Main
.
