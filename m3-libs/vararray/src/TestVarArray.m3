
(* -----------------------------------------------------------------------1- *)
(* File TestVarArray.m3                                                      *)
(* Modula-3 source code.                                                     *)
(* Copyright 2013, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

MODULE TestVarArray EXPORTS Main 

; IMPORT Fmt 
; IMPORT Integer 
; IMPORT Random  
; IMPORT Thread 
; IMPORT Stdio
; IMPORT Word 
; IMPORT Wr 

; IMPORT IntCharVarArray AS IC  
; IMPORT IntIntVarArray AS II 
; IMPORT IntRanges  
; IMPORT Spinner 

; TYPE SsTyp = II . SsTyp 
; TYPE ElemTyp = II . ElemTyp 

; CONST EmptyRange = IntRanges . EmptyRange 
; CONST FullRange = IntRanges . FullRange 

(* Local data on var arrays. *)  

; TYPE RangeTyp = IntRanges . RangeTyp 

; TYPE ArrTyp 
  = RECORD 
      TouchedRange : RangeTyp 
    ; MaxRange : RangeTyp := RangeTyp { - 512 , 511 }  
    ; InitElemValue : ElemTyp  
    ; ExpansionFactor : REAL 
    ; Var : II . T 
    ; Label : TEXT 
    END 

; PROCEDURE StdContents 
    ( <* UNUSED *> READONLY A : ArrTyp ; Ss : SsTyp ) : ElemTyp 
  (* Between tests, each array is kept with all touched elements having value
     Fetch(A.Var,Ss) = StdContents (A,Ss).
     This is now algorithmically derived, to obviate keeping whole duplicate
     arrays in the tester.
  *) 

  = BEGIN 
      RETURN Word . Plus ( Ss , 10 ) 
    END StdContents 

; PROCEDURE AltContents 
    ( <* UNUSED *> READONLY A : ArrTyp ; Ss : SsTyp ) : ElemTyp 
  (* During a test, any elements of an array that are or could be assigned-to 
     are given values Fetch(A.Var,Ss) = AltContents (A,Ss), temporarily until
     the test is finished.  
     This is now algorithmically derived, to obviate keeping whole duplicate
     arrays in the tester.
  *) 

  = BEGIN 
      RETURN Word . Plus ( Ss , 100 )
    END AltContents 

; PROCEDURE AltContentsBy 
    ( READONLY A : ArrTyp 
    ; AltRange , StdRange : RangeTyp 
    ; Ss : SsTyp 
    ; By : INTEGER 
    ) 
  : ElemTyp 

  = VAR LResult : ElemTyp 

  ; BEGIN 
      IF By > 0 
      THEN 
        IF ( Ss - AltRange . Lo ) MOD By = 0 
        THEN LResult := AltContents ( A , Ss ) 
        ELSIF IntRanges . InRange ( Ss , StdRange ) 
        THEN LResult := StdContents ( A , Ss )  
        ELSE LResult := A . InitElemValue 
        END (* IF *) 
      ELSIF By < 0 
      THEN 
        IF ( AltRange . Hi - Ss ) MOD By = 0 
        THEN LResult := AltContents ( A , Ss ) 
        ELSIF IntRanges . InRange ( Ss , StdRange ) 
        THEN LResult := StdContents ( A , Ss )  
        ELSE LResult := A . InitElemValue 
        END (* IF *) 
      ELSE (* Paranoia.  Shouldn't happen. *) 
        LResult := A . InitElemValue 
      END (* IF *) 
    ; RETURN LResult 
    END AltContentsBy 

(* Use of ranges in var arrays. *) 

; PROCEDURE SetLocalTouchedRange ( VAR (* IN OUT *) A : ArrTyp ; R : RangeTyp ) 

  = BEGIN 
      IF IntRanges . RangeIsEmpty ( R ) 
      THEN A . TouchedRange := EmptyRange  
      ELSE A . TouchedRange := R 
      END 
    END SetLocalTouchedRange 

(* Operation identities. *) 
 
; TYPE OpTyp  
    = { 
      (* Mutators: *) 
        New
      , Touch 
      , Project 
      , Assign 
      , AssignSubarray 
      , CallbackWithElem 
      , CallbackWithSubarray
      , ForAllTouchedInRange
      , ForAllInRange
      , Copy
      , Compact 
      (* Queries: *) 
      , TouchedRange
      , InitElemValue 
      , Fetch
      , FetchSubarray
      , AllocatedRange 
      , RawInfo       (* Could this be considered a mutator? *) 
      } 

; PROCEDURE OpImage ( Op : OpTyp ) : TEXT 

  = BEGIN 
      CASE Op 
      OF OpTyp . Touch => RETURN "Touch"
      | OpTyp . New => RETURN "New"
      | OpTyp . Project => RETURN "Project"
      | OpTyp . Assign => RETURN "Assign"
      | OpTyp . AssignSubarray => RETURN "AssignSubarray"
      | OpTyp . CallbackWithElem => RETURN "CallbackWithElem"
      | OpTyp . CallbackWithSubarray => RETURN "CallbackWithSubarray"
      | OpTyp . ForAllTouchedInRange => RETURN "ForAllTouchedInRange"
      | OpTyp . ForAllInRange => RETURN "ForAllInRange"
      | OpTyp . Copy => RETURN "Copy"
      | OpTyp . Compact => RETURN "Compact"
      | OpTyp . TouchedRange => RETURN "TouchedRange"
      | OpTyp . InitElemValue => RETURN "InitElemValue"
      | OpTyp . Fetch => RETURN "Fetch"
      | OpTyp . FetchSubarray => RETURN "FetchSubarray"
      | OpTyp . AllocatedRange => RETURN "AllocatedRange"
      | OpTyp . RawInfo => RETURN "RawInfo"
      END (* CASE *) 
    END OpImage 

(* Low level output *) 

; VAR GWrT : Wr . T := Stdio . stdout 

; PROCEDURE WT ( Msg : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( GWrT , Msg ) 
    END WT 

; PROCEDURE NL ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> BEGIN 
      WT ( Wr . EOL ) 
    ; Wr . Flush ( GWrT ) 
    END NL 

(* Error messages and statistics. *) 

; VAR GImplicitTestCt : CARDINAL 
  (* ^During a single explicit test (Do* ), some procedures in VarArray are
     used to check results.  These are counted from zero during each explicit
     test procedure here. *) 
; VAR GFailedCheckCt : CARDINAL 
  (* ^Multiple problems could be discovered during a single explicit test
     procedure.  They are counted from zero during each explicit test here. *)  
; VAR GTestCt : CARDINAL 
  (* ^Total tests performed, explicit and implicit. *) 
; VAR GFailedTestCt : CARDINAL 
  (* ^Total explicit tests that failed at least one check. *) 
; VAR GTestCtByOp : ARRAY OpTyp OF CARDINAL 
  (* ^Total tests performed, explicit and implicit, by operation. *) 
; VAR GFailedTestCtByOp : ARRAY OpTyp OF CARDINAL 
  (* ^Total explicit tests that failed at least one check, by operation. *) 

; PROCEDURE ZeroStats ( ) 

  = BEGIN 
      GTestCt := 0 
    ; GFailedTestCt := 0 
    ; GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; GTestCt := 0 
    ; FOR ROp := FIRST ( OpTyp ) TO LAST ( OpTyp ) 
      DO
        GFailedTestCtByOp [ ROp ] := 0 
      ; GTestCtByOp [ ROp ] := 0 
      END (* FOR *) 
    END ZeroStats 

; PROCEDURE WriteStats ( ) 

  = BEGIN 
      WT ( "Test results summary:" ) 
    ; NL ( ) 
    ; NL ( ) 
    ; FOR ROp := FIRST ( OpTyp ) TO LAST ( OpTyp ) 
      DO 
        WT ( Fmt . Pad ( Fmt . Int ( GTestCtByOp [ ROp ] ) , 7 ) ) 
      ; WT ( " tests " )
      ; WT ( Fmt . Pad ( Fmt . Int ( GFailedTestCtByOp [ ROp ] ) , 7 ) )
      ; IF GFailedTestCtByOp [ ROp ] = 0 
        THEN WT ( " failures for " ) 
        ELSE WT ( " FAILURES for " ) 
        END (* IF *) 
      ; WT ( OpImage ( ROp ) ) 
      ; NL ( ) 
      END (* FOR *) 
    ; WT ( Fmt . Pad ( Fmt . Int ( GTestCt ) , 7 ) ) 
    ; WT ( " tests " ) 
    ; WT ( Fmt . Pad ( Fmt . Int ( GFailedTestCt ) , 7 ) )
    ; IF GFailedTestCt = 0 
      THEN WT ( " failures total" ) 
      ELSE WT ( " FAILURES total" ) 
      END (* IF *) 
    ; NL ( ) 
    ; IF GFailedTestCt > 0 
      THEN 
        WT ( "Test Run FAILED." )
      ; NL ( ) 
      ELSE 
        WT ( "Test Run SUCCEEDED." )
      ; NL ( ) 
      END 
    ; NL ( ) 
    END WriteStats 

; PROCEDURE Error ( READONLY A : ArrTyp ; Op : OpTyp ; Msg : TEXT ) 

  = BEGIN
      IF NOT Spinner . ProgressLineIsEmpty ( ) THEN NL ( ) END  
    ; WT ( "Error, array " ) 
    ; WT ( A . Label ) 
    ; WT ( ", operation " ) 
    ; WT ( OpImage ( Op ) ) 
    ; WT ( ": ") 
    ; WT ( Msg ) 
    ; NL ( ) 
    ; INC ( GFailedCheckCt ) 
    END Error 

; PROCEDURE ErrorVal 
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; PreLabel : TEXT 
    ; Ss : SsTyp 
    ; ActVal , ExpVal : ElemTyp 
    ) 

  = BEGIN 
      Error 
        ( A , Op 
        , PreLabel 
          & "at subscript " & Fmt . Int ( Ss )  
          & ", element value " & Fmt . Int ( ActVal )  
          & ", expected value " & Fmt . Int ( ExpVal )  
        ) 
    END ErrorVal  

; PROCEDURE CountTest ( <* UNUSED *> READONLY A : ArrTyp ; Op : OpTyp ) 

  = BEGIN 
      INC ( GTestCt , GImplicitTestCt + 1 ) 
    ; INC ( GTestCtByOp [ Op ] ) 
    ; IF GFailedCheckCt > 0 
      THEN 
        INC ( GFailedTestCt ) 
      ; INC ( GFailedTestCtByOp [ Op ] ) 
      ; IF NOT Spinner . ProgressLineIsEmpty ( ) THEN NL ( ) END  
      ; WT ( "This test failed " ) 
      ; WT ( Fmt . Int ( GFailedCheckCt ) )
      ; IF GFailedCheckCt = 1 
        THEN WT ( " check. " ) 
        ELSE WT ( " checks. " ) 
        END 
      ; NL ( ) 
      END 
    ; IF ReportIndividualTests 
      THEN 
        WT ( OpImage ( Op ) & " " ) 
      ; WT ( Fmt . Int ( GTestCtByOp [ Op ] ) & " " ) 
      ; WT ( Fmt . Int ( GTestCt ) & " " ) 
      ; NL ( ) 
      END 
    ; IF GFailedCheckCt > 0 OR ReportIndividualTests 
      THEN Spinner . RedisplayProgress ( ) 
      END (* IF *) 
    ; Spinner . NoteProgress 
        ( (* IN OUT *) GGroupTestCt , Incr := GImplicitTestCt + 1 ) 
    END CountTest 

(* Checks of results. *) 

; PROCEDURE CheckExcept 
    ( READONLY A : ArrTyp ; Op : OpTyp ; ActHappened , ExpHappened : BOOLEAN )

  = BEGIN 
      IF ActHappened 
      THEN 
        IF ExpHappened 
        THEN (* OK *) 
        ELSE 
          Error ( A , Op , "Unexpected exception raised." ) 
        END 
      ELSE  
        IF ExpHappened 
        THEN
          Error ( A , Op , "Expected exception not raised." ) 
        ELSE (* OK *) 
        END 
      END 
    END CheckExcept 

; PROCEDURE CheckInt 
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; ActInt , ExpInt : INTEGER 
    ; Label : TEXT 
    )

  = BEGIN 
      IF ActInt # ExpInt 
      THEN
        Error 
          ( A , Op 
          , "Got " & Fmt . Int ( ActInt ) 
            & ", expected " & Fmt . Int ( ExpInt ) 
            & ", for " & Label 
          )  
      END (* IF *) 
    END CheckInt

; PROCEDURE CheckRange 
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; ActRange , ExpRange : RangeTyp 
    ; Label : TEXT 
    ) 

  = BEGIN 
      IF ActRange # ExpRange 
      THEN 
        Error 
          ( A , Op 
          , Label 
            & " [" & Fmt . Int ( ActRange . Lo ) 
            & ".." & Fmt . Int ( ActRange . Hi ) 
            & "], expected [" & Fmt . Int ( ExpRange . Lo ) 
            & ".." & Fmt . Int ( ExpRange . Hi ) 
            & "]" & Fmt . Int ( ActRange . Lo ) 
          ) 
      END (* IF *) 
    END CheckRange  

; PROCEDURE CheckAlloc 
    ( READONLY A : ArrTyp ; Op : OpTyp ; ExpRange : RangeTyp ) 

  = VAR LAllocRange : RangeTyp 

  ; BEGIN 
      LAllocRange := II . AllocatedRange ( A . Var ) 
    ; CheckRange ( A , Op , LAllocRange , ExpRange , "Allocated range is" ) 
    END CheckAlloc 

; PROCEDURE CheckUnallocValue ( READONLY A : ArrTyp ; Op : OpTyp ; Ss : SsTyp )

  = VAR LElemVal : ElemTyp 

  ; BEGIN 
      LElemVal := II . Fetch ( A . Var , Ss ) 
    ; IF LElemVal # A . InitElemValue 
      THEN
        ErrorVal 
          ( A , Op , "Unallocated elem" , Ss , LElemVal , A . InitElemValue )  
      END (* IF *) 
    END CheckUnallocValue 

; PROCEDURE CheckUnallocValues 
    ( READONLY A : ArrTyp ; Op : OpTyp ; Range : RangeTyp )

  = BEGIN
      IF NOT IntRanges . RangeIsEmpty ( Range ) 
      THEN 
        CheckUnallocValue( A , Op , Range . Lo ) 
      ; IF Range . Hi > Range . Lo + 1 
        THEN 
          CheckUnallocValue( A , Op , ( Range . Lo + Range . Hi ) DIV 2 ) 
        END (* IF *) 
      ; IF Range . Hi > Range . Lo 
        THEN 
          CheckUnallocValue( A , Op , Range . Hi ) 
        END (* IF *) 
      END (* IF *)  
    END CheckUnallocValues 

; PROCEDURE CheckContentsByFetch  
    ( READONLY A : ArrTyp  
    ; Op : OpTyp 
    ; AllocRange , StdRange , AltRange : RangeTyp 
    ; By : INTEGER 
    ) 

  = VAR LVarVal : ElemTyp  
  ; VAR LExpectedVal : ElemTyp 
  ; VAR LI : SsTyp 

  ; BEGIN 
      IF AllocRange . Lo <= AllocRange . Hi 
      THEN 
        LI := AllocRange . Lo 
      ; LOOP 
          LVarVal := II . Fetch ( A . Var , LI ) 
        ; IF IntRanges . InRange ( LI , AltRange ) 
          THEN 
            LExpectedVal 
              := AltContentsBy ( A , AltRange , StdRange , LI , By )  
          ELSIF IntRanges . InRange ( LI , StdRange ) 
          THEN LExpectedVal := StdContents ( A , LI )  
          ELSE LExpectedVal := A . InitElemValue 
          END
        ; IF LVarVal # LExpectedVal 
          THEN 
            ErrorVal ( A , Op , "Using fetch," , LI , LVarVal , LExpectedVal )  
          END
        ; IF LI = AllocRange . Hi 
          THEN EXIT 
          ELSE INC ( LI ) 
          END (* IF *)  
        END (* LOOP *) 
      ; IF GFailedCheckCt = 0 
        THEN (* Let's give Fetch some credit for its role in checking. *) 
          INC ( GTestCt ) 
        ; INC ( GImplicitTestCt ) 
        ; INC ( GTestCtByOp [ OpTyp . Fetch ] ) 
        END 
      END (* IF *) 
    END CheckContentsByFetch  

; PROCEDURE CheckContentsByRawInfo 
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; StdRange , AltRange : RangeTyp 
    ; By : INTEGER 
    ) 

  = VAR LRawInfo : II . RawInfoTyp 
  ; VAR LExpectedVal , LActVal : ElemTyp 
  ; VAR LTouched : RangeTyp
  ; VAR LSs : INTEGER 

  ; BEGIN 
      LRawInfo := II . RawInfo ( A . Var ) 
    ; LTouched := II . TouchedRange ( A . Var )
    ; CheckRange 
        ( A 
        , OpTyp . RawInfo 
        , LRawInfo . Touched 
        , LTouched 
        , "RawInfo.Touched is" 
        )  
      (* ^This is really a check on RawInfo itself. *) 
    
    ; LSs := LRawInfo . Touched . Lo 
    ; LOOP 
        IF LSs >= LRawInfo . Touched . Hi 
        THEN EXIT 
        ELSE 
          IF IntRanges . InRange ( LSs , AltRange ) 
          THEN 
            LExpectedVal := AltContentsBy ( A , AltRange , StdRange , LSs , By )
          ELSIF IntRanges . InRange ( LSs , StdRange ) 
          THEN LExpectedVal := StdContents ( A , LSs )  
          ELSE LExpectedVal := A . InitElemValue 
          END (* IF *) 
        ; LActVal 
            := LRawInfo . ArrayRef 
               ^ [ Word . Minus ( LSs , LRawInfo . BiasInt ) ] 
        ; IF LActVal # LExpectedVal 
          THEN 
            ErrorVal 
              ( A , Op , "Using RawInfo," , LSs , LActVal , LExpectedVal )
          END (* IF *) 
        ; INC ( LSs ) 
        END (* IF *) 
      END (* LOOP *)  
    ; IF GFailedCheckCt = 0 
      THEN (* Let's give RawInfo some credit for its role in checking. *) 
        INC ( GTestCt ) 
      ; INC ( GImplicitTestCt ) 
      ; INC ( GTestCtByOp [ OpTyp . RawInfo ] ) 
      END 
    END CheckContentsByRawInfo   

; PROCEDURE CheckContentsByFetchSubarray  
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; AllocRange , StdRange , AltRange : RangeTyp 
    ; By : INTEGER 
    ) 

  = VAR LNumber : CARDINAL 
  ; VAR LArrayRef : REF ARRAY OF ElemTyp 
  ; VAR LSs : SsTyp 
  ; VAR LExpectedVal : ElemTyp 

  ; BEGIN 
      LNumber := IntRanges . NumberOfRange ( AllocRange ) 
    ; LArrayRef := NEW ( REF ARRAY OF ElemTyp , LNumber )   
    ; II . FetchSubarray ( A . Var , AllocRange . Lo , LArrayRef ^ ) 
    ; FOR RI := 0 TO LNumber - 1  
      DO 
        LSs := AllocRange . Lo + RI  
      ; IF IntRanges . InRange ( LSs , AltRange ) 
        THEN 
          LExpectedVal := AltContentsBy ( A , AltRange , StdRange , LSs , By )  
        ELSIF IntRanges . InRange ( LSs , StdRange ) 
        THEN LExpectedVal := StdContents ( A , LSs )  
        ELSE LExpectedVal := A . InitElemValue 
        END
      ; WITH WElemVal = LArrayRef ^ [ RI ] 
        DO IF WElemVal # LExpectedVal 
          THEN 
            ErrorVal 
              ( A , Op , "Using FetchSubarray," , RI , WElemVal , LExpectedVal )
          END 
        END 
      END (* FOR *) 
    ; IF GFailedCheckCt = 0 
      THEN (* Let's give FetchSubarray some credit for its role in checking. *) 
        INC ( GTestCt ) 
      ; INC ( GImplicitTestCt ) 
      ; INC ( GTestCtByOp [ OpTyp . FetchSubarray ] ) 
      END 
    END CheckContentsByFetchSubarray   

; PROCEDURE CheckContentsByForAllInRange   
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; AllocRange , TRange , StdRange , AltRange : RangeTyp 
    ; By : INTEGER 
    ) 

  = VAR LVarVal : ElemTyp  

  ; PROCEDURE Callback ( CbSs : SsTyp ; VAR Elem : ElemTyp ) 

    = VAR LExpectedVal : ElemTyp 

    ; BEGIN 
        IF IntRanges . InRange ( CbSs , AltRange ) 
        THEN 
          LExpectedVal := AltContentsBy ( A , AltRange , StdRange , CbSs , By )
        ELSIF IntRanges . InRange ( CbSs , StdRange ) 
        THEN LExpectedVal := StdContents ( A , CbSs )  
        ELSE LExpectedVal := A . InitElemValue 
        END
      ; IF Elem # LExpectedVal 
        THEN 
          ErrorVal 
            ( A , Op , "Using ForAllInRange," , CbSs , LVarVal , LExpectedVal )
        END 
      END Callback 

  ; BEGIN (* CheckContentsByForAllInRange *)
      <* FATAL ANY *> BEGIN 
        II . ForAllInRange 
          ( A . Var , AllocRange . Lo , AllocRange . Hi , 1 , Callback ) 
      END (* Block *) 
    ; II . Project ( A . Var , TRange ) (* Restore original touched range. *)  
    ; IF GFailedCheckCt = 0 
      THEN (* Let's give ForAllInRange some credit for its role in checking. *) 
        INC ( GTestCt ) 
      ; INC ( GImplicitTestCt ) 
      ; INC ( GTestCtByOp [ OpTyp . ForAllInRange ] ) 
      END 
    END CheckContentsByForAllInRange 

; PROCEDURE CheckContents 
    ( READONLY A : ArrTyp 
    ; Op : OpTyp 
    ; AltRange , StdRange : RangeTyp := EmptyRange 
    ; By : INTEGER := 1 
    ) 

  = VAR LTRange , LAllocRange : RangeTyp 
  ; VAR LGroupTestCt : CARDINAL 

  ; BEGIN 
      LTRange := II . TouchedRange ( A . Var )
    ; LAllocRange := II . AllocatedRange ( A . Var )
    ; IF NOT IntRanges . IsSubrange ( LTRange , LAllocRange ) 
      THEN
        Error ( A , Op , "Allocated range does not contain touched range" ) 
      END 
    ; IF IntRanges . RangeIsEmpty ( LTRange )  
      THEN (* Actual touched range is empty. *) 
        IF IntRanges . RangeIsEmpty ( A . TouchedRange ) 
        THEN (* Both have empty touched range. *) 
        ELSE 
          Error ( A , Op , "expected nonempty touched range" ) 
        END 
      ELSE (* Actual touched range nonempty. *) 
        IF IntRanges . RangeIsEmpty ( A . TouchedRange )
        THEN 
          Error ( A , Op , "expected empty touched range" ) 
        ELSE (* Actual and expected touched ranges are both nonempty. *) 
          IF LTRange # A . TouchedRange  
          THEN (* Unequal, nonempty touched ranges. *) 
            Error ( A , Op , "wrong expected touched range" ) 
          ELSE 
            LGroupTestCt := GGroupTestCt 
          ; CheckContentsByFetch 
              ( A , Op , LAllocRange , StdRange , AltRange , By ) 
          ; CheckContentsByRawInfo ( A , Op , StdRange , AltRange , By ) 
          ; IF TRUE OR GGroupTestCt > LGroupTestCt 
            THEN 
              CheckContentsByFetchSubarray 
                ( A , Op , LAllocRange , StdRange , AltRange , By ) 
            ; IF GGroupTestCt > LGroupTestCt 
              THEN 
                CheckContentsByForAllInRange
                  ( A , Op , LAllocRange , LTRange , StdRange , AltRange , By ) 
              END (* IF *) 
            END (* IF *) 
          (* It would be really over the top to check the entire SsTyp range 
             for InitElemValue.  Instead, we check the ends and middle of the
             unallocated ranges. *) 
          ; IF FIRST ( SsTyp ) < LAllocRange . Lo 
            THEN 
              CheckUnallocValues 
                ( A , Op , RangeTyp { FIRST ( SsTyp ) , LAllocRange . Lo - 1 } )
            END (* IF *) 
          ; IF LAllocRange . Hi < LAST ( SsTyp ) 
            THEN 
              CheckUnallocValues 
                ( A , Op , RangeTyp { LAllocRange . Hi + 1 , LAST ( SsTyp ) } )
            END 
          ; IF GFailedCheckCt = 0 
            THEN (* Let's give TouchedRange and AllocatedRange some credit for 
                    their role in checking. *) 
              INC ( GTestCt , 2 ) 
            ; INC ( GImplicitTestCt , 2 ) 
            ; INC ( GTestCtByOp [ OpTyp . TouchedRange ] ) 
            ; INC ( GTestCtByOp [ OpTyp . AllocatedRange ] ) 
            END 
          END 
        END 
      END  
    END CheckContents 

; PROCEDURE SetStdElemValues ( READONLY A : ArrTyp ; Op : OpTyp ) 

  = PROCEDURE Callback ( Ss : SsTyp ; VAR Elems : ARRAY OF ElemTyp )

    = BEGIN 
        IF Ss # LTRange . Lo 
        THEN
          Error 
           ( A , Op  
           , "2ry: Wrong subscript setting standard element valuess. " 
           )  
        ELSE 
          FOR RI := 0 TO LAST ( Elems ) 
          DO 
            Elems [ RI ] := StdContents ( A , Ss + RI ) 
          END (* FOR *) 
        END 
      END Callback 

  ; VAR LTRange : RangeTyp 

  ; BEGIN (* SetStdElemValues *) 
      LTRange := II . TouchedRange ( A . Var )
    ; <* FATAL ANY *> BEGIN 
        II . CallbackWithSubarray 
          ( A . Var 
          , LTRange . Lo 
          , IntRanges . NumberOfRange ( LTRange ) 
          , Callback 
          ) 
      END 
    ; IF II . TouchedRange ( A . Var ) # LTRange 
      THEN (* This should never happen. *) 
        Error ( A , Op , "2ry: Touched changed while restoring elements." ) 
      END 
    END SetStdElemValues 

(* Tests of mutating operations. *) 

; PROCEDURE DoNew 
    ( VAR A : ArrTyp 
    ; InitElemValue : INTEGER := InitInt  
    ; InitialAlloc : RangeTyp := EmptyRange 
    ; ExpansionFactor : REAL := 1.1 
    ; Label : TEXT := "" 
    ; MaxRange : RangeTyp := IntRanges . FullRange 
    ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; CONST LOp = OpTyp . New  

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; A . TouchedRange := EmptyRange 
    ; A . InitElemValue := InitElemValue 
    ; A . ExpansionFactor := ExpansionFactor 
    ; A . Label := Label 
    ; A . MaxRange := MaxRange 
    ; SetLocalTouchedRange ( A , EmptyRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( InitialAlloc )
    ; TRY 
        A . Var := II . New ( InitElemValue , InitialAlloc , ExpansionFactor ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckInt 
        ( A 
        , OpTyp . InitElemValue 
        , II . InitElemValue ( A . Var )  
        , InitElemValue 
        , "InitElemValue" 
        ) 
    ; IF GFailedCheckCt = 0 
      THEN (* Let's give some credit to InitElemValue. *) 
        INC ( GTestCt ) 
      ; INC ( GImplicitTestCt ) 
      ; INC ( GTestCtByOp [ OpTyp . InitElemValue ] ) 
      END 
    ; CheckContents ( A , LOp ) 
    ; CheckAlloc ( A , LOp , InitialAlloc ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoNew 

; PROCEDURE DoTouch ( VAR A : ArrTyp ; TRange : RangeTyp ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange : RangeTyp 
  ; CONST LOp = OpTyp . Touch   

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , TRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; TRY II . Touch ( A . Var , TRange ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , StdRange := LOldTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoTouch 

; PROCEDURE DoProject ( VAR A : ArrTyp ; PRange : RangeTyp ) 

  = VAR LOldTRange , LNewTRange : RangeTyp 
  ; VAR LOldAllocRange : RangeTyp 
  ; CONST LOp = OpTyp . Project 
 
  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldAllocRange := II . AllocatedRange ( A . Var ) 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LNewTRange := IntRanges . IntersectionRange ( LOldTRange , PRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; II . Project ( A . Var , PRange ) 
    ; CheckContents ( A , LOp , StdRange := LNewTRange ) 
    ; CheckAlloc ( A , LOp , LOldAllocRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoProject 

; PROCEDURE DoAssign ( VAR A : ArrTyp ; Ss : SsTyp ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange , LSsRange : RangeTyp 
  ; VAR LValue : ElemTyp 
  ; CONST LOp = OpTyp . Assign  

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LSsRange := RangeTyp { Ss , Ss }  
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , LSsRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; LValue := AltContents ( A , Ss ) 
    ; TRY II . Assign ( A . Var , Ss , LValue ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , LSsRange , StdRange := LOldTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoAssign 

; PROCEDURE DoAssignSubarray ( VAR A : ArrTyp ; AssignRange : RangeTyp ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange : RangeTyp 
  ; VAR LNumber : CARDINAL 
  ; VAR LValues : REF ARRAY OF ElemTyp 
  ; CONST LOp = OpTyp . AssignSubarray  

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , AssignRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; LNumber := MAX ( AssignRange . Hi - AssignRange . Lo + 1 , 0 ) 
    ; LValues := NEW ( REF ARRAY OF ElemTyp , LNumber ) 
    ; FOR RI := 0 TO LNumber - 1 
      DO LValues ^ [ RI ] := AltContents ( A , AssignRange . Lo + RI ) 
      END 
    ; TRY II . AssignSubarray ( A . Var , AssignRange . Lo , LValues ^ ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , AssignRange , StdRange := LOldTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoAssignSubarray 

; PROCEDURE DoCallbackWithElem ( VAR A : ArrTyp ; Ss : SsTyp ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange , LSsRange : RangeTyp 
  ; VAR LValue : ElemTyp 
  ; CONST LOp = OpTyp . CallbackWithElem 

  ; PROCEDURE Callback ( CbSs : SsTyp ; VAR Elem : ElemTyp ) 

    = BEGIN 
        CheckInt ( A , LOp , CbSs , Ss , "callback subscript" ) 
      ; Elem := LValue 
      END Callback 

  ; BEGIN (* DoCallbackWithElem *)
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LSsRange := RangeTyp { Ss , Ss } 
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , LSsRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; LValue := AltContents ( A , Ss ) 
    ; <* FATAL ANY *> BEGIN  
        TRY II . CallbackWithElem ( A . Var , Ss , Callback ) 
        EXCEPT II . AllocationFailure 
        => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
        END 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , LSsRange , StdRange := LOldTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoCallbackWithElem 

; PROCEDURE DoCallbackWithSubarray 
    ( VAR A : ArrTyp ; Lo : SsTyp ; Number : CARDINAL ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange , LSsRange : RangeTyp
  ; VAR LActualNumber : CARDINAL 
  ; VAR LValues : REF ARRAY OF ElemTyp 
  ; CONST LOp = OpTyp . CallbackWithSubarray  

  ; PROCEDURE Callback 
      ( CbLo : SsTyp ; VAR Elems : ARRAY OF ElemTyp ) 

    = VAR LElemsNumber : CARDINAL 

    ; BEGIN 
        LElemsNumber := NUMBER ( Elems ) 
      ; CheckInt ( A , LOp , CbLo , LSsRange . Lo , "callback subscript" ) 
      ; CheckInt 
          ( A , LOp , LElemsNumber , LActualNumber , "callback array number" )
      ; Elems := LValues ^  
      END Callback 

  ; BEGIN (* DoCallbackWithSubarray *) 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LSsRange := IntRanges . RangeOfLoPlusNumber ( Lo , Number ) 
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , LSsRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LActualNumber := IntRanges . NumberOfRange ( LSsRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; LValues := NEW ( REF ARRAY OF ElemTyp , LActualNumber ) 
    ; FOR RI := 0 TO LActualNumber - 1 
      DO LValues ^ [ RI ] := AltContents ( A , LSsRange . Lo + RI ) 
      END 
    ; <* FATAL ANY *> BEGIN
        TRY 
          II . CallbackWithSubarray 
            ( A . Var , LSsRange . Lo , Number , Callback )  
        EXCEPT II . AllocationFailure 
        => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
        END 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , LSsRange , StdRange := LOldTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoCallbackWithSubarray 

; PROCEDURE DoForAllTouchedInRange 
    ( VAR A : ArrTyp 
    ; From : SsTyp := FIRST ( SsTyp ) 
    ; To : SsTyp := LAST ( SsTyp ) 
    ; By : INTEGER := 1  
    ) 

  = VAR LTRange , LFRange , LDoRange : RangeTyp 
  ; VAR LSs : SsTyp 
  ; CONST LOp = OpTyp . ForAllTouchedInRange   

  ; PROCEDURE Callback 
      ( CbSs : SsTyp ; VAR Elem : ElemTyp ) 

    = BEGIN 
        CheckInt ( A , LOp , CbSs , LSs , "callback subscript" ) 
      ; Elem := AltContents ( A , CbSs ) 
      ; INC ( LSs , By ) (* Could overflow, but then won't be used. *) 
      END Callback 

  ; BEGIN (* DoForAllTouchedInRange *) 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LTRange := II . TouchedRange ( A . Var ) 
    ; CASE Integer . Compare ( By , 0 ) 
      OF - 1 
      =>  LFRange := RangeTyp { To , From } 
      | 0 (* Don't try this at home. *) 
      =>  RETURN 
      | 1 
      =>  LFRange := RangeTyp { From , To } 
      END (* CASE *) 
    ; LDoRange := IntRanges . IntersectionRange ( LFRange , LTRange ) 
    ; IF By > 0 
      THEN LSs := LDoRange . Lo  
      ELSE LSs := LDoRange . Hi
      END (* IF *)   
    ; <* FATAL ANY *> BEGIN 
        II . ForAllTouchedInRange ( A . Var , From , To , By , Callback ) 
      END 
    ; CheckContents ( A , LOp , LDoRange , StdRange := LTRange , By := By ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoForAllTouchedInRange  

; PROCEDURE DoForAllInRange 
    ( VAR A : ArrTyp ; From , To : SsTyp ; By : INTEGER := 1 ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange , LDoRange : RangeTyp 
  ; VAR LSs : SsTyp 
  ; CONST LOp = OpTyp . ForAllInRange   

  ; PROCEDURE Callback 
      ( CbSs : SsTyp ; VAR Elem : ElemTyp ) 

    = BEGIN 
        CheckInt ( A , LOp , CbSs , LSs , "callback subscript" ) 
      ; Elem := AltContents ( A , CbSs ) 
      ; INC ( LSs , By ) (* Could overflow, but then won't be used. *) 
      END Callback 

  ; BEGIN (* DoForAllInRange *) 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; CASE Integer . Compare ( By , 0 ) 
      OF - 1 
      =>  LDoRange := RangeTyp { To , From } 
      | 0 (* Don't try this at home. *) 
      =>  RETURN 
      | 1 
      =>  LDoRange := RangeTyp { From , To } 
      END (* CASE *) 
    ; LNewTRange := IntRanges . EnclosingRange ( LOldTRange , LDoRange ) 
    ; SetLocalTouchedRange ( A , LNewTRange ) 
    ; LExpectAllocFailure := NOT IntRanges . RangeIsAllocatable ( LNewTRange )
    ; LSs := From 
    ; <* FATAL ANY *> BEGIN 
        TRY II . ForAllInRange ( A . Var , From , To , By , Callback ) 
        EXCEPT II . AllocationFailure 
        => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
        END 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( A , LOp , LDoRange , StdRange := LOldTRange , By := By ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoForAllInRange  

; PROCEDURE DoCopy 
    ( READONLY A : ArrTyp 
    ; VAR CopiedA : ArrTyp 
    ; PRange : RangeTyp := FullRange 
    ; AllocRange : RangeTyp := EmptyRange 
    ) 

  = VAR LExpectAllocFailure : BOOLEAN 
  ; VAR LOldTRange , LNewTRange , LNewAllocRange : RangeTyp 
  ; CONST LOp = OpTyp . Copy    

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; CopiedA := A 
    ; LOldTRange := II . TouchedRange ( A . Var ) 
    ; LNewTRange := IntRanges . IntersectionRange ( PRange , LOldTRange ) 
    ; SetLocalTouchedRange ( CopiedA , LNewTRange ) 
    ; IF AllocRange = EmptyRange 
      THEN (* Preserve the allocation *) 
        LNewAllocRange := II . AllocatedRange ( A . Var ) 
      ELSE 
        LNewAllocRange := IntRanges . EnclosingRange ( AllocRange , LNewTRange )
      END (* IF *) 
    ; LExpectAllocFailure 
        := NOT IntRanges . RangeIsAllocatable ( LNewAllocRange )
    ; TRY 
        CopiedA . Var := II . Copy ( A . Var , PRange , AllocRange ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , LExpectAllocFailure ) 
      END 
    ; CheckExcept ( A , LOp , FALSE , LExpectAllocFailure ) 
    ; CheckContents ( CopiedA , LOp , StdRange := LNewTRange ) 
    ; CheckAlloc ( CopiedA , LOp , LNewAllocRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoCopy 

; PROCEDURE DoCompact ( VAR A : ArrTyp  ) 

  = VAR LTRange : RangeTyp 
  ; CONST LOp = OpTyp . Compact    

  ; BEGIN 
      GFailedCheckCt := 0 
    ; GImplicitTestCt := 0 
    ; LTRange := II . TouchedRange ( A . Var ) 
    ; TRY II . Compact ( A . Var ) 
      EXCEPT II . AllocationFailure 
      => CheckExcept ( A , LOp , TRUE , FALSE ) 
         (* Although Compact can raise AllocationFailure, it is only if 
            NEW can't deliver, and we can't make this happen. *) 
      END 
    ; CheckExcept ( A , LOp , FALSE , FALSE ) 
    ; CheckContents ( A , LOp , StdRange := LTRange ) 
    ; CheckAlloc ( A , LOp , LTRange ) 
    ; CountTest ( A , LOp ) 
    ; SetStdElemValues ( A , LOp ) 
    END DoCompact 

; VAR GGroupTestCt , GFillCt , GHardCt , GCharCt , GRandCt : INTEGER := 0 

; VAR Min := -128 
; VAR Max := 127 
; VAR MaxRange : IntRanges . RangeTyp 

; VAR A : IC . T 
; PROCEDURE MiscCharTests ( ) 

  = VAR LTouchedRange : IntRanges . RangeTyp 
  ; VAR LVal0 , LVal1 , LValM1 , LValMin , LValMax : CHAR 
  ; VAR LInit : CHAR 
  ; VAR LArr : ARRAY [ 0 .. 9 ] OF CHAR 
             := ARRAY [ 0 .. 9 ] OF CHAR 
                { 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' } 

  ; BEGIN 
      WT ( "Doing hard coded CHAR tests." ) 
    ; NL ( ) 
    ; GGroupTestCt := 0 
    ; Spinner . ResetProgress ( ) 
    ; TRY A := IC . New ( ';' , MaxRange ) 
      EXCEPT IC . AllocationFailure 
      => (* Not very likely. *)  
        WT ( " AllocationFailure doing CHAR New ." ) 
      ; NL ( ) 
      ; INC ( GFailedCheckCt ) 
      END (* EXCEPT *) 
    ; TRY IC . Assign ( A , -5 , 'A' ) 
      EXCEPT IC . AllocationFailure 
      => (* Not very likely. *)  
        WT ( " AllocationFailure doing CHAR Assign ." ) 
      ; NL ( ) 
      ; INC ( GFailedCheckCt ) 
      END (* EXCEPT *) 
    ; LTouchedRange := IC . TouchedRange ( A ) 
    ; LVal0 := IC . Fetch ( A , 0 ) 
    ; LVal1 := IC . Fetch ( A , 1 ) 
    ; LValM1 := IC . Fetch ( A , - 1 ) 
    ; LValMin := IC . Fetch ( A , Min ) 
    ; LValMax := IC . Fetch ( A , Max )

    ; TRY IC . AssignSubarray ( A , 2 , SUBARRAY ( LArr , 0 , 0 ) )  
      EXCEPT IC . AllocationFailure 
      => (* Not very likely. *)  
        WT ( " AllocationFailure doing CHAR AssignSubarray ." ) 
      ; NL ( ) 
      ; INC ( GFailedCheckCt ) 
      END (* EXCEPT *) 

    ; LInit := IC . InitElemValue ( A )
    ; Spinner . ShowExactProgress ( GGroupTestCt ) 
    ; WT ( " Hard coded CHAR tests." ) 
    ; NL ( ) 
    ; NL ( ) 
    ; GCharCt := GGroupTestCt  
    END MiscCharTests 

; VAR B , C , D : ArrTyp 
; VAR E , F , G : ArrTyp 
; VAR FirstSs := FIRST ( SsTyp )  
; VAR LastSs := LAST ( SsTyp )  
; CONST InitInt = FIRST ( ElemTyp ) 

; PROCEDURE HardCodedIntTests ( ) 

  = BEGIN 
      WT ( "Doing hard coded INTEGER tests." ) 
    ; NL ( ) 
    ; GGroupTestCt := 0 
    ; Spinner . ResetProgress ( ) 
    ; DoNew ( B , Label := "B" , MaxRange := RangeTyp { - 256 , 255 } ) 
    ; DoNew 
        ( C , Label := "C" , MaxRange := RangeTyp { FirstSs , FirstSs + 511 } ) 
    ; DoNew 
        ( D , Label := "D" , MaxRange := RangeTyp { LastSs - 511 , LastSs } ) 

    ; DoAssign ( B , 0 ) 
    ; DoAssign ( C , FirstSs + 3 ) 
    ; DoAssign ( C , FirstSs ) 
    ; DoAssign ( D , LastSs - 7 ) 
    ; DoAssign ( D , LastSs ) 
    
    ; DoTouch ( B , RangeTyp { - 2 , 2 } ) 
    ; DoTouch ( C , RangeTyp { FirstSs , FirstSs + 3 } ) 
    ; DoTouch ( D , RangeTyp { LastSs - 4 , LastSs } ) 
    ; DoTouch ( D , IntRanges . EmptyRange ) 

    ; DoAssignSubarray ( B , RangeTyp { - 5 , 5 } ) 
    ; DoAssignSubarray ( C , RangeTyp { FirstSs , FirstSs + 10 } ) 
    ; DoAssignSubarray ( D , RangeTyp { LastSs - 10 , LastSs } ) 

    ; DoTouch ( B , RangeTyp { - 2 , 2 } ) 
    ; DoTouch ( C , RangeTyp { FirstSs , FirstSs + 3 } ) 
    ; DoTouch ( D , RangeTyp { LastSs - 4 , LastSs } ) 

    ; DoTouch ( B , RangeTyp { - 10 , 10 } ) 
    ; DoTouch ( C , RangeTyp { FirstSs , FirstSs + 20 } ) 
    ; DoTouch ( D , RangeTyp { LastSs -20 , LastSs } ) 

    ; DoProject ( B , RangeTyp { -3 , 3 } ) 
    ; DoCompact ( B ) 
    ; DoAssignSubarray ( B , RangeTyp { - 3 , 5 } ) (* Expand up. *)  
    ; DoAssignSubarray ( B , RangeTyp { - 5 , 5 } ) (* Down. *) 
    ; DoAssignSubarray ( B , RangeTyp { - 10 , 10 } ) (* Up and down. *) 

    ; DoProject ( C , RangeTyp { FirstSs + 5, FirstSs + 8 } ) 
    ; DoCompact ( C ) 
    ; DoAssignSubarray ( C , RangeTyp { FirstSs + 5 , FirstSs + 15 } ) (* Up. *)
    ; DoAssignSubarray ( C , RangeTyp { FirstSs + 3 , FirstSs + 15 } ) (* Dn. *)
    ; DoAssignSubarray ( C , RangeTyp { FirstSs  , FirstSs + 20 } ) (* Both. *)

    ; DoProject ( D , RangeTyp { LastSs - 10 , LastSs - 5 } ) 
    ; DoCompact ( D ) 
    ; DoAssignSubarray ( D , RangeTyp { LastSs - 10 , LastSs - 3 } ) (* Up. *)
    ; DoAssignSubarray ( D , RangeTyp { LastSs - 15 , LastSs - 3 } ) (* Down. *)
    ; DoAssignSubarray ( D , RangeTyp { LastSs - 20 , LastSs } ) (* Both. *)

    ; DoCallbackWithElem ( B , 0 ) 
    ; DoCallbackWithElem ( C , FirstSs ) 
    ; DoCallbackWithElem ( C , FirstSs + 3 ) 
    ; DoCallbackWithElem ( D , LastSs ) 
    ; DoCallbackWithElem ( D , LastSs - 7 ) 
    
    ; DoCallbackWithSubarray ( B , - 8 , 24 ) 
    ; DoCallbackWithSubarray ( C , FirstSs , 10 ) 
    ; DoCallbackWithSubarray ( D , LastSs - 12 , 12 ) 

    ; DoForAllTouchedInRange ( B , - 5 , 5 ) 
    ; DoForAllTouchedInRange ( C , FirstSs + 1 , FirstSs + 2 ) 
    ; DoForAllTouchedInRange ( D , LastSs - 5 , LastSs - 3 ) 

    ; DoForAllTouchedInRange ( B ) 
    ; DoForAllTouchedInRange ( C ) 
    ; DoForAllTouchedInRange ( D ) 

    ; DoForAllTouchedInRange ( B , - 5 , 5 , By := - 1 ) 
    ; DoForAllTouchedInRange ( C , FirstSs + 1 , FirstSs + 2 , By := - 1 ) 
    ; DoForAllTouchedInRange ( D , LastSs - 5 , LastSs - 3 , By := - 1 ) 

    ; DoForAllTouchedInRange ( B , By := - 1 ) 
    ; DoForAllTouchedInRange ( C , By := - 1 ) 
    ; DoForAllTouchedInRange ( D , By := - 1 ) 

    ; DoForAllInRange ( B , - 5 , 6 ) 
    ; DoForAllInRange ( C , FirstSs + 1 , FirstSs + 2 ) 
    ; DoForAllInRange ( D , LastSs - 5 , LastSs - 3 ) 

    ; DoForAllInRange ( B , - 5 , 5 , By := - 1 ) 
    ; DoForAllInRange ( C , FirstSs + 1 , FirstSs + 2 , By := - 1 ) 
    ; DoForAllInRange ( D , LastSs - 5 , LastSs - 3 , By := - 1 ) 

    ; DoForAllInRange ( B , - 20 , 20 ) 
    ; DoForAllInRange ( C , FirstSs , FirstSs + 20 ) 
    ; DoForAllInRange ( D , LastSs - 22 , LastSs  ) 

    ; DoForAllInRange ( B , - 25 , 25 , By := - 1 ) 
    ; DoForAllInRange ( C , FirstSs , FirstSs + 25 , By := - 1 ) 
    ; DoForAllInRange ( D , LastSs - 25 , LastSs , By := - 1 ) 

    ; DoCopy ( B , (* VAR *) E )  
    ; DoCopy ( C , (* VAR *) F )  
    ; DoCopy ( D , (* VAR *) G )  

    ; DoProject ( E , RangeTyp { -9 , 9 } ) 
    ; DoProject ( F , RangeTyp { FirstSs , FirstSs + 9 } ) 
    ; DoProject ( G , RangeTyp { LastSs - 11 , LastSs } ) 

    ; DoCompact ( E ) 
    ; DoCompact ( F ) 
    ; DoCompact ( G ) 

    ; DoCopy ( B , (* VAR *) E , AllocRange := EmptyRange )  
    ; DoCopy ( C , (* VAR *) F , AllocRange := EmptyRange )  
    ; DoCopy ( D , (* VAR *) G , AllocRange := EmptyRange )  

    ; DoCompact ( E ) 
    ; DoCompact ( F ) 
    ; DoCompact ( G ) 

    ; DoCopy ( B , (* VAR *) E , PRange := RangeTyp { - 2 , 3 } )  
    ; DoCopy ( C , (* VAR *) F , PRange := RangeTyp { FirstSs , FirstSs + 3 } ) 
    ; DoCopy ( D , (* VAR *) G , PRange := RangeTyp { LastSs - 4 , LastSs } )  

    ; DoCompact ( E ) 
    ; DoCompact ( F ) 
    ; DoCompact ( G ) 

    ; DoCopy 
        ( B , (* VAR *) E , PRange := RangeTyp { - 2 , 3 } 
        , AllocRange := EmptyRange 
        )  
    ; DoCopy 
        ( C , (* VAR *) F , PRange := RangeTyp { FirstSs , FirstSs + 3 } 
        , AllocRange := EmptyRange 
        )  
    ; DoCopy 
        ( D , (* VAR *) G , PRange := RangeTyp { LastSs - 4 , LastSs } 
        , AllocRange := EmptyRange 
        )  

    ; DoCompact ( E ) 
    ; DoCompact ( F ) 
    ; DoCompact ( G ) 

    ; DoCopy 
        ( B , (* VAR *) E , PRange := RangeTyp { FirstSs , FirstSs + 3 } 
        , AllocRange := EmptyRange 
        )  
    ; DoCopy 
        ( C , (* VAR *) F , PRange := RangeTyp { LastSs - 4 , LastSs } 
        , AllocRange := EmptyRange 
        )  
    ; DoCopy 
        ( D , (* VAR *) G , PRange := RangeTyp { 0 , 10 } 
        , AllocRange := EmptyRange 
        )  

    ; DoCompact ( E ) 
    ; DoCompact ( F ) 
    ; DoCompact ( G ) 

    ; Spinner . ShowExactProgress ( GGroupTestCt ) 
    ; WT ( " Hard coded INTEGER tests." ) 
    ; NL ( ) 
    ; NL ( ) 
    ; GHardCt := GGroupTestCt  
    END HardCodedIntTests 

; VAR RandV := NEW ( Random . Default ) . init ( fixed := TRUE ) 

; CONST ArrMax = 100 
; VAR GArrs : ARRAY [ 0 .. ArrMax - 1 ] OF ArrTyp  
; VAR GArrsNext : INTEGER := 0  

; VAR GInitialArrsCt : CARDINAL := 3 * 7 
      (* Seven sizes, lo, middle, and hi end of range. *) 

; PROCEDURE FillArrs ( ) 

  = VAR LRange : CARDINAL 
  ; VAR LLo , LHi : SsTyp 
  ; VAR LLabel : TEXT 

  ; BEGIN 
      WT ( "Doing array creation tests." ) 
    ; NL ( ) 
    ; GGroupTestCt := 0 
    ; Spinner . ResetProgress ( ) 
    ; LRange := 10 
    ; GArrsNext := FIRST ( GArrs ) 
    ; LOOP 
        IF GArrsNext >= GInitialArrsCt 
        THEN EXIT 
        ELSE 

          CASE GArrsNext MOD 3 
          OF 0 
          =>  LLo := FirstSs 
            ; LHi := LLo + LRange - 1 
            ; LLabel := "Lo" & Fmt . Int ( LRange ) 
          | 1  
          =>  LLo := - ( LRange DIV 2 )  
            ; LHi := LLo + LRange - 1 
            ; LLabel := "Mid" & Fmt . Int ( LRange ) 
          | 2 
          =>  LHi := LastSs 
            ; LLo := LHi - LRange + 1 
            ; LLabel := "Hi" & Fmt . Int ( LRange ) 
            ; IF LRange <= 40690 
              THEN LRange := 4 * LRange 
              END (* IF *) 
          END (* CASE *) 
        ; DoNew 
            ( GArrs [ GArrsNext ] 
            , Label := LLabel 
            , MaxRange := RangeTyp { LLo , LHi } 
            ) 
        ; INC ( GArrsNext ) 
        END (* IF *) 
      END (* LOOP *) 
    ; Spinner . ShowExactProgress ( GGroupTestCt ) 
    ; WT ( " Array creation tests." ) 
    ; NL ( ) 
    ; NL ( ) 
    ; GFillCt := GGroupTestCt  
    END FillArrs 

; PROCEDURE RandRange ( READONLY A : ArrTyp ) : RangeTyp 

  = VAR LLo , LHi : SsTyp 
  ; VAR LResult : RangeTyp 

  ; BEGIN 
      LLo := RandV . integer ( A . MaxRange . Lo , A . MaxRange . Hi ) 
    ; LHi := RandV . integer ( A . MaxRange . Lo , A . MaxRange . Hi ) 
    ; IF LLo <= LHi (* Nonempty. *) 
         OR RandV . integer ( 0 , 9 ) = 0 
            (* ^Make empty ranges lower probability. *)  
      THEN LResult := RangeTyp { LLo , LHi } 
      ELSE (* Swap and make it nonempty. *) 
        LResult := RangeTyp { LHi , LLo } 
      END (* IF *) 
    ; RETURN LResult 
    END RandRange 

; PROCEDURE RandLoNumber 
    ( READONLY A : ArrTyp ; VAR Lo : SsTyp ; VAR Number : CARDINAL ) 

  = BEGIN 
      Lo := RandV . integer ( A . MaxRange . Lo , A . MaxRange . Hi ) 
    ; Number := RandV . integer ( 0 , A . MaxRange . Hi - Lo + 1) 
    END RandLoNumber  

; PROCEDURE RandSs ( READONLY A : ArrTyp ) : SsTyp 

  = VAR LResult : SsTyp 

  ; BEGIN 
      LResult := RandV . integer ( A . MaxRange . Lo , A . MaxRange . Hi ) 
    ; RETURN LResult 
    END RandSs 

; PROCEDURE RandBy ( <* UNUSED *> READONLY A : ArrTyp ) : INTEGER 

  = VAR LResult : INTEGER 

  ; BEGIN 
      LResult := RandV . integer ( - 4 , 4 ) 
    ; IF LResult = 0 THEN LResult := 1 END 
    ; RETURN LResult 
    END RandBy 

; PROCEDURE RandomIntTests ( TestCt : CARDINAL ) 

  = VAR LArrSs : INTEGER  
  ; VAR LLo : SsTyp 
  ; VAR LNumber : CARDINAL 
  ; VAR LRange : RangeTyp 
  ; VAR LCopyArr : ArrTyp 

  ; BEGIN 
      WT ( "Doing random tests." ) 
    ; NL ( ) 
    ; GGroupTestCt := 0 
    ; Spinner . ResetProgress ( ) 
    ; FOR RI := 0 TO TestCt 
      DO 
        LArrSs := RandV . integer ( FIRST ( GArrs ) , GArrsNext - 1 ) 
      ; WITH WArr = GArrs [ LArrSs ] 
        DO IF WArr . Var # NIL 
          THEN (* Paranoid.  Shouldn't fail. *) 
            CASE RandV . integer ( 0 , 10 ) 
            OF 0 => (* New *) 
            | 1 (* Touch *) 
            => DoTouch ( WArr , RandRange ( WArr ) ) 
            | 2 (* Project *) 
            => DoProject ( WArr , RandRange ( WArr ) ) 
            | 3 (* Assign *) 
            => DoAssign ( WArr , RandSs ( WArr ) ) 
            | 4 (* AssignSubarray *) 
            => DoAssignSubarray ( WArr , RandRange ( WArr ) ) 
            | 5 (* CallbackWithElem *) 
            => DoCallbackWithElem ( WArr , RandSs ( WArr ) ) 
            | 6 (* CallbackWithSubarray *) 
            =>  RandLoNumber ( WArr , (* VAR *) LLo , (* VAR *) LNumber ) 
              ; DoCallbackWithSubarray ( WArr , LLo , LNumber ) 
            | 7 (* ForAllTouchedInRange *) 
            =>  LRange := RandRange ( WArr )
              ; DoForAllTouchedInRange 
                  ( WArr , LRange . Lo , LRange . Hi , RandBy ( WArr ) )
            | 8 (* ForAllInRange *) 
            => LRange := RandRange ( WArr )
              ; DoForAllInRange 
                  ( WArr , LRange . Lo , LRange . Hi , RandBy ( WArr ) )
            | 9 (* Copy *) 
            =>  DoCopy 
                  ( WArr 
                  , (* VAR *) LCopyArr 
                  , PRange := RandRange ( WArr ) 
                  , AllocRange := RandRange ( WArr ) 
                  )
               ; IF RandV . integer ( 0 , 9 ) = 0 
                 THEN (* Reduce the probability of doing this. *) 
                   IF GArrsNext <= LAST ( GArrs ) 
                   THEN 
                     GArrs [ GArrsNext ] := LCopyArr 
                   ; INC ( GArrsNext ) 
                   END (* IF *) 
                 END (* IF *) 
            | 10 (* Compact *) 
            =>  DoCompact ( WArr )  
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    ; Spinner . ShowExactProgress ( GGroupTestCt ) 
    ; WT ( " Random tests." ) 
    ; NL ( ) 
    ; NL ( ) 
    ; GRandCt := GGroupTestCt  
    END RandomIntTests 

; PROCEDURE Init ( ) 

  = BEGIN 
      MaxRange := IntRanges . RangeTyp { Min , Max }
    ; Spinner . SpinCt := 10  
    ; GHardCt := 0 
    ; GCharCt := 0 
    ; GFillCt := 0 
    ; GRandCt := 0 
    END Init 

; VAR ReportIndividualTests := FALSE 

; BEGIN 
    Init ( ) 
  ; ZeroStats ( ) 
  ; WT ( "Tests of VarArray. " ) 
  ; NL ( ) 
  ; NL ( ) 
  ; MiscCharTests ( ) 
  ; HardCodedIntTests ( ) 
  ; FillArrs ( ) 
  ; RandomIntTests ( 100000 ) 
  ; WriteStats ( ) 
  END TestVarArray 
. 
