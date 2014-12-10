 
(* -----------------------------------------------------------------------1- *)
(* File Test.i3  Modula-3 source code.  for OrdSets.                         *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

MODULE Test EXPORTS Main 

; IMPORT FileWr 
; IMPORT Fmt
; IMPORT Params 
; IMPORT Pickle2 
; IMPORT Random 
; IMPORT Stdio
; IMPORT Text 
; IMPORT Tick
; IMPORT Time
; IMPORT Thread
; IMPORT Wr 

; IMPORT IntSets 
; IMPORT Sets 
; IMPORT UnsafeUtils 
; IMPORT WidecharSets 
  (* ^This is only to ensure a non-integer instantiation will compile. *) 

; VAR WrT : Wr . T 
; VAR PWrT : Wr . T 

; CONST MaxSets = 500000 (* The maximum number retained at any one time. *) 
(* BUG NOTE:      ^Setting this to 7000000 causes m3cc(aka m3cgc) to segfault
                  trying to compile this.
*) 

; CONST IntBias = - 1024 
  (* ^This is for testing IntSet's working on negative element values.  The
      Sets package will not handle this, so an element E is present in a 
      Sets.T iff E+IntBias is present in the matching IntSets.T.
      The bias is added when passing element values in to IntSets and 
      removed when getting them back, so all comparisons and displays of
      values use unbiased (therefore nonnegative) values.  
      However, the bias is inconvenient for debugging, so IntBias can be set to
      zero for most debugging.     
  *) 
; CONST TestCtPad = 8 
; CONST HexPad = BYTESIZE( REFANY ) - 1

; VAR GOldSets := ARRAY [ 0 .. MaxSets ] OF Sets . T { NIL , .. }
; VAR GNewSets := ARRAY [ 0 .. MaxSets ] OF IntSets . T { NIL , .. }
; VAR GSetNos := ARRAY [ 0 .. MaxSets ] OF INTEGER { - 1 , .. } 
; VAR GStoredSetCt : CARDINAL := 1 
; VAR GTotalSetCt : CARDINAL := 0 
; VAR GSetNoToStopOn : INTEGER := FIRST ( INTEGER ) 

; VAR RandV := NEW ( Random . Default ) . init ( fixed := TRUE ) 

; VAR GFillCt := 0 
; VAR GOperationCt := 0 
; VAR GImageCt := 0 
; VAR GPredCt := 0 
; VAR GTotalCt := 0 
; VAR GFailureCt := 0 
; VAR GCompareCt := 0  

; TYPE TimeTyp = LONGREAL 
; VAR GOldSetsTick : TimeTyp := 0.0D0 
; VAR GNewSetsTick : TimeTyp := 0.0D0  
; VAR GOldSetsTime : TimeTyp := 0.0D0 
; VAR GNewSetsTime : TimeTyp := 0.0D0  
; VAR GTimingOverheadTick : TimeTyp 
; VAR GTimingOverheadTime : TimeTyp 

; VAR GStartTimeOld , GStopTimeOld : Time . T 
; VAR GStartTimeNew , GStopTimeNew : Time . T 
; VAR GStartTickOld , GStopTickOld : Tick . T 
; VAR GStartTickNew , GStopTickNew : Tick . T 

; VAR GOldTimedCt , GNewTimedCt : INTEGER 

; VAR GTimeOldCopies : BOOLEAN := TRUE 

; CONST GLen = 80 
; VAR GProgressLine : ARRAY [ 0 .. GLen ] OF CHAR 
; VAR GProgressLen : CARDINAL := 0 

; VAR GDoImageTests : BOOLEAN := FALSE 
; VAR GDoOld : BOOLEAN := FALSE 
; VAR GDoNew : BOOLEAN := FALSE 
; VAR GDoCompareTests : BOOLEAN := FALSE 
; VAR GDoRandomTests : BOOLEAN := FALSE 
; VAR GDoSymDiff : BOOLEAN := FALSE 
; VAR GDoCompareOperands : BOOLEAN := FALSE 
; VAR GDoCompareResults : BOOLEAN := FALSE 
; VAR GDoWritePickle : BOOLEAN := FALSE 
; VAR GDoDisplayHelp : BOOLEAN := FALSE 
; VAR GDoDisplayVersion : BOOLEAN := FALSE 

; PROCEDURE GetParams ( ) 

  = VAR LParam : TEXT 
  ; VAR LChar : CHAR 

  ; BEGIN 
      GDoImageTests := FALSE 
    ; GDoOld := FALSE 
    ; GDoNew := FALSE 
    ; GDoCompareTests := FALSE 
    ; GDoRandomTests := FALSE 
    ; GDoSymDiff := FALSE 
    ; GDoCompareOperands := FALSE 
    ; GDoCompareResults := FALSE 
    ; GDoWritePickle := FALSE 
    ; GDoDisplayHelp := FALSE 
    ; GDoDisplayVersion := FALSE 

    ; FOR RParamNo := 1 TO Params . Count - 1 
      DO
        LParam := Params . Get ( RParamNo ) 
      ; IF Text . GetChar ( LParam , 0 ) = '-' 
        THEN
          FOR RCharNo := 1 TO Text . Length ( LParam ) - 1  
          DO
            LChar := Text . GetChar ( LParam , RCharNo ) 
          ; CASE LChar 
            OF 'a' 
            => GDoCompareTests := TRUE  
            ; GDoImageTests := TRUE 
            ; GDoRandomTests := TRUE 
            ; GDoNew := TRUE  
            ; GDoCompareOperands := TRUE  
            ; GDoCompareResults := TRUE  
            ; GDoOld := TRUE 
            ; GDoSymDiff := TRUE 
            | 'c' => GDoCompareTests := TRUE 
            | 'd' => GDoRandomTests := TRUE 
            | 'h' => GDoDisplayHelp := TRUE 
            | 'i' => GDoImageTests := TRUE 
            | 'o' => GDoNew := TRUE  
            | 'p' => GDoCompareOperands := TRUE  
            | 'P' => GDoWritePickle := TRUE 
            ; GDoRandomTests := TRUE 
            | 'r' => GDoCompareResults := TRUE  
            | 's' => GDoOld := TRUE 
            | 'v' => GDoDisplayVersion := TRUE 
            | 'y' => GDoSymDiff := TRUE 
            ELSE 
              WL ( "Invalid option character: \'" 
                   & Text . FromChar ( LChar )  & "\'"  
                 ) 
            ; GDoDisplayHelp := TRUE 
            END 
          END (* FOR *) 
        ELSE
          WL ( "Invalid parameter: \"" & LParam & "\"" ) 
        ; GDoDisplayHelp := TRUE 
        END (* IF *)  
      END (* FOR *)  
    END GetParams 

; PROCEDURE WL ( T : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , T ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    END WL 

; VAR PickleFileName := "Sets.pkl" 

; PROCEDURE DisplayHelp ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WL ( "Usage: OrdSetsTest {-{option}}" ) 
    ; WL ( "  Options are: " ) 
    ; WL ( "  -a Do all tests, -i, -o, -p, -r, -s, -y." ) 
    ; WL ( "  -c Run fixed tests on Compare." ) 
    ; WL ( "  -d Run many randomly generated tests." ) 
    ; WL ( "  -h Display help text and exit." ) 
    ; WL ( "  -i Run tests on Image function." ) 
    ; WL ( "  -o Test the new OrdSets module (instantiated for INTEGER)." ) 
    ; WL ( "  -p Compare IntSet/OrdSet values of operands of operations." ) 
    ; WL ( "     (only if both -o and -s are specified." ) 
    ; WL ( "  -P Write pickle of random sets to file \""
           & PickleFileName 
           & "\"." 
         ) 
    ; WL ( "  -r Compare IntSet/OrdSet values of results of operations." ) 
    ; WL ( "     (only if both -o and -s are specified." ) 
    ; WL ( "  -s Test the old Sets module." ) 
    ; WL ( "  -v Display version and exit." ) 
    ; WL ( "  -y Test SymDiff operations, in selected modules." ) 
    ; Wr . Flush ( PWrT ) 
    END DisplayHelp 

; PROCEDURE DisplayVersion ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WL ( "OrdSets test driver, version 1.0" ) 
    ; Wr . Flush ( PWrT ) 
    END DisplayVersion  

; PROCEDURE ResetProgress ( ) 

  = BEGIN 
      GProgressLen := 0
    END ResetProgress 

; PROCEDURE AppendProgress ( Val : TEXT ) 

  = VAR LLen : CARDINAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      LLen := Text . Length ( Val ) 
    ; IF GProgressLen > 0 
         AND GProgressLine [ GProgressLen - 1 ] = '\010' 
      THEN 
        DEC ( GProgressLen , 2 ) 
      END (* IF *) 
    ; Text . SetChars 
        ( SUBARRAY ( GProgressLine , GProgressLen , LLen ) , Val ) 
    ; INC ( GProgressLen , LLen ) 
    ; Wr . PutText ( PWrT , Val ) 
    ; Wr . Flush ( PWrT )    
    END AppendProgress 

; PROCEDURE RedisplayProgress ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutString ( PWrT , SUBARRAY ( GProgressLine , 0 , GProgressLen ) )
    ; Wr . Flush ( PWrT )    
    END RedisplayProgress 

; CONST SpinCt = 100 

; PROCEDURE NoteProgress ( VAR Ct : INTEGER ; Incr : CARDINAL := 1 ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF Ct MOD ( 500 * SpinCt ) = 0 
      THEN (* New line. *) 
        ResetProgress ( ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; AppendProgress ( Fmt . Pad ( Fmt . Int ( Ct ) , TestCtPad ) ) 
      ELSIF Ct MOD ( 100 * SpinCt ) = 0 
      THEN 
        AppendProgress ( Fmt . Int ( ( Ct DIV ( 100 * SpinCt ) ) MOD 10 ) ) 
      ELSIF Ct MOD ( 50 * SpinCt ) = 0 
      THEN 
        AppendProgress ( "+" ) 
      ELSIF Ct MOD ( 10 * SpinCt ) = 0 
      THEN 
        AppendProgress ( "." )
      END (* IF *)
    ; CASE ( Ct DIV SpinCt ) MOD 4 <* NOWARN *>  
      OF 0 => AppendProgress ( "/\010" )
      | 1 => AppendProgress ( "-\010" )
      | 2 => AppendProgress ( "\\\010" )
      | 3 =>AppendProgress ( "|\010" )
      END (* CASE *)
    ; INC ( Ct , Incr ) 
    END NoteProgress 

; PROCEDURE ShowExactProgress ( Ct : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . PutText ( PWrT , Fmt . Pad ( Fmt . Int ( Ct ) , TestCtPad ) ) 
    ; Wr . Flush ( PWrT )    
    END ShowExactProgress

; PROCEDURE StartTimingOld ( ) 

  = BEGIN 
      GStartTimeOld := Time . Now ( ) 
    ; GStartTickOld := Tick . Now ( ) 
    END StartTimingOld 

; PROCEDURE StopTimingOld ( ) 

  = VAR LElapsedSecsTick , LElapsedSecsTime  : LONGREAL 

  ; BEGIN 
      GStopTickOld := Tick . Now ( ) 
    ; GStopTimeOld := Time . Now ( )
    ; LElapsedSecsTick := Tick . ToSeconds ( GStopTickOld - GStartTickOld ) 
    ; LElapsedSecsTime := GStopTimeOld - GStartTimeOld 
    ; GOldSetsTick := GOldSetsTick + LElapsedSecsTick 
    ; GOldSetsTime := GOldSetsTime + LElapsedSecsTime 
    ; INC ( GOldTimedCt ) 
    END StopTimingOld 

; PROCEDURE StartTimingNew ( ) 

  = BEGIN 
      GStartTimeNew := Time . Now ( ) 
    ; GStartTickNew := Tick . Now ( ) 
    END StartTimingNew 

; PROCEDURE StopTimingNew ( ) 

  = VAR LElapsedSecsTick , LElapsedSecsTime  : LONGREAL 

  ; BEGIN 
      GStopTickNew := Tick . Now ( ) 
    ; GStopTimeNew := Time . Now ( )
    ; LElapsedSecsTick := Tick . ToSeconds ( GStopTickNew - GStartTickNew ) 
    ; LElapsedSecsTime := GStopTimeNew - GStartTimeNew 
    ; GNewSetsTick := GNewSetsTick + LElapsedSecsTick 
    ; GNewSetsTime := GNewSetsTime + LElapsedSecsTime 
    ; INC ( GNewTimedCt ) 
    END StopTimingNew 

; VAR EstimateCt := 10000 

; PROCEDURE EstimateTimingOverhead ( ) 

  = BEGIN
      GOldSetsTick := 0.0D0 
    ; GOldSetsTime := 0.0D0 
    ; GOldTimedCt := 0 
    ; FOR RI := 0 TO EstimateCt 
      DO
        StartTimingOld ( ) 
      ; StopTimingOld ( ) 
      END (* FOR *) 
    ; GTimingOverheadTick := GOldSetsTick / FLOAT ( GOldTimedCt , LONGREAL ) 
    ; GTimingOverheadTime := GOldSetsTime / FLOAT ( GOldTimedCt , LONGREAL ) 
    ; GOldTimedCt := 0 
    ; GOldSetsTick := 0.0D0 
    ; GOldSetsTime := 0.0D0 
    END EstimateTimingOverhead 

; CONST MaxLine = 80 

; PROCEDURE HexAddr ( Addr : REFANY ) : TEXT 

  = VAR LAddress : INTEGER 

  ; BEGIN 
      LAddress := UnsafeUtils . IntOfRefany ( Addr ) 
    ; RETURN 
        "16_" & Fmt . Pad ( Fmt . Int ( LAddress , 16 ) , HexPad , '0' ) 
    END HexAddr

; TYPE ProcOfIntTyp = PROCEDURE ( Elem : INTEGER )  

; TYPE ProcOfProcOfIntTyp = PROCEDURE ( Proc : ProcOfIntTyp ) 

; PROCEDURE DumpSet 
    ( Label : TEXT ; Set : REFANY ; Enumerator : ProcOfProcOfIntTyp ) 

  = VAR LineLen : CARDINAL 
  ; VAR IsFirstElem : BOOLEAN := TRUE 
  ; VAR RangeLoElem , RangeHiElem : IntSets . ElemT := IntSets . NullElem 

  ; PROCEDURE FlushRange ( ) 

    = VAR Lt : TEXT 
    ; VAR LLen : CARDINAL

    ; <* FATAL Thread . Alerted , Wr . Failure *> 
      BEGIN 
        IF RangeLoElem # IntSets . NullElem 
        THEN 
          IF RangeLoElem < RangeHiElem 
          THEN 
            Lt := Fmt . Int ( RangeLoElem ) & ".." & Fmt . Int ( RangeHiElem )
          ELSE 
            Lt := Fmt . Int ( RangeHiElem )
          END (* IF *)  
        ; IF IsFirstElem 
          THEN
            IsFirstElem := FALSE  
          ELSE 
            Lt := "," & Lt 
          END (* IF *) 
        ; LLen := Text . Length ( Lt ) 
        ; IF LineLen + LLen > MaxLine 
          THEN
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , "  " ) 
          ; LineLen := 2 + LLen 
          ELSE INC ( LineLen , LLen )   
          END (* IF *) 
        ; Wr . PutText ( WrT , Lt ) 
        END (* IF *) 
      END FlushRange 

  ; PROCEDURE Visit ( Elem : INTEGER ) 

    = BEGIN 
        IF RangeLoElem = IntSets . NullElem 
        THEN (* Start a new range. *) 
          RangeLoElem := Elem 
        ; RangeHiElem := Elem 
        ELSIF Elem = RangeHiElem + 1 
        THEN (* Just extend the stored range with this element. *) 
          RangeHiElem := Elem 
        ELSE (* Print the stored range, then start a new range. *) 
          FlushRange ( ) 
        ; RangeLoElem := Elem 
        ; RangeHiElem := Elem 
        END (* IF *)  
      END Visit 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN (* DumpSet *)  
      Wr . PutText ( WrT , Label ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; LineLen := Text . Length ( Label ) + 2
    ; IF Set = NIL 
      THEN 
        Wr . PutText ( WrT , "NIL" ) 
      ELSE 
        Wr . PutText ( WrT , HexAddr ( Set ) ) 
      ; INC ( LineLen , 11 ) 
      ; Wr . PutText ( WrT , " = {" ) 
      ; INC ( LineLen , 4 ) 
      ; Enumerator ( Visit ) 
      ; FlushRange ( ) 
      ; Wr . PutText ( WrT , "}" ) 
      END (* IF *) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT )    
    END DumpSet 

; PROCEDURE DumpOldSet ( Set : Sets . T ) 

  = PROCEDURE EnumOldElems ( GeneralVisitor : ProcOfIntTyp )

    = PROCEDURE SpecVisitor ( Elem : Sets . tElement ) 

      = BEGIN 
          GeneralVisitor ( Elem )   
        END SpecVisitor  

    ; <* FATAL ANY *> 
      BEGIN 
        Sets . ForAllDo ( Set , SpecVisitor ) 
      END EnumOldElems

  ; BEGIN (* DumpOldSet *)  
      DumpSet ( "OldSet" , Set , EnumOldElems ) 
    END DumpOldSet 

; PROCEDURE DumpNewSet ( Set : IntSets . T ) 

  = PROCEDURE EnumNewElems ( GeneralVisitor : ProcOfIntTyp )

    = PROCEDURE SpecVisitor ( Elem : IntSets . ValidElemT ) 

      = BEGIN 
          GeneralVisitor ( Elem - IntBias )   
        END SpecVisitor  

    ; <* FATAL ANY *> 
      BEGIN 
        IntSets . ForAllDo ( Set , SpecVisitor ) 
      END EnumNewElems

  ; BEGIN (* DumpNewSet *)  
      DumpSet ( "IntSet" , Set , EnumNewElems ) 
    END DumpNewSet 

(* ======================================================================= *) 

; PROCEDURE WriteSetNo ( SetNo : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      IF SetNo >= 0 
      THEN 
        Wr . PutText ( WrT , ", Set no " )
      END (* IF *) 
    ; Wr . PutText ( WrT , Fmt . Int ( SetNo ) ) 
    END WriteSetNo 
  
; EXCEPTION BailEarly 

; PROCEDURE NoteFailure ( ) 

  = BEGIN 
      INC ( GFailureCt ) (* A good place for a breakpoint. *) 
    END NoteFailure 

; PROCEDURE CompareSets 
    ( OldSet : Sets . T 
    ; NewSet : IntSets . T 
    ; Context : TEXT 
    ; SetNo : INTEGER  
    ) 

  = VAR LIt : IntSets . Iterator 
  ; VAR LNewElem : INTEGER 

  ; PROCEDURE VisitOldElem ( OldElem : Sets . tElement ) 
    RAISES { BailEarly } 

    = <* FATAL Thread . Alerted , Wr . Failure *> 
      BEGIN
        LNewElem := LIt . current ( )  
      ; IF LNewElem = IntSets . NullElem 
        THEN
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteSetNo ( SetNo ) 
        ; Wr . PutText ( WrT , ": Sets unequal, extra old element: " )
        ; Wr . PutText ( WrT , Fmt . Int ( OldElem ) )
        ; Wr . PutText ( WrT , ", no new element: " )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; DumpOldSet ( OldSet ) 
        ; DumpNewSet ( NewSet ) 
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        ; RAISE BailEarly 
        ELSIF OldElem # LNewElem - IntBias 
        THEN
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteSetNo ( SetNo ) 
        ; Wr . PutText ( WrT , ": Sets unequal at old element: " )
        ; Wr . PutText ( WrT , Fmt . Int ( OldElem ) )
        ; Wr . PutText ( WrT , ", new element: " )
        ; Wr . PutText ( WrT , Fmt . Int ( LNewElem - IntBias ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; DumpOldSet ( OldSet ) 
        ; DumpNewSet ( NewSet ) 
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        ; RAISE BailEarly 
        ELSE 
          LIt . advance ( )  
        END (* IF *)   
      END VisitOldElem 
  
  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      IF Sets . IsEmpty ( OldSet ) 
      THEN IF NOT IntSets . IsEmpty ( NewSet ) 
        THEN 
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteSetNo ( SetNo ) 
        ; Wr . PutText ( WrT , ": Old set empty, new is not." )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; DumpNewSet ( NewSet ) 
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        END (* IF *)  
      ELSIF IntSets . IsEmpty ( NewSet ) 
      THEN 
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Context )
      ; WriteSetNo ( SetNo ) 
      ; Wr . PutText ( WrT , ": New set empty, old is not." )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; DumpOldSet ( OldSet ) 
      ; NoteFailure ( ) 
      ; RedisplayProgress ( ) 
      ELSE (* Both are nonempty. *) 
        LIt := IntSets . NewIterator ( NewSet ) 
      ; TRY 
          Sets . ForAllDo ( OldSet , VisitOldElem ) 
        ; LNewElem := LIt . current ( ) 
        ; IF LNewElem # IntSets . NullElem 
          THEN (* New set has extra elements. *) 
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , Context )
          ; WriteSetNo ( SetNo ) 
          ; Wr . PutText ( WrT , ": Sets unequal, extra new element: " )
          ; Wr . PutText ( WrT , Fmt . Int ( LNewElem - IntBias ) )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; DumpOldSet ( OldSet ) 
          ; DumpNewSet ( NewSet ) 
          ; NoteFailure ( ) 
          ; RedisplayProgress ( ) 
          END (* IF*) 
        EXCEPT 
        BailEarly  
        => 
        ELSE 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END CompareSets 

; PROCEDURE CompareElem 
    ( OldElem , NewElem : INTEGER ; Context : TEXT ; SetNo : INTEGER ) 

  = VAR LUnbiasedNewElem := NewElem - IntBias 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF NewElem = IntSets . NullElem 
      THEN 
        LUnbiasedNewElem := NewElem 
      ELSE 
        LUnbiasedNewElem := NewElem - IntBias 
      END (* IF *) 
    ; IF OldElem # LUnbiasedNewElem 
      THEN
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Context )
      ; WriteSetNo ( SetNo ) 
      ; Wr . PutText ( WrT , ": Element values unequal, old: " )
      ; Wr . PutText ( WrT , Fmt . Int ( OldElem ) )
      ; Wr . PutText ( WrT , ", new: " )
      ; Wr . PutText ( WrT , Fmt . Int ( LUnbiasedNewElem ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      ; NoteFailure ( ) 
      ; RedisplayProgress ( ) 
      END (* IF *) 
    END CompareElem 

; PROCEDURE CompareInt 
    ( OldInt , NewInt : INTEGER ; Context : TEXT ; SetNo : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OldInt # NewInt 
      THEN
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Context )
      ; WriteSetNo ( SetNo ) 
      ; Wr . PutText ( WrT , ": Integer values unequal, old: " )
      ; Wr . PutText ( WrT , Fmt . Int ( OldInt ) )
      ; Wr . PutText ( WrT , ", new: " )
      ; Wr . PutText ( WrT , Fmt . Int ( NewInt ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      ; NoteFailure ( ) 
      ; RedisplayProgress ( ) 
      END (* IF *) 
    END CompareInt 

; PROCEDURE CompareBool 
    ( OldBool , NewBool : BOOLEAN ; Context : TEXT 
    ; SetNo1 : INTEGER 
    ; SetNo2 : INTEGER := - 1  
    ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OldBool # NewBool 
      THEN
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Context )
      ; WriteSetNo ( SetNo1 ) 
      ; WriteSetNo ( SetNo2 ) 
      ; Wr . PutText ( WrT , ": Booleans unequal, old: " )
      ; Wr . PutText ( WrT , Fmt . Bool ( OldBool ) )
      ; Wr . PutText ( WrT , ", new: " )
      ; Wr . PutText ( WrT , Fmt . Bool ( NewBool ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      ; NoteFailure ( ) 
      ; RedisplayProgress ( ) 
      END (* IF *) 
    END CompareBool 

; PROCEDURE CheckIntSet 
    ( Set : IntSets . T ; Context : TEXT ; SetNo : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      TRY
        IntSets . VerifySet ( Set ) 
      EXCEPT 
      IntSets . BadInvariant ( EMessage )
      => Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "##############################################" )
      ; Wr . PutText ( WrT , "##############################################" )
      ; Wr . PutText ( WrT , "#######################" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Context )
      ; WriteSetNo ( SetNo ) 
      ; Wr . PutText ( WrT , ": Bad invariant: " )
      ; Wr . PutText ( WrT , EMessage )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; DumpNewSet ( Set ) 
      ; NoteFailure ( ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )
      ; RedisplayProgress ( ) 
      END (* TRY EXCEPT *) 
    END CheckIntSet 

; CONST RandomMean = 1024

; CONST DenseHalfInterval = 1024 

; CONST OldSetSize = DenseHalfInterval * 2 

; PROCEDURE DenseI ( Mean : INTEGER ) : INTEGER 

  = BEGIN 
      RETURN 
        RandV . integer 
          ( Mean - DenseHalfInterval , Mean + DenseHalfInterval - 1 )   
(* FIXME: Make this more like a Gaussian.  Well, definitely not when
          choosing which operation to do. *) 
    END DenseI 

; PROCEDURE FillBase ( N : CARDINAL )

  = VAR LSs : INTEGER 
  ; VAR LElem1 , LElem2 : INTEGER 
  ; VAR LSet : Sets . T 
  ; VAR LIntSet : IntSets . T 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      ResetProgress ( ) 
    ; GFillCt := 0 
    ; IF GDoOld 
      THEN 
        Sets . MakeSet ( GOldSets [ 0 ] , OldSetSize )  
      ; Sets . AssignEmpty ( GOldSets [ 0 ] )  
      END (* IF *) 
    ; GNewSets [ 0 ] := NIL 
    ; GStoredSetCt := 1 
    ; FOR RI := 1 TO N 
      DO 
        IF GStoredSetCt >= MaxSets 
        THEN LSs := RandV . integer ( 1 , MaxSets )   
        ELSE
          LSs := GStoredSetCt 
        ; INC ( GStoredSetCt ) 
        END (* IF *)
      ; IF GTotalSetCt = GSetNoToStopOn 
        THEN 
          WL ( "Stopping on requested set in FillBase" ) 
        ; Wr . Flush ( PWrT ) 
        ; NoteFailure ( ) (* Not a true failure, but just for debugging. *)  
        END (* IF *) 
      ; IF RandV . boolean ( ) 
        THEN
          LElem1 := DenseI ( RandomMean ) 
        ; IF GDoOld 
          THEN 
            StartTimingOld ( ) 
          ; Sets . MakeSet ( LSet , OldSetSize ) 
          ; Sets . Include ( LSet , LElem1 ) 
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            StartTimingNew ( ) 
          ; LIntSet := IntSets . Singleton ( LElem1 + IntBias )
          ; StopTimingNew ( ) 
          ; CheckIntSet ( LIntSet , "Fill-singleton" , GTotalSetCt ) 
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareSets ( LSet , LIntSet , "FillSingleton" , GTotalSetCt )  
          END (* IF *) 
        ELSE
          LElem1 := DenseI ( RandomMean ) 
        ; LElem2 := DenseI ( RandomMean ) 
        ; IF GDoOld 
          THEN 
            StartTimingOld ( ) 
          ; Sets . MakeSet ( LSet , OldSetSize ) 
          ; FOR RElem := LElem1 TO LElem2 
            DO 
              Sets . Include ( LSet , RElem ) 
            END (* FOR *) 
          ; StopTimingOld ( ) 
          END (*( IF *) 
        ; IF GDoNew 
          THEN 
            StartTimingNew ( ) 
          ; LIntSet := IntSets . Range ( LElem1 + IntBias , LElem2 + IntBias )
          ; StopTimingNew ( ) 
          ; CheckIntSet ( LIntSet , "Fill-range" , GTotalSetCt ) 
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareSets ( LSet , LIntSet , "FillRange" , GTotalSetCt )  
          END (* IF *) 
        END (* IF *)
      ; GOldSets [ LSs ] := LSet
      ; GNewSets [ LSs ] := LIntSet   
      ; GSetNos [ LSs ] := GTotalSetCt 
      ; INC ( GTotalSetCt ) 
      ; NoteProgress ( (* VAR *) GFillCt ) 
      END (* FOR *)  
    ; ShowExactProgress ( GFillCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END FillBase 

; PROCEDURE MoreSets ( N : CARDINAL )  

  = VAR LResultSs , LOpndSs : CARDINAL 
  ; VAR LOldOp , LOldResult , LOldCopy : Sets . T 
  ; VAR LNewOp , LNewResult , LNewCopy : IntSets . T 
  ; VAR LOpCtToStore : CARDINAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      ResetProgress ( ) 
    ; GOperationCt := 0 
    ; FOR RI := 1 TO N 
      DO
        IF GStoredSetCt >= MaxSets 
        THEN LResultSs := RandV . integer ( 0 , MaxSets )   
        ELSE
          LResultSs := GStoredSetCt 
        ; INC ( GStoredSetCt ) 
        END (* IF *)
      ; LOpndSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
      ; LOldOp := GOldSets [ LOpndSs ] 
      ; LNewOp := GNewSets [ LOpndSs ] 
      ; LOldResult := NIL 
      ; LNewResult := NIL
      ; IF GDoOld 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp ) 
        ; StopTimingOld ( ) 
        END (* IF *) 
      ; LNewResult := LNewOp 
      ; IF GDoCompareOperands 
        THEN 
          CompareSets ( LOldOp , LNewOp , "More-op1" , GSetNos [ RI ] )  
        END (* IF *) 
      ; IF GDoCompareResults
        THEN 
          CompareSets 
            ( LOldResult , LNewResult , "MoreSets-1" , GSetNos [ LOpndSs ] ) 
        END (* IF *) 
      ; LOpCtToStore := RandV . integer ( 1 , 7 ) 
      ; FOR RJ := 1 TO LOpCtToStore 
        DO
          LOpndSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
        ; LOldOp := GOldSets [ LOpndSs ] 
        ; LNewOp := GNewSets [ LOpndSs ]
        ; IF GDoOld 
          THEN 
            StartTimingOld ( ) 
          ; Sets . Copy ( LOldCopy , LOldResult )  
          ; Sets . Union ( LOldResult , LOldOp ) 
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            LNewCopy := LNewResult
          ; StartTimingNew ( )  
          ; LNewResult := IntSets . Union ( LNewCopy , LNewOp ) 
          ; StopTimingNew ( ) 
          ; CheckIntSet ( LNewResult , "MoreSets" , GTotalSetCt ) 
          END (* IF *) 
        ; IF GDoCompareOperands 
          THEN 
            CompareSets ( LOldOp , LNewOp , "More-opn" , GSetNos [ LOpndSs ] )
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareSets 
              ( LOldResult , LNewResult , "MoreSets-n" , GSetNos [ LOpndSs ] ) 
          END (* IF *) 
        ; INC ( GTotalSetCt ) 
        ; NoteProgress ( (* VAR *) GOperationCt ) 
        END (* FOR *) 
      ; IF GTotalSetCt = GSetNoToStopOn 
        THEN (* Perhaps a little late, but harder to code otherwise. *)  
          WL ( "Stopping on requested set in MoreSets" ) 
        ; Wr . Flush ( PWrT ) 
        ; NoteFailure ( ) (* Not a true failure, but just for debugging. *)  
        END (* IF *) 
      ; GOldSets [ LResultSs ] := LOldResult
      ; GNewSets [ LResultSs ] := LNewResult
      ; GSetNos [ LResultSs ] := GTotalSetCt 
      END (* FOR *) 
    ; ShowExactProgress ( GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END MoreSets  

; PROCEDURE DoUnion ( Ss : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldResult : Sets . T 
  ; VAR LNewOp1 , LNewOp2 , LNewResult : IntSets . T 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp1 )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp1 )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Union ( LOldResult , LOldOp2 )   
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Union ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Union" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp1 , LNewOp1 , "Union-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets ( LOldOp2 , LNewOp2 , "Union-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Union" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoUnion 

; PROCEDURE DoIntersection ( Ss : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldResult : Sets . T 
  ; VAR LNewOp1 , LNewOp2 , LNewResult : IntSets . T 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; LOldResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp1 )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp1 )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Intersection ( LOldResult , LOldOp2 )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Intersection ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Intersection" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets 
          ( LOldOp1 , LNewOp1 , "Intersection-op1" , GSetNos [ LSs1 ] )
      ; CompareSets 
          ( LOldOp2 , LNewOp2 , "Intersection-op2" , GSetNos [ LSs2 ] )
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Intersection" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoIntersection 

; PROCEDURE DoDifference ( Ss : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldResult : Sets . T 
  ; VAR LNewOp1 , LNewOp2 , LNewResult : IntSets . T 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp1 )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp1 )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Difference ( LOldResult , LOldOp2 )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Difference ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Difference" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets 
          ( LOldOp1 , LNewOp1 , "Difference-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets 
          ( LOldOp2 , LNewOp2 , "Difference-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Difference" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoDifference 

; PROCEDURE DoSymDiff ( Ss : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldResult : Sets . T 
  ; VAR LNewOp1 , LNewOp2 , LNewResult : IntSets . T 

  ; BEGIN 
      IF GDoSymDiff 
      THEN 
        LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
      ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
      ; LOldOp1 := GOldSets [ LSs1 ] 
      ; LOldOp2 := GOldSets [ LSs2 ] 
      ; LNewOp1 := GNewSets [ LSs1 ] 
      ; LNewOp2 := GNewSets [ LSs2 ] 
      ; LOldResult := NIL 
      ; LNewResult := NIL 
      ; IF GDoOld 
        THEN 
          IF GTimeOldCopies 
          THEN 
            StartTimingOld ( ) 
          ; Sets . Copy ( LOldResult , LOldOp1 )   
          ; StopTimingOld ( ) 
          ELSE 
            Sets . Copy ( LOldResult , LOldOp1 )   
          END (* IF *) 
        ; StartTimingOld ( ) 
        ; Sets . SymDiff ( LOldResult , LOldOp2 ) 
        ; StopTimingOld ( ) 
        END (* IF *) 
      ; IF GDoNew 
        THEN 
          StartTimingNew ( ) 
        ; LNewResult := IntSets . SymDiff ( LNewOp1 , LNewOp2 )  
        ; StopTimingNew ( ) 
        ; CheckIntSet ( LNewResult , "SymDiff" , GTotalSetCt ) 
        END (* IF *) 
      ; IF GDoCompareOperands 
        THEN 
          CompareSets ( LOldOp1 , LNewOp1 , "SymDiff-op1" , GSetNos [ LSs1 ] ) 
        ; CompareSets ( LOldOp2 , LNewOp2 , "SymDiff-op2" , GSetNos [ LSs2 ] ) 
        END (* IF *) 
      ; IF GDoCompareResults 
        THEN 
          CompareSets ( LOldResult , LNewResult , "SymDiff" , GTotalSetCt )  
        END (* IF *) 
      ; GOldSets [ Ss ] := LOldResult
      ; GNewSets [ Ss ] := LNewResult
      END (* IF *)
    END DoSymDiff 

; PROCEDURE DoProject ( Ss : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp , LOldResult : Sets . T 
  ; VAR LNewOp , LNewResult : IntSets . T 
  ; VAR LLo , LHi , LTemp : INTEGER 

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; LLo := DenseI ( RandomMean ) 
    ; LHi := DenseI ( RandomMean ) 
    ; IF LLo > LHi AND RandV . integer ( 1 , 9 ) # 5  
      THEN
        LTemp := LLo
      ; LLo := LHi 
      ; LHi := LTemp 
      END (* IF *) 
    ; LOldResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Project ( LOldResult , LLo , LHi )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult 
          := IntSets . Project ( LNewOp , LLo + IntBias , LHi + IntBias )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Project" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Project-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Project" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoProject 

; PROCEDURE DoInclude ( Ss : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp , LOldResult : Sets . T 
  ; VAR LNewOp , LNewResult : IntSets . T 
  ; VAR LElem : INTEGER 

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; LElem := DenseI ( RandomMean ) 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Include ( LOldResult , LElem )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Include ( LNewOp , LElem + IntBias )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Include" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Include-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Include" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoInclude 

; PROCEDURE DoExclude ( Ss : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp , LOldResult : Sets . T 
  ; VAR LNewOp , LNewResult : IntSets . T 
  ; VAR LElem : INTEGER 

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; LElem := DenseI ( RandomMean ) 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; Sets . Exclude ( LOldResult , LElem )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Exclude ( LNewOp , LElem + IntBias )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Exclude" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Exclude-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Exclude" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoExclude 

; PROCEDURE DoExtract ( Ss : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp , LOldResult : Sets . T 
  ; VAR LNewOp , LNewResult : IntSets . T 
  ; VAR LOldElem , LNewElem : INTEGER 

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; LOldResult := NIL 
    ; LNewResult := NIL 
    ; IF GDoOld 
      THEN 
        IF GTimeOldCopies 
        THEN 
          StartTimingOld ( ) 
        ; Sets . Copy ( LOldResult , LOldOp )   
        ; StopTimingOld ( ) 
        ELSE 
          Sets . Copy ( LOldResult , LOldOp )   
        END (* IF *) 
      ; StartTimingOld ( ) 
      ; IF Sets . IsEmpty ( LOldOp ) 
        THEN LOldElem := IntSets . NullElem  
        ELSE LOldElem := Sets . Extract ( LOldResult )  
        END (* IF *) 
      ; StopTimingOld ( ) 
      ; LNewResult := LNewOp 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewElem := IntSets . ExtractArbitraryMember ( LNewResult )  
      ; StopTimingNew ( ) 
      ; CheckIntSet ( LNewResult , "Extract" , GTotalSetCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Extract-opnd" , GSetNos [ LSs ] ) 
      ; CompareElem ( LOldElem , LNewElem , "Extract" , GSetNos [ LSs ] )
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareSets ( LOldResult , LNewResult , "Extract" , GTotalSetCt ) 
      END (* IF *) 
    ; GOldSets [ Ss ] := LOldResult
    ; GNewSets [ Ss ] := LNewResult
    END DoExtract 

; PROCEDURE Operations ( N : CARDINAL ) 

  = VAR LN : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      ResetProgress ( ) 
    ; GOperationCt := 0 
    ; FOR RI := 1 TO N 
      DO
        IF GStoredSetCt >= MaxSets 
        THEN 
          LN := RandV . integer ( 0 , MaxSets )   
        ELSE
          LN := GStoredSetCt 
        END (* IF *)
      ; IF GTotalSetCt = GSetNoToStopOn 
        THEN 
          WL ( "Stopping on requested set in Operations" ) 
        ; Wr . Flush ( PWrT ) 
        ; NoteFailure ( ) (* Not a true failure, but just for debugging. *)  
        END (* IF *) 
      ; CASE RandV . integer ( 0 , 7 ) <* NOWARN *> 
        OF 0 => DoUnion ( LN ) 
        | 1 => DoIntersection ( LN )  
        | 2 => DoProject ( LN )  
        | 3 => DoDifference ( LN )  
        | 4 => DoSymDiff ( LN )  
        | 5 => DoInclude ( LN )  
        | 6 => DoExclude ( LN )  
        | 7 => DoExtract ( LN )   
        END (* CASE *) 
      ; GSetNos [ LN ] := GTotalSetCt 
      ; INC ( GTotalSetCt ) 
      ; IF GStoredSetCt < MaxSets 
        THEN 
          INC ( GStoredSetCt ) 
        END (* IF *)
      ; NoteProgress ( (* VAR *) GOperationCt ) 
      END (* FOR *) 
    ; ShowExactProgress ( GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END Operations    

; PROCEDURE DoIsEmpty ( ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : Sets . T 
  ; VAR LNewOp : IntSets . T 
  ; VAR LOldResult , LNewResult : BOOLEAN  

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . IsEmpty ( LOldOp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . IsEmpty ( LNewOp )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "IsEmpty-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool ( LOldResult , LNewResult , "IsEmpty" , GSetNos [ LSs ] )
      END (* IF *) 
    END DoIsEmpty

; PROCEDURE DoMinimum ( ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : Sets . T 
  ; VAR LNewOp : IntSets . T 
  ; VAR LOldResult , LNewResult : INTEGER  

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( )
      ; IF Sets . IsEmpty ( LOldOp ) 
        THEN LOldResult := IntSets . NullElem  
        ELSE LOldResult := Sets . Minimum ( LOldOp ) 
        END (* IF *) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Minimum ( LNewOp )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Minimum-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareElem ( LOldResult , LNewResult , "Minimum" , GSetNos [ LSs ] )
      END (* IF *) 
    END DoMinimum

; PROCEDURE DoMaximum ( ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : Sets . T 
  ; VAR LNewOp : IntSets . T 
  ; VAR LOldResult , LNewResult : INTEGER  

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; IF Sets . IsEmpty ( LOldOp ) 
        THEN LOldResult := IntSets . NullElem  
        ELSE LOldResult := Sets . Maximum ( LOldOp ) 
        END (* IF *) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Maximum ( LNewOp )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Maximum-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareElem ( LOldResult , LNewResult , "Maximum" , GSetNos [ LSs ] )
      END (* IF *) 
    END DoMaximum

; PROCEDURE DoCard ( ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : Sets . T 
  ; VAR LNewOp : IntSets . T 
  ; VAR LOldResult , LNewResult : INTEGER  

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . Card ( LOldOp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Card ( LNewOp )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "Card-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareInt ( LOldResult , LNewResult , "Card" , GSetNos [ LSs ] )
      END (* IF *) 
    END DoCard

; PROCEDURE DoIsSubset ( ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 : Sets . T 
  ; VAR LNewOp1 , LNewOp2 : IntSets . T 
  ; VAR LOldResult , LNewResult : BOOLEAN 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . IsSubset ( LOldOp1 , LOldOp2 )   
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . IsSubset ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp1 , LNewOp1 , "IsSubset-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets ( LOldOp2 , LNewOp2 , "IsSubset-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool 
          ( LOldResult , LNewResult , "IsSubset" 
          , GSetNos [ LSs1 ] , GSetNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoIsSubset

; PROCEDURE DoIsProperSubset ( ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 : Sets . T 
  ; VAR LNewOp1 , LNewOp2 : IntSets . T 
  ; VAR LOldResult , LNewResult : BOOLEAN 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . IsProperSubset ( LOldOp1 , LOldOp2 )   
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . IsProperSubset ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets 
          ( LOldOp1 , LNewOp1 , "IsProperSubset-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets 
          ( LOldOp2 , LNewOp2 , "IsProperSubset-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool 
          ( LOldResult , LNewResult , "IsProperSubset" 
          , GSetNos [ LSs1 ] , GSetNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoIsProperSubset

; PROCEDURE DoIsEqual ( ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 : Sets . T 
  ; VAR LNewOp1 , LNewOp2 : IntSets . T 
  ; VAR LOldResult , LNewResult : BOOLEAN 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . IsEqual ( LOldOp1 , LOldOp2 ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Equal ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp1 , LNewOp1 , "IsEqual-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets ( LOldOp2 , LNewOp2 , "IsEqual-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool 
          ( LOldResult , LNewResult , "IsEqual" 
          , GSetNos [ LSs1 ] , GSetNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoIsEqual

; PROCEDURE DoDisjoint ( ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldTemp : Sets . T 
  ; VAR LNewOp1 , LNewOp2 : IntSets . T 
  ; VAR LOldResult , LNewResult : BOOLEAN 

  ; BEGIN 
      LSs1  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LSs2  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp1 := GOldSets [ LSs1 ] 
    ; LOldOp2 := GOldSets [ LSs2 ] 
    ; LNewOp1 := GNewSets [ LSs1 ] 
    ; LNewOp2 := GNewSets [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; Sets . MakeSet ( LOldTemp , OldSetSize ) 
      ; Sets . Copy ( (* VAR *) LOldTemp , LOldOp1 ) 
      ; Sets . Intersection ( (* VAR *) LOldTemp , LOldOp2 ) 
      ; LOldResult := Sets . IsEmpty ( LOldTemp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . Disjoint ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp1 , LNewOp1 , "Disjoint-op1" , GSetNos [ LSs1 ] )  
      ; CompareSets ( LOldOp2 , LNewOp2 , "Disjoint-op2" , GSetNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool 
          ( LOldResult , LNewResult , "Disjoint" 
          , GSetNos [ LSs1 ] , GSetNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoDisjoint

; PROCEDURE DoIsElement ( ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : Sets . T 
  ; VAR LNewOp : IntSets . T 
  ; VAR LElem : INTEGER 
  ; VAR LOldResult , LNewResult : BOOLEAN   

  ; BEGIN 
      LSs  := RandV . integer ( 0 , GStoredSetCt - 1 )   
    ; LOldOp := GOldSets [ LSs ] 
    ; LNewOp := GNewSets [ LSs ] 
    ; LElem := DenseI ( RandomMean ) 
    ; IF GDoOld 
      THEN 
        StartTimingOld ( ) 
      ; LOldResult := Sets . IsElement ( LElem , LOldOp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        StartTimingNew ( ) 
      ; LNewResult := IntSets . IsElement ( LElem + IntBias , LNewOp )  
      ; StopTimingNew ( ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareSets ( LOldOp , LNewOp , "IsElement-op" , GSetNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool ( LOldResult , LNewResult , "IsElement" , GSetNos [ LSs ] )
      END (* IF *) 
    END DoIsElement

; PROCEDURE R ( Lo , Hi : INTEGER ) : IntSets . T 
  (* Compactly callable constructor for a range set. *) 

  = BEGIN 
      RETURN IntSets . Range ( Lo , Hi ) 
    END R 

; PROCEDURE B ( X , Y , Z , W , H , Q : INTEGER := IntSets . NullElem ) 
  : IntSets . T 
  (* Compactly callable constructor for abit set. *) 

  = VAR A : ARRAY [ 0 .. 5 ] OF INTEGER 

  ; BEGIN 
      A := ARRAY [ 0 .. 5 ] OF INTEGER { X , Y , Z , W , H , Q } 
    ; RETURN IntSets . FromArray ( A ) 
    END B 

; PROCEDURE DoCompare 
    ( Left , Right : IntSets . T ; Expected : [ - 1 .. 1 ] ) 

  = VAR LResult : [ - 1 .. 1 ] 

  ; BEGIN 
      LResult := IntSets . Compare ( Left , Right ) 
    ; IF GDoCompareResults 
      THEN 
        CompareInt ( Expected , LResult , "Compare" , 0 )
(* FIXME: CompareInt really doesn't display quite the right info. *) 
      END (* IF *) 
    ; INC ( GCompareCt )
    ; INC ( GTotalCt )
    END DoCompare 

; PROCEDURE CompareTests ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF GDoCompareTests 
      THEN 
        Wr . PutText ( PWrT , "Tests on Compare." ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; GCompareCt := 0 

      ; DoCompare ( IntSets . Empty ( ) , R ( 5 , 10 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 10 ) , IntSets . Empty ( ) , 1 ) 
      ; DoCompare ( IntSets . Empty ( ) , IntSets . Empty ( ) , 0 ) 

      ; DoCompare ( R ( 4 , 10 ) , R ( 5 , 10 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 10 ) , R ( 4 , 10 ) , 1 ) 
      ; DoCompare ( R ( 4 , 10 ) , R ( 4 , 11 ) , - 1 ) 
      ; DoCompare ( R ( 4 , 11 ) , R ( 4 , 10 ) , 1 ) 
      ; DoCompare ( R ( 4 , 10 ) , R ( 4 , 10 ) , 0 ) 

      ; DoCompare ( B ( 4 , 10 ) , B ( 5 , 10 ) , - 1 ) 
      ; DoCompare ( B ( 5 , 10 ) , B ( 4 , 10 ) , 1 ) 

      ; DoCompare ( R ( 4 , 10 ) , B ( 5 , 10 ) , -1 ) 
      ; DoCompare ( B ( 5 , 10 ) , R ( 4 , 10 ) , 1 ) 
      ; DoCompare ( R ( 5 , 10 ) , B ( 3 , 10 ) , 1 ) 
      ; DoCompare ( B ( 3 , 10 ) , R ( 5 , 10 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 9 ) , B ( 5 , 10 ) , 1 ) 
      ; DoCompare ( B ( 5 , 10 ) , R ( 5 , 9 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 10 ) , B ( 5 , 10 ) , 1 ) 
      ; DoCompare ( B ( 5 , 10 ) , R ( 5 , 10 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 8 ) , B ( 5 , 10 ) , 1 ) 
      ; DoCompare ( B ( 5 , 10 ) , R ( 5 , 8 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 8 ) , B ( 5 , 6 , 7 , 10 ) , 1 ) 
      ; DoCompare ( B ( 5 , 6 , 7 , 10 ) , R ( 5 , 8 ) , - 1 ) 
      ; DoCompare ( R ( 5 , 8 ) , B ( 5 , 6 , 7 , 8 , 10 ) , - 1 ) 
      ; DoCompare ( B ( 5 , 6 , 7 , 8 , 10 ) , R ( 5 , 8 ) , 1 ) 

      ; DoCompare ( B ( 16 , 135 ) , B ( 16 , 135 ) , 0 ) 
      ; DoCompare ( B ( 16 , 135 ) , B ( 16 , 195 ) , 1 ) 
      ; DoCompare ( B ( 16 , 195 ) , B ( 16 , 135 ) , - 1 ) 
      ; DoCompare ( B ( 16 , 65 , 135 ) , B ( 16 , 66 , 195 ) , 1 ) 
      ; DoCompare ( B ( 16 , 66 , 195 ) , B ( 16 , 65 , 135 ) , - 1 ) 
      ; DoCompare ( B ( 16 , 66 , 135 ) , B ( 16 , 65 , 195 ) , - 1 ) 
      ; DoCompare ( B ( 16 , 65 , 195 ) , B ( 16 , 66 , 135 ) , 1 ) 

      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . PutText ( PWrT , "Completed " ) 
      ; Wr . PutText ( PWrT , Fmt . Int ( GCompareCt ) ) 
      ; Wr . PutText ( PWrT , " tests on Compare." ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      END (* IF *) 
    END CompareTests 

; PROCEDURE IntImage ( I : IntSets . ValidElemT ) : TEXT 
  (* Wrapper to match params. *) 

  = BEGIN 
      RETURN Fmt . Int ( I ) 
    END IntImage 

; PROCEDURE TestImage 
   ( S : IntSets . T 
   ; Expected : TEXT 
   ; Prefix : TEXT := "" 
   ; LineLen : CARDINAL := 80 
   ) 

  = VAR LImage : TEXT 

  ; BEGIN 
      LImage := IntSets . Image ( S , IntImage , Prefix , LineLen ) 
    ; INC ( GImageCt ) 
    ; IF Text . Equal ( LImage , Expected ) 
      THEN 
        Wr . PutText ( PWrT , "Image as expected: \"" ) 
      ; Wr . PutText ( PWrT , LImage ) 
      ; Wr . PutText ( PWrT , "\"" ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      ELSE 
        INC ( GFailureCt ) 
      ; Wr . PutText ( PWrT , "Expected Image: \"" ) 
      ; Wr . PutText ( PWrT , Expected ) 
      ; Wr . PutText ( PWrT , "\"" ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . PutText ( PWrT , "  Actual Image: \"" ) 
      ; Wr . PutText ( PWrT , LImage ) 
      ; Wr . PutText ( PWrT , "\"" ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      END (* IF *) 
    END TestImage 

; TYPE AOI = ARRAY OF INTEGER 

; CONST Last = LAST ( IntSets . ValidElemT ) 
; CONST First = FIRST ( IntSets . ValidElemT ) 

; PROCEDURE DoImages ( ) 

  = VAR LImageFailureCt : INTEGER := 0 

  ; BEGIN 
      Wr . PutText ( PWrT , "Tests on Image function" ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; LImageFailureCt := - GFailureCt 
    ; TestImage 
        ( IntSets . Empty ( ) 
        , "{}"
        ) 
    ; TestImage 
        ( IntSets . Singleton ( 5 ) 
        , "{5}"
        ) 
    ; TestImage 
        ( IntSets . Range ( 3 , 4 ) 
        , "{3,4}"
        ) 
    ; TestImage 
        ( IntSets . Range ( 3 , 8 ) 
        , "{3..8}"
        ) 
    ; TestImage 
        ( IntSets . FromArray ( AOI { 1 , 3 , 5 , 6 , 8 , 9 , 10 , 12 } ) 
        , "{1,3,5,6,8..10,12}"
        ) 
(* TODO: To test cases against the bottom of the INTEGER range, we would need
         a different instantiation of OrdSets with ValidElemT at the bottom. *)
    ; IF LAST ( INTEGER ) = 16_7FFFFFFF
      THEN (* 32-bit host. *) 
        TestImage 
          ( IntSets . Singleton ( First ) 
          , "{-2147483647}"
          ) 
      ; TestImage 
          ( IntSets . Range ( First , First + 1 ) 
          , "{-2147483647,-2147483646}"
          ) 
      ; TestImage 
          ( IntSets . Range ( First , First + 2 ) 
          , "{-2147483647..-2147483645}"
          ) 
      ; TestImage 
          ( IntSets . Singleton ( Last ) 
          , "{2147483647}"
          ) 
      ; TestImage 
          ( IntSets . Range ( Last - 1 , Last ) 
          , "{2147483646,2147483647}"
          ) 
      ; TestImage 
          ( IntSets . Range ( Last - 2 , Last ) 
          , "{2147483645..2147483647}"
          ) 
      ELSE (* 64-bit host *) 
        TestImage 
          ( IntSets . Singleton ( First ) 
          , "{-9223372036854775807}"
          ) 
      ; TestImage 
          ( IntSets . Range ( First , First + 1 ) 
          , "{-9223372036854775807,-9223372036854775806}"
          ) 
      ; TestImage 
          ( IntSets . Range ( First , First + 2 ) 
          , "{-9223372036854775807..-9223372036854775805}"
          ) 
      ; TestImage 
          ( IntSets . Singleton ( Last ) 
          , "{9223372036854775807}"
          ) 
      ; TestImage 
          ( IntSets . Range ( Last - 1 , Last ) 
          , "{9223372036854775806,9223372036854775807}"
          ) 
      ; TestImage 
          ( IntSets . Range ( Last - 2 , Last ) 
          , "{9223372036854775805..9223372036854775807}"
          ) 
      END (* IF *) 

    ; INC ( LImageFailureCt , GFailureCt ) 
    ; Wr . PutText ( PWrT , "Completed " ) 
    ; Wr . PutText ( PWrT , Fmt . Int ( GImageCt ) )  
    ; Wr . PutText ( PWrT , " tests on Image function, with " ) 
    ; Wr . PutText ( PWrT , Fmt . Int ( LImageFailureCt ) )  
    ; Wr . PutText ( PWrT , " failures." ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    END DoImages 

; PROCEDURE Predicates ( N : CARDINAL ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      ResetProgress ( ) 
    ; GPredCt := 0 
    ; FOR RI := 1 TO N 
      DO
        CASE RandV . integer ( 0 , 8 ) <* NOWARN *> 
        OF 0 => DoIsEmpty ( ) 
        | 1 => DoMinimum ( )  
        | 2 => DoMaximum ( )  
        | 3 => DoCard ( )  
        | 4 => DoIsSubset ( )  
        | 5 => DoIsProperSubset ( )  
        | 6 => DoIsEqual ( )  
        | 7 => DoIsElement ( )   
        | 8 => DoDisjoint ( )   
        END (* CASE *) 
      ; NoteProgress ( (* VAR *) GPredCt ) 
      END (* FOR *) 
    ; ShowExactProgress ( GPredCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END Predicates    

; PROCEDURE Work ( ) 

  = VAR LPklWr : Wr . T 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WrT := Stdio . stdout 
    ; PWrT := Stdio . stderr 

    ; GetParams ( ) 
    ; IF GDoDisplayHelp 
      THEN 
        DisplayVersion ( ) 
      ; DisplayHelp ( ) 
      ELSIF GDoDisplayVersion 
      THEN 
        DisplayVersion ( ) 
      ELSIF NOT ( GDoImageTests OR GDoRandomTests OR GDoCompareTests ) 
      THEN  
        WL ( "No Tests specified." ) 
      ; Wr . Flush ( PWrT ) 
      ELSE 
        IF GDoImageTests 
        THEN 
          DoImages ( ) 
        END (* IF *) 
      ; CompareTests ( ) 
      ; IF NOT ( GDoOld AND GDoNew )
        THEN 
          GDoCompareOperands := FALSE 
        ; GDoCompareResults := FALSE 
        END (*  IF *)  
      ; IF GDoRandomTests AND ( GDoOld OR GDoNew ) 
        THEN 
          Wr . PutText ( PWrT , "Estimating overhead of timing." ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . Flush ( PWrT ) 
        ; EstimateTimingOverhead ( ) 

        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . PutText ( PWrT , "One tick = " ) 
        ; Wr . PutText ( PWrT , Fmt . Int ( SpinCt ) ) 
        ; Wr . PutText ( PWrT , " operations. " ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 

        ; IF WrT # PWrT 
          THEN 
            Wr . PutText 
              ( WrT , "Generating initial singleton and range sets." )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          END (* IF *) 
        ; Wr . PutText 
            ( PWrT , "Generating initial singleton and range sets." ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . Flush ( PWrT ) 
        ; FillBase ( 50000 ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 

        ; IF WrT # PWrT 
          THEN 
            Wr . PutText ( WrT , "Generating additional operand sets." ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          END (* IF *) 
        ; Wr . PutText ( PWrT , "Generating additional operand sets." ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . Flush ( PWrT ) 
        ; MoreSets ( 100000 ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 

        ; IF WrT # PWrT 
          THEN 
            Wr . PutText ( WrT , "Operations that produce new sets." ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          END (* IF *) 
        ; Wr . PutText ( PWrT , "Operations that produce new sets." ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . Flush ( PWrT ) 
        ; Operations ( 500000 ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; INC ( GTotalCt , GOperationCt ) 

        ; IF GDoWritePickle 
          THEN 
            LPklWr := FileWr .  Open ( PickleFileName ) 
          ; FOR RI := 0 TO GStoredSetCt - 1 
            DO 
              Pickle2 . Write ( LPklWr , GNewSets [ RI ] )
            END (* FOR *) 
          ; Wr . Flush ( LPklWr ) 
          ; Wr . Close ( LPklWr ) 
          ; Wr . PutText ( WrT , "Wrote pickle file \"" ) 
          ; Wr . PutText ( WrT , PickleFileName ) 
          ; Wr . PutText ( WrT , "\", containing " ) 
          ; Wr . PutText ( WrT , Fmt . Int ( GStoredSetCt ) ) 
          ; Wr . PutText ( WrT , " sets." ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          END (* IF *) 

        ; IF WrT # PWrT 
          THEN 
            Wr . PutText ( WrT , "Predicates on sets." ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          END (* IF *) 
        ; Wr . PutText ( PWrT , "Predicates on sets." ) 
        ; Wr . PutText ( PWrT , Wr . EOL ) 
        ; Wr . Flush ( PWrT ) 
        ; Predicates ( 500000 ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; INC ( GTotalCt , GPredCt ) 

        ; Wr . PutText ( WrT , "Total tests failed:   " ) 
        ; Wr . PutText 
            ( WrT , Fmt . Pad ( Fmt . Int ( GFailureCt ) , TestCtPad ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , "Total tests executed: " ) 
        ; Wr . PutText 
            ( WrT , Fmt . Pad ( Fmt . Int ( GTotalCt ) , TestCtPad ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 

        ; Wr . PutText 
            ( WrT , "Estimated seconds of instrumentation overhead per call: " )
        ; Wr . PutText ( WrT , Fmt . LongReal ( GTimingOverheadTick ) ) 
        ; Wr . PutText ( WrT , ", using Time: " ) 
        ; Wr . PutText ( WrT , Fmt . LongReal ( GTimingOverheadTime ) ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 

        ; Wr . PutText ( WrT , "Seconds spent in old Sets module:    " ) 
        ; GOldSetsTick 
            := GOldSetsTick 
               - FLOAT ( GOldTimedCt , LONGREAL ) * GTimingOverheadTick 
        ; GOldSetsTime 
            := GOldSetsTime 
               - FLOAT ( GOldTimedCt , LONGREAL ) * GTimingOverheadTime 
        ; Wr . PutText ( WrT , Fmt . LongReal ( GOldSetsTick ) ) 
        ; Wr . PutText ( WrT , ", using Time: " ) 
        ; Wr . PutText ( WrT , Fmt . LongReal ( GOldSetsTime ) ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 

        ; Wr . PutText ( WrT , "Seconds spent in new IntSets module: " ) 
        ; GNewSetsTick 
            := GNewSetsTick 
               - FLOAT ( GNewTimedCt , LONGREAL ) * GTimingOverheadTick 
        ; GNewSetsTime 
            := GNewSetsTime 
               - FLOAT ( GNewTimedCt , LONGREAL ) * GTimingOverheadTime 
        ; Wr . PutText ( WrT , Fmt . LongReal ( GNewSetsTick ) ) 
        ; Wr . PutText ( WrT , ", using Time: " ) 
        ; Wr . PutText ( WrT , Fmt . LongReal ( GNewSetsTime ) ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT ) 
        END (* IF *) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      END (* IF *) 
    END Work  

; BEGIN 
    Work ( ) 
  END Test 
. 
