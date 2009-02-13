MODULE Test EXPORTS Main 

; IMPORT FloatMode 
; IMPORT Fmt
; IMPORT Lex 
; IMPORT Params 
; IMPORT Random 
; IMPORT Rd
; IMPORT RTCollector 
; IMPORT RTCollectorSRC 
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextClass 
; IMPORT TextRd 
; IMPORT Tick
; IMPORT Time
; IMPORT Thread
; IMPORT Word  
; IMPORT Wr 

; IMPORT UnsafeUtils 
; IMPORT TextUtils

; VAR WrT : Wr . T 
; VAR PWrT : Wr . T 

; CONST MaxTexts = 100000
; VAR GOldTexts := ARRAY [ 0 .. MaxTexts ] OF TEXT { NIL , .. }
; VAR GNewTexts := ARRAY [ 0 .. MaxTexts ] OF TEXT { NIL , .. }
; VAR GTextNos := ARRAY [ 0 .. MaxTexts ] OF INTEGER { - 1 , .. } 
; VAR GStoredTextCt : CARDINAL := 1 
; VAR GTotalTextCt : CARDINAL := 0 
; VAR BaseTextCt : CARDINAL := 50000
; VAR OperationCt : CARDINAL := 100000
; VAR QueryCt : CARDINAL := 100000


; VAR RandV := NEW ( Random . Default ) . init ( fixed := TRUE ) 

; VAR GFillCt := 0 
; VAR GOperationCt := 0 
; VAR GTestCt := 0 
; VAR GTotalCt := 0 
; VAR GFailureCt := 0 

; TYPE TimeTyp = LONGREAL 
; VAR GOldTextsTick : TimeTyp := 0.0D0 
; VAR GNewTextsTick : TimeTyp := 0.0D0  
; VAR GOldTextsTime : TimeTyp := 0.0D0 
; VAR GNewTextsTime : TimeTyp := 0.0D0  
; VAR GTimingOverheadTick : TimeTyp 
; VAR GTimingOverheadTime : TimeTyp 

; VAR GStartTimeOld , GStopTimeOld : Time . T 
; VAR GStartTimeNew , GStopTimeNew : Time . T 
; VAR GStartTickOld , GStopTickOld : Tick . T 
; VAR GStartTickNew , GStopTickNew : Tick . T 

; VAR GOldTimedCt , GNewTimedCt : INTEGER 

(* These are used to accumulate totals, while the variables
   in TextClass accumulate for subintervals.
*) 
; VAR GOldOps , GNewOps : REF TextClass . OpsInfo  
; VAR GOldObjs , GNewObjs : REF TextClass . ObjsInfo  

; CONST GLen = 80 
; VAR GProgressLine : ARRAY [ 0 .. GLen ] OF CHAR 
; VAR GProgressLen : CARDINAL := 0 

; VAR GDoOld : BOOLEAN := FALSE 
; VAR GDoNew : BOOLEAN := FALSE 
; VAR GDoUnbal : BOOLEAN := FALSE 
; VAR GDoLToR : BOOLEAN := TRUE 
; VAR GDoCompareOperands : BOOLEAN := FALSE 
; VAR GDoCompareResults : BOOLEAN := FALSE 
; VAR GDoCheckTexts : BOOLEAN := FALSE 
; VAR GDoDisplayHelp : BOOLEAN := FALSE 
; VAR GDoDisplayVersion : BOOLEAN := FALSE 
; VAR GBaseLength : CARDINAL := 0 
; CONST MaxBaseLength = 1000 

; CONST StatsInterval = 10000000

; PROCEDURE GetParams ( ) 

  = VAR LParam : TEXT 
  ; VAR LParamNo : CARDINAL 
  ; VAR LChar : CHAR 

  ; BEGIN 
      GDoOld := FALSE 
    ; GDoNew := FALSE 
    ; GDoUnbal := FALSE 
    ; GDoLToR := TRUE 
    ; GDoCompareOperands := FALSE 
    ; GDoCompareResults := FALSE 
    ; GDoCheckTexts := FALSE 
    ; GDoDisplayHelp := FALSE 
    ; GDoDisplayVersion := FALSE 
    ; GBaseLength := 0 
    ; BaseTextCt := 50000
    ; OperationCt := 100000
    ; QueryCt := 100000

    ; TextClass . Flatten := FALSE
    ; TextClass . MaxFlat8 := 32
    ; TextClass . MaxFlatWide := 16

    ; LParamNo := 1 
    ; WHILE LParamNo < Params . Count  
      DO
        LParam := Params . Get ( LParamNo ) 
      ; IF Text . GetChar ( LParam , 0 ) = '-' 
        THEN
          FOR RCharNo := 1 TO Text . Length ( LParam ) - 1  
          DO
            LChar := Text . GetChar ( LParam , RCharNo ) 

          ; PROCEDURE NumArg ( VAR (* IN OUT *) Result : CARDINAL ) 
            : BOOLEAN (* Done with the current argument. *) 

            = VAR LNumParam : TEXT 
            ; VAR LValue : INTEGER 
            ; VAR LRdT : Rd . T 
            ; BEGIN 
                IF RCharNo = Text . Length ( LParam ) - 1 
                   (* No more chars in this argument. *)
                   AND LParamNo + 1 < Params . Count
                       (* An argument follows *)    
                THEN 
                  INC ( LParamNo ) 
                ; LNumParam := Params . Get ( LParamNo ) 
                ; LRdT := TextRd . New ( LNumParam ) 
                ; TRY 
                    <* FATAL FloatMode.Trap, Rd.Failure, Thread.Alerted *> 
                    BEGIN 
                      LValue := Lex . Int ( LRdT ) 
                    ; IF Rd . EOF ( LRdT ) 
                      THEN 
                        Result := MIN ( MAX ( LValue , 0 ) , MaxBaseLength )
                      ; RETURN TRUE  
                      ELSE RAISE Lex . Error 
                      END (* IF *)  
                    END (* Block *) 
                  EXCEPT 
                  Lex . Error 
                  => WL ( "Invalid numeric argument: \"" & LParam & "\",") 
                  END (* TRY EXCEPT *) 
                ELSE 
                  WL ( "Missing numeric argument," ) 
                END (* IF *) 
              ; WL ( "Using " & Fmt . Int ( Result ) & " ." ) 
              ; RETURN FALSE 
              END NumArg 

          ; BEGIN (* Block *) 
              CASE LChar 
              OF 'a' 
              => GDoNew := TRUE  
              ; GDoOld := TRUE 
              ; GDoCompareResults := TRUE  
              ; GDoCheckTexts := TRUE 
              ; TextClass . Flatten := TRUE 
              ; TextClass . MaxFlat8 := 32
              ; TextClass . MaxFlatWide := 16
              ; GBaseLength := 0 
              ; TextClass . Flatten := TRUE  
              | 'b' => IF NumArg ( GBaseLength ) THEN EXIT END  
              | 'c' => GDoCheckTexts := TRUE 
              | 'd' => IF NumArg ( OperationCt ) THEN EXIT END  
              | 'f' => IF NumArg ( TextClass . MaxFlat8 ) 
                       THEN 
                         TextClass . MaxFlatWide := TextClass . MaxFlat8 DIV 2 
                       ; TextClass . Flatten := TextClass . MaxFlat8 > 0  
                       ; EXIT 
                       ELSE 
                         TextClass . Flatten := FALSE 
                       END (* IF *)    
              | 'g' => IF NumArg ( BaseTextCt ) THEN EXIT END  
              | 'h' => GDoDisplayHelp := TRUE 
              | 'l' => GDoUnbal := TRUE 
              | 'n' => GDoNew := TRUE  
              | 'o' => GDoOld := TRUE 
              | 'p' => GDoCompareOperands := TRUE  
              | 'q' => IF NumArg ( QueryCt ) THEN EXIT END  
              | 'r' 
              => GDoUnbal := TRUE 
              ; GDoLToR := FALSE 
              | 's' => GDoCompareResults := TRUE  
              | 'v' => GDoDisplayVersion := TRUE 
              ELSE 
                WL ( "Invalid option character: \'" 
                     & Text . FromChar ( LChar )  & "\'"  
                   ) 
              ; GDoDisplayHelp := TRUE 
              END (* CASE *) 
            END (* Block *)   
          END (* FOR *) 
        ELSE
          WL ( "Invalid parameter: \"" & LParam & "\"" ) 
        ; GDoDisplayHelp := TRUE 
        END (* IF *)  
      ; INC ( LParamNo ) 
      END (* WHILE *)  
    ; IF NOT GDoNew OR NOT GDoOld 
      THEN
        GDoCompareOperands := FALSE 
      ; GDoCompareResults := FALSE 
      END (* IF *) 
    END GetParams 

; PROCEDURE WL ( T : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , T ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    END WL 

; PROCEDURE DisplayHelp ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WL ( "Usage: OrdTextsTest {-{option}}" ) 
    ; WL ( "  Options are: " ) 
    ; WL ( "  -a Do all tests: -n -o -s -f 32." ) 
    ; WL ( "  -b <n> All base TEXT strings will have exact length <n>." ) 
    ; WL ( "         Random, if -b or <n> is omitted or <n> is zero." ) 
    ; WL ( "  -c Check computed TEXT representations for validity." ) 
    ; WL ( "  -d <n> Do <n> Cat and/or Sub operations." ) 
    ; WL ( "     defaults to 100000" ) 
    ; WL ( "  -f The new Cat flattens results not longer than <n> bytes." ) 
    ; WL ( "     Defaults to zero (no flattening)." ) 
    ; WL ( "  -g <n> Generate <n> initial flat TEXTs." ) 
    ; WL ( "     defaults to 50000" ) 
    ; WL ( "  -h Display help text and exit." ) 
    ; WL ( "  -l Build highly unbalanced Cat trees, L to R." ) 
    ; WL ( "  -n Test the new operations." ) 
    ; WL ( "  -o Test the old operations." ) 
    ; WL ( "  -p Compare old/new values of operands of operations." ) 
    ; WL ( "     (only if both -n and -o are specified." ) 
    ; WL ( "  -q <n> Do <n> querying operations." ) 
    ; WL ( "     defaults to 100000" ) 
    ; WL ( "  -r Build highly unbalanced Cat trees, R to L." ) 
    ; WL ( "  -s Compare old/new values of results of operations." ) 
    ; WL ( "     (only if both -n and -o are specified." ) 
    ; WL ( "  -v Display version and exit." ) 
    ; Wr . Flush ( PWrT ) 
    END DisplayHelp 

; PROCEDURE DisplayVersion ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WL ( "Cm3 new TEXT test driver, version 1.0" ) 
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

; PROCEDURE NoteProgress ( VAR Ct : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF Ct MOD 5000 = 0 
      THEN (* New line. *) 
        ResetProgress ( ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; AppendProgress ( Fmt . Pad ( Fmt . Int ( Ct ) , 7 ) ) 
      ELSIF Ct MOD 1000 = 0 
      THEN 
        AppendProgress ( Fmt . Int ( ( Ct DIV 1000 ) MOD 10 ) ) 
      ELSIF Ct MOD 500 = 0 
      THEN 
        AppendProgress ( "+" ) 
      ELSIF Ct MOD 100 = 0 
      THEN 
        AppendProgress ( "." )
      END (* IF *)
    ; CASE ( Ct DIV 10 ) MOD 4 <* NOWARN *>  
      OF 0 => AppendProgress ( "/\010" )
      | 1 => AppendProgress ( "-\010" )
      | 2 => AppendProgress ( "\\\010" )
      | 3 =>AppendProgress ( "|\010" )
      END (* CASE *)
    ; INC ( Ct ) 
    END NoteProgress 

; PROCEDURE ShowExactProgress ( Ct : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . PutText ( PWrT , Fmt . Pad ( Fmt . Int ( Ct ) , 7 ) ) 
    ; Wr . Flush ( PWrT )    
    END ShowExactProgress

; PROCEDURE StartTimingOld ( ) 

  = BEGIN 
      TextClass . CollectStats := TRUE
    ; GStartTimeOld := Time . Now ( ) 
    ; GStartTickOld := Tick . Now ( ) 
    END StartTimingOld 

; PROCEDURE StopTimingOld ( ) 

  = VAR LElapsedSecsTick , LElapsedSecsTime  : LONGREAL 

  ; BEGIN 
      GStopTickOld := Tick . Now ( ) 
    ; GStopTimeOld := Time . Now ( )
    ; LElapsedSecsTick := Tick . ToSeconds ( GStopTickOld - GStartTickOld ) 
    ; LElapsedSecsTime := GStopTimeOld - GStartTimeOld 
    ; GOldTextsTick := GOldTextsTick + LElapsedSecsTick 
    ; GOldTextsTime := GOldTextsTime + LElapsedSecsTime 
    ; INC ( GOldTimedCt ) 
    ; TextClass . CollectStats := FALSE
    END StopTimingOld 

; PROCEDURE StartTimingNew ( ) 

  = BEGIN 
      TextClass . CollectStats := TRUE
    ; GStartTimeNew := Time . Now ( ) 
    ; GStartTickNew := Tick . Now ( ) 
    END StartTimingNew 

; PROCEDURE StopTimingNew ( ) 

  = VAR LElapsedSecsTick , LElapsedSecsTime  : LONGREAL 

  ; BEGIN 
      GStopTickNew := Tick . Now ( ) 
    ; GStopTimeNew := Time . Now ( )
    ; LElapsedSecsTick := Tick . ToSeconds ( GStopTickNew - GStartTickNew ) 
    ; LElapsedSecsTime := GStopTimeNew - GStartTimeNew 
    ; GNewTextsTick := GNewTextsTick + LElapsedSecsTick 
    ; GNewTextsTime := GNewTextsTime + LElapsedSecsTime 
    ; INC ( GNewTimedCt ) 
    ; TextClass . CollectStats := FALSE
    END StopTimingNew 

; VAR EstimateCt := 10000 

; PROCEDURE EstimateTimingOverhead ( ) 

  = BEGIN
      GOldTextsTick := 0.0D0 
    ; GOldTextsTime := 0.0D0 
    ; GOldTimedCt := 0 
    ; FOR RI := 0 TO EstimateCt 
      DO
        StartTimingOld ( ) 
      ; StopTimingOld ( ) 
      END (* FOR *) 
    ; GTimingOverheadTick := GOldTextsTick / FLOAT ( GOldTimedCt , LONGREAL ) 
    ; GTimingOverheadTime := GOldTextsTime / FLOAT ( GOldTimedCt , LONGREAL ) 
    ; GOldTimedCt := 0 
    ; GOldTextsTick := 0.0D0 
    ; GOldTextsTime := 0.0D0 
    END EstimateTimingOverhead 

; CONST MaxLine = 80 

; PROCEDURE HexAddr ( Addr : REFANY ) : TEXT 

  = VAR LAddress : INTEGER 

  ; BEGIN 
      LAddress := UnsafeUtils . IntOfRefany ( Addr ) 
    ; RETURN "16_" & Fmt . Pad ( Fmt . Int ( LAddress , 16 ) , 8 , '0' ) 
    END HexAddr

; PROCEDURE DumpText ( Text : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF Text = NIL 
      THEN 
        Wr . PutText ( WrT , "NIL" ) 
      ELSE 
        Wr . PutText ( WrT , HexAddr ( Text ) ) 
      ; Wr . PutChar ( WrT , ':' ) 
      ; Wr . PutText ( WrT , Text ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      END (* IF *) 
    END DumpText 

; PROCEDURE WriteTextNo ( TextNo : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      IF TextNo >= 0 
      THEN 
        Wr . PutText ( WrT , ", Text no " )
      END (* IF *) 
    ; Wr . PutText ( WrT , Fmt . Int ( TextNo ) ) 
    END WriteTextNo 

; PROCEDURE NoteFailure ( ) 

  = BEGIN 
      INC ( GFailureCt ) 
    END NoteFailure 
  
; PROCEDURE CompareTexts 
    ( OldText : TEXT 
    ; NewText : TEXT 
    ; Context : TEXT 
    ; TextNo : INTEGER  
    ) 

  = VAR LOldLength , LNewLength : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      IF GDoOld AND GDoNew 
      THEN
        IF OldText = NIL 
        THEN 
          IF NewText = NIL 
          THEN
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , Context )
          ; WriteTextNo ( TextNo ) 
          ; Wr . PutText ( WrT , ": Both texts are NIL." )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; NoteFailure ( ) 
          ; RedisplayProgress ( ) 
          ELSE 
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , Context )
          ; WriteTextNo ( TextNo ) 
          ; Wr . PutText ( WrT , ": Old text is NIL, new is not." )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; DumpText ( NewText ) 
          ; NoteFailure ( ) 
          ; RedisplayProgress ( ) 
          END (* IF *) 
        ELSE 
          IF NewText = NIL 
          THEN
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , Context )
          ; WriteTextNo ( TextNo ) 
          ; Wr . PutText ( WrT , ": Old text is non-NIL, new is NIL." )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; DumpText ( OldText ) 
          ; NoteFailure ( ) 
          ; RedisplayProgress ( ) 
          ELSE 
            LOldLength := Text . Length ( OldText ) 
          ; LNewLength := Text . Length ( NewText ) 
          ; IF LOldLength # LNewLength  
            THEN 
              Wr . PutText ( WrT , Wr . EOL ) 
            ; Wr . PutText ( WrT , Context )
            ; WriteTextNo ( TextNo ) 
            ; Wr . PutText ( WrT , ": Texts have unequal lengths, " )
            ; Wr . PutText ( WrT , Fmt . Int ( LOldLength ) ) 
            ; Wr . PutText ( WrT , ", " ) 
            ; Wr . PutText ( WrT , Fmt . Int ( LNewLength ) )
            ; Wr . PutText ( WrT , Wr . EOL ) 
            ; DumpText ( OldText ) 
            ; DumpText ( NewText ) 
            ; NoteFailure ( ) 
            ; RedisplayProgress ( ) 
            ELSIF Text . Equal ( OldText , NewText ) 
            THEN (* Equal lengths and contents. *) 
            ELSE 
              Wr . PutText ( WrT , Wr . EOL ) 
            ; Wr . PutText ( WrT , Context )
            ; WriteTextNo ( TextNo ) 
            ; Wr . PutText ( WrT , ": Texts are not equal:" )
            ; Wr . PutText ( WrT , Wr . EOL ) 
            ; DumpText ( OldText ) 
            ; DumpText ( NewText ) 
            ; NoteFailure ( ) 
            ; RedisplayProgress ( ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END CompareTexts 

; PROCEDURE CompareInt  
    ( OldInt , NewInt : INTEGER 
    ; Context : TEXT 
    ; TextNo1 : INTEGER 
    ; TextNo2 : INTEGER := - 1 
    )

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF GDoOld AND GDoNew 
      THEN 
        IF OldInt # NewInt 
        THEN
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteTextNo ( TextNo1 ) 
        ; IF TextNo2 >= 0 THEN WriteTextNo ( TextNo2 ) END 
        ; Wr . PutText ( WrT , ": Integer values unequal, old: " )
        ; Wr . PutText ( WrT , Fmt . Int ( OldInt ) )
        ; Wr . PutText ( WrT , ", new: " )
        ; Wr . PutText ( WrT , Fmt . Int ( NewInt ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT )    
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        END (* IF *) 
      END (* IF *) 
    END CompareInt 

; PROCEDURE CompareBool 
    ( OldBool , NewBool : BOOLEAN ; Context : TEXT 
    ; TextNo1 : INTEGER 
    ; TextNo2 : INTEGER := - 1  
    ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF GDoOld AND GDoNew 
      THEN 
        IF OldBool # NewBool 
        THEN
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteTextNo ( TextNo1 ) 
        ; IF TextNo2 >= 0 THEN WriteTextNo ( TextNo2 ) END 
        ; Wr . PutText ( WrT , ": Booleans unequal, old: " )
        ; Wr . PutText ( WrT , Fmt . Bool ( OldBool ) )
        ; Wr . PutText ( WrT , ", new: " )
        ; Wr . PutText ( WrT , Fmt . Bool ( NewBool ) )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT )    
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        END (* IF *) 
      END (* IF *) 
    END CompareBool

; PROCEDURE CheckNoWideChars ( T , Context : TEXT ; TextNo : INTEGER) 

  = VAR LWCharsRef : REF ARRAY OF WIDECHAR 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      LWCharsRef := NEW ( REF ARRAY OF WIDECHAR , Text . Length ( T ) ) 
    ; Text . SetWideChars ( LWCharsRef ^ , T ) 
    ; FOR RI := FIRST ( LWCharsRef ^ ) TO LAST ( LWCharsRef ^ ) 
      DO WITH WWC = LWCharsRef ^ [ RI ] 
        DO 
          IF ORD ( WWC ) > ORD ( LAST ( CHAR ) ) 
          THEN 
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , Context )
          ; WriteTextNo ( TextNo ) 
          ; Wr . PutText ( WrT , ", contains wide character: VAL ( " )
          ; Wr . PutText ( WrT , Fmt . Int ( ORD ( WWC ) ) )
          ; Wr . PutText ( WrT , ", WIDECHAR ), at position " )
          ; Wr . PutText ( WrT , Fmt . Int ( RI ) )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT )    
          ; NoteFailure ( ) 
          ; RedisplayProgress ( ) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END CheckNoWideChars

; PROCEDURE CheckHasWideChars ( T , Context : TEXT ; TextNo : INTEGER) 

  = VAR LWCharsRef : REF ARRAY OF WIDECHAR 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      LWCharsRef := NEW ( REF ARRAY OF WIDECHAR , Text . Length ( T ) ) 
    ; Text . SetWideChars ( LWCharsRef ^ , T ) 
    ; FOR RI := FIRST ( LWCharsRef ^ ) TO LAST ( LWCharsRef ^ ) 
      DO WITH WWC = LWCharsRef ^ [ RI ] 
        DO 
          IF ORD ( WWC ) > ORD ( LAST ( CHAR ) ) 
          THEN 
            RETURN 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Context )
    ; WriteTextNo ( TextNo ) 
    ; Wr . PutText ( WrT , ", contains no wide characters, though " )
    ; Wr . PutText ( WrT , "Text.HasWideChars reported it does." )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT )    
    ; NoteFailure ( ) 
    ; RedisplayProgress ( ) 
    END CheckHasWideChars

; PROCEDURE CompareWides 
    ( READONLY Old , New : ARRAY OF WIDECHAR 
    ; Context : TEXT 
    ; TextNo : INTEGER  
    ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF GDoOld AND GDoNew 
      THEN 
        IF NUMBER ( Old ) # NUMBER ( New ) OR Old # New
        THEN 
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteTextNo ( TextNo ) 
        ; Wr . PutText ( WrT , ": WIDECHAR array values unequal, old: " )
(* TODO: Convert escapes in these writes: *) 
        ; Wr . PutWideString ( WrT , Old )
        ; Wr . PutText ( WrT , ", new: " )
(* TODO: Convert escapes in these writes: *) 
        ; Wr . PutWideString ( WrT , New)
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT )    
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        END (* IF *)  
      END (* IF *) 
    END CompareWides
        
; PROCEDURE CompareChars 
    ( READONLY Old , New : ARRAY OF CHAR 
    ; Context : TEXT 
    ; TextNo : INTEGER  
    ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF GDoOld AND GDoNew 
      THEN 
        IF NUMBER ( Old ) # NUMBER ( New ) OR Old # New
        THEN 
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteTextNo ( TextNo ) 
        ; Wr . PutText ( WrT , ": CHAR array values unequal, old: " )
(* TODO: Convert escapes in these writes: *) 
        ; Wr . PutString ( WrT , Old )
        ; Wr . PutText ( WrT , ", new: " )
(* TODO: Convert escapes in these writes: *) 
        ; Wr . PutString ( WrT , New)
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT )    
        ; NoteFailure ( ) 
        ; RedisplayProgress ( ) 
        END (* IF *)  
      END (* IF *) 
    END CompareChars

; PROCEDURE CheckText 
    ( Text : TEXT ; Context : TEXT ; TextNo : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      IF GDoCheckTexts
      THEN 
        TRY
          TextUtils . VerifyText ( Text ) 
        EXCEPT 
        TextUtils . BadInvariant ( EMessage )
        => Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , "##############################################" )
        ; Wr . PutText ( WrT , "##############################################" )
        ; Wr . PutText ( WrT , "#######################" )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , Context )
        ; WriteTextNo ( TextNo ) 
        ; Wr . PutText ( WrT , ": Bad invariant: " )
        ; Wr . PutText ( WrT , EMessage )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; DumpText ( Text ) 
        ; NoteFailure ( ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT )
        ; RedisplayProgress ( ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END CheckText 

; PROCEDURE OpLabel ( Op : TextClass . Op ) : TEXT  

  = BEGIN 
      CASE Op 
      OF TextClass . Op . FromChar =>      RETURN "FromCh   "
      | TextClass . Op . FromWideChar =>   RETURN "FromWCh  "
      | TextClass . Op . FromChars =>      RETURN "FromChs  "
      | TextClass . Op . FromWideChars =>  RETURN "FromWChs "
      | TextClass . Op . Cat =>            RETURN "Cat      "
      | TextClass . Op . Sub =>            RETURN "Sub      "
      | TextClass . Op . Equal =>          RETURN "Equal    "
      | TextClass . Op . Compare =>        RETURN "Compare  "
      | TextClass . Op . Hash =>           RETURN "Hash     "
      | TextClass . Op . HasWideChars =>   RETURN "HasWideCh"
      | TextClass . Op . GetChar =>        RETURN "GetCh    "
      | TextClass . Op . GetWideChar =>    RETURN "GetWCh   "
      | TextClass . Op . SetChars =>       RETURN "SetCh    "
      | TextClass . Op . SetWideChars =>   RETURN "SetWCh   "
      | TextClass . Op . FindChar =>       RETURN "FindCh   "
      | TextClass . Op . FindWideChar =>   RETURN "FindWCh  "
      | TextClass . Op . FindCharR =>      RETURN "FindChR  "
      | TextClass . Op . FindWideCharR =>  RETURN "FindWChR "
      | TextClass . Op . get_char =>       RETURN "get_ch   "
      | TextClass . Op . get_wide_char =>  RETURN "get_w_ch "
      | TextClass . Op . get_chars =>      RETURN "get_chs  "
      | TextClass . Op . get_wide_chars => RETURN "get_w_chs"
      ELSE                                 RETURN "         "
      END (* CASE *) 
    END OpLabel 

; PROCEDURE DisplayOpLine 
    ( READONLY OpInfo : TextClass . OpInfo ; Age , OpLabel : TEXT )

  = VAR AvgTime , AvgRecurse , AvgIter : REAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OpInfo . GroundCt # 0 
      THEN 
        Wr . PutText ( WrT , Age ) 
      ; Wr . PutText ( WrT , OpLabel ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( OpInfo . Time1 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( OpInfo . Time2 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; AvgTime := OpInfo . Time1 / FLOAT ( OpInfo . GroundCt ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( AvgTime , Fmt . Style . Fix , 10 ) , 15 )
          ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( OpInfo . GroundCt ) , 10 ) ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( OpInfo . RecurseCt ) , 10 ) ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( OpInfo . IterCt ) , 10 ) ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( OpInfo . MaxRecurseCt ) , 10 ) ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( OpInfo . MaxIterCt ) , 10 ) ) 
      ; AvgRecurse 
          := FLOAT ( OpInfo . RecurseCt ) / FLOAT ( OpInfo . GroundCt ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
             ( Fmt . Real ( AvgRecurse , Fmt . Style . Fix , 6 ) , 12 )
          )
      ; AvgIter := FLOAT ( OpInfo . IterCt ) / FLOAT ( OpInfo . GroundCt ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad ( Fmt . Real ( AvgIter , Fmt . Style . Fix , 6 ) , 12 ) 
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (* IF *) 
    END DisplayOpLine 

; PROCEDURE DisplayOps ( OldOps , NewOps : REF TextClass . OpsInfo )

  = VAR TotalOldTime1 , TotalOldTime2 , TotalNewTime1 , TotalNewTime2 : REAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OldOps # NIL OR NewOps # NIL 
      THEN 
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      ; Wr . PutText ( WrT , "------- Operation statistics: ------------------" )
      ; Wr . PutText ( WrT , "----------------------------------------" )
      ; Wr . PutText ( WrT , "----------------------------------" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "Operation         Time1     Time2       Avg time" )
      ; Wr . PutText ( WrT , " Top calls   Recurse   Iterate   Max rec" )
      ; Wr . PutText ( WrT , "  Max iter     Avg rec    Avg iter" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; TotalOldTime1 := 0.0 
      ; TotalOldTime2 := 0.0 
      ; TotalNewTime1 := 0.0 
      ; TotalNewTime2 := 0.0 
      ; FOR ROp := FIRST ( TextClass . Op ) TO TextClass . Op . FindWideCharR
        DO
          IF OldOps # NIL 
          THEN 
            DisplayOpLine ( OldOps ^ [ ROp ] , "Old " , OpLabel ( ROp ))  
          ; TotalOldTime1 := TotalOldTime1 + OldOps ^ [ ROp ] . Time1
          ; TotalOldTime2 := TotalOldTime2 + OldOps ^ [ ROp ] . Time2
          END (* IF *) 
        ; IF NewOps # NIL 
          THEN 
            DisplayOpLine ( NewOps ^ [ ROp ] , "New " , OpLabel ( ROp ) )  
          ; TotalNewTime1 := TotalNewTime1 + NewOps ^ [ ROp ] . Time1
          ; TotalNewTime2 := TotalNewTime2 + NewOps ^ [ ROp ] . Time2
          END (* IF *) 
        END (* FOR *) 

      ; Wr . PutText ( WrT , "Old " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalOldTime1 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalOldTime2 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "New " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalNewTime1 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalNewTime2 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; Wr . PutText ( WrT , "--------- Internal operations: -----------------" )
      ; Wr . PutText ( WrT , "----------------------------------------" )
      ; Wr . PutText ( WrT , "----------------------------------" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; TotalOldTime1 := 0.0 
      ; TotalOldTime2 := 0.0 
      ; TotalNewTime1 := 0.0 
      ; TotalNewTime2 := 0.0 
      ; FOR ROp := TextClass . Op . get_char TO LAST ( TextClass . Op ) 
        DO
          IF OldOps # NIL 
          THEN 
            DisplayOpLine ( OldOps ^ [ ROp ] , "Old " , OpLabel ( ROp ))  
          ; TotalOldTime1 := TotalOldTime1 + OldOps ^ [ ROp ] . Time1
          ; TotalOldTime2 := TotalOldTime2 + OldOps ^ [ ROp ] . Time2
          END (* IF *) 
        ; IF NewOps # NIL 
          THEN 
            DisplayOpLine ( NewOps ^ [ ROp ] , "New " , OpLabel ( ROp ) )  
          ; TotalNewTime1 := TotalNewTime1 + NewOps ^ [ ROp ] . Time1
          ; TotalNewTime2 := TotalNewTime2 + NewOps ^ [ ROp ] . Time2
          END (* IF *) 
        END (* FOR *) 
      ; Wr . PutText ( WrT , "Old " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalOldTime1 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalOldTime2 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "New " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalNewTime1 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Real ( TotalNewTime2 , Fmt . Style . Fix , 5 ) , 10 )
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; Wr . PutText ( WrT , "------------------------------------------------" )
      ; Wr . PutText ( WrT , "----------------------------------------" )
      ; Wr . PutText ( WrT , "----------------------------------" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      END (* IF *) 
    END DisplayOps 

; PROCEDURE CheckpointOps ( ) 

  = BEGIN 
      DisplayOps ( TextClass . OldOps , TextClass . NewOps ) 
    ; IF TextClass . OldOps # NIL 
      THEN 
        IF GOldOps # NIL 
        THEN 
          FOR ROp := FIRST ( TextClass . Op ) TO LAST ( TextClass . Op ) 
          DO 
            WITH WS = GOldOps ^ [ ROp ] , WL = TextClass . OldOps [ ROp ] 
            DO 
              WS . Time1 := WS . Time1 + WL . Time1 
            ; WS . Time2 := WS . Time2 + WL . Time2 
            ; INC ( WS . GroundCt , WL . GroundCt ) 
            ; INC ( WS . RecurseCt , WL . RecurseCt ) 
            ; INC ( WS . IterCt , WL . IterCt ) 
            ; INC ( WS . MaxRecurseCt , WL . MaxRecurseCt ) 
            ; INC ( WS . MaxIterCt , WL . MaxIterCt ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; TextClass . InitOps ( TextClass . OldOps ^ ) 
      END (* IF *) 
    ; IF TextClass . NewOps # NIL 
      THEN 
        IF GNewOps # NIL 
        THEN 
          FOR ROp := FIRST ( TextClass . Op ) TO LAST ( TextClass . Op ) 
          DO 
            WITH WS = GNewOps ^ [ ROp ] , WL = TextClass . NewOps [ ROp ] 
            DO 
              WS . Time1 := WS . Time1 + WL . Time1 
            ; WS . Time2 := WS . Time2 + WL . Time2 
            ; INC ( WS . GroundCt , WL . GroundCt ) 
            ; INC ( WS . RecurseCt , WL . RecurseCt ) 
            ; INC ( WS . IterCt , WL . IterCt ) 
            ; INC ( WS . MaxRecurseCt , WL . MaxRecurseCt ) 
            ; INC ( WS . MaxIterCt , WL . MaxIterCt ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; TextClass . InitOps ( TextClass . NewOps ^ ) 
      END (* IF *) 
    END CheckpointOps 

; PROCEDURE ObjLabel ( Obj : TextClass . Obj ) : TEXT 

  = BEGIN 
      CASE Obj 
      OF TextClass . Obj . Text8Short => RETURN "T8Short  " 
      | TextClass . Obj . Text8 =>       RETURN "T8       "
      | TextClass . Obj . Text8Chars =>  RETURN "T8Chars  "
      | TextClass . Obj . Text16Short => RETURN "T16Short "
      | TextClass . Obj . Text16 =>      RETURN "T16      "
      | TextClass . Obj . Text16Chars => RETURN "T16Chars "
      | TextClass . Obj . TextCat =>     RETURN "TextCat  "
      | TextClass . Obj . TextSub =>     RETURN "TextSub  "
      ELSE                               RETURN "         "
      END (* CASE *) 
    END ObjLabel 

; CONST HeapSz = BYTESIZE ( Word . T )

; PROCEDURE DisplayObjLine 
    ( ObjInfo : TextClass . ObjInfo ; Age : TEXT ; Obj : TextClass . Obj )  

  = VAR AvgAllocSize , AvgRetainedSize : REAL 
  ; VAR AllocSize , CollectSize : INTEGER  
  ; VAR RetainedCt , RetainedSize : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF ObjInfo . AllocCt # 0 
      THEN
        Wr . PutText ( WrT , Age ) 
      ; Wr . PutText ( WrT , ObjLabel ( Obj ) ) 
      ; Wr . PutText 
         ( WrT , Fmt . Pad ( Fmt . Int ( ObjInfo . AllocCt ) , 10 ) ) 
      ; AllocSize 
          := ObjInfo . AllocSize + ObjInfo . AllocCt * HeapSz 
      ; Wr . PutText 
         ( WrT , Fmt . Pad ( Fmt . Int ( AllocSize ) , 10 ) )
      ; IF ObjInfo . AllocSize # 0 
        THEN 
          AvgAllocSize 
            := FLOAT ( ObjInfo . AllocSize ) / FLOAT ( ObjInfo . AllocCt )  
        ; Wr . PutText 
            ( WrT 
            , Fmt . Pad 
                ( Fmt . Real ( AvgAllocSize , Fmt . Style . Fix , 2 ) , 10 ) 
            ) 
        ELSE Wr . PutText ( WrT , "     <n/a>" ) 
        END (* IF *) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( ObjInfo . AllocChars ) , 10 ) ) 
      ; CollectSize 
          := ObjInfo . CollectSize + ObjInfo . CollectCt * HeapSz 
      ; RetainedCt := ObjInfo . AllocCt - ObjInfo . CollectCt 
      ; RetainedSize := AllocSize - CollectSize  
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( RetainedCt ) , 10 ) ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( RetainedSize ) , 10 ) ) 
      ; IF RetainedCt # 0 
        THEN 
          AvgRetainedSize := FLOAT ( RetainedSize ) / FLOAT ( RetainedCt )  
        ; Wr . PutText 
            ( WrT 
            , Fmt . Pad 
                ( Fmt . Real ( AvgRetainedSize , Fmt . Style . Fix , 2 ) , 10 ) 
            ) 
        ELSE Wr . PutText ( WrT , "     <n/a>" ) 
        END (* IF *) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( ObjInfo . CollectChars ) , 10 ) ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (* IF *) 
    END DisplayObjLine 

; PROCEDURE DisplayObjs ( OldObjs , NewObjs : REF TextClass . ObjsInfo )

  = VAR TotalOldAllocCt , TotalOldAllocSize : INTEGER 
  ; VAR TotalOldCollectCt , TotalOldCollectSize : INTEGER 
  ; VAR TotalNewAllocCt , TotalNewAllocSize : INTEGER 
  ; VAR TotalNewCollectCt , TotalNewCollectSize : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OldObjs # NIL OR NewObjs # NIL 
      THEN 
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      ; Wr . PutText ( WrT , "------- Heap statistics: ------------------" )
      ; Wr . PutText ( WrT , "-------------------------------" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "Object kind " )
      ; Wr . PutText ( WrT , "    AllocCt   AllocSz  AllocAvg AllocChrs" )
      ; Wr . PutText ( WrT , "   RetainCt  RetainSz RetainAvg CollChars" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; TotalOldAllocCt := 0 
      ; TotalOldAllocSize := 0 
      ; TotalOldCollectCt := 0 
      ; TotalOldCollectSize := 0 
      ; TotalNewAllocCt := 0 
      ; TotalNewAllocSize := 0 
      ; TotalNewCollectCt := 0 
      ; TotalNewCollectSize := 0 
      ; FOR RObj := FIRST ( TextClass . Obj ) TO LAST ( TextClass . Obj ) 
        DO
          IF OldObjs # NIL 
          THEN 
            DisplayObjLine ( OldObjs ^ [ RObj ] , "Old " , RObj )  
          ; INC ( TotalOldAllocCt , OldObjs ^ [ RObj ] . AllocCt ) 
          ; INC ( TotalOldAllocSize , OldObjs ^ [ RObj ] . AllocSize ) 
          ; INC ( TotalOldAllocSize , OldObjs ^ [ RObj ] . AllocCt * HeapSz ) 
          ; INC ( TotalOldCollectCt , OldObjs ^ [ RObj ] . CollectCt ) 
          ; INC ( TotalOldCollectSize , OldObjs ^ [ RObj ] . CollectSize ) 
          ; INC ( TotalOldCollectSize , OldObjs ^ [ RObj ] . CollectCt * HeapSz ) 
          END (* IF *) 
        ; IF NewObjs # NIL 
          THEN 
            DisplayObjLine ( NewObjs ^ [ RObj ] , "New " , RObj )  
          ; INC ( TotalNewAllocCt , NewObjs ^ [ RObj ] . AllocCt ) 
          ; INC ( TotalNewAllocSize , NewObjs ^ [ RObj ] . AllocSize ) 
          ; INC ( TotalNewAllocSize , NewObjs ^ [ RObj ] . AllocCt * HeapSz ) 
          ; INC ( TotalNewCollectCt , NewObjs ^ [ RObj ] . CollectCt ) 
          ; INC ( TotalNewCollectSize , NewObjs ^ [ RObj ] . CollectSize ) 
          ; INC ( TotalNewCollectSize , NewObjs ^ [ RObj ] . CollectCt * HeapSz ) 
          END (* IF *) 
        END (* FOR *) 

      ; Wr . PutText ( WrT , "Old " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( TotalOldAllocCt ) , 10 ) )
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( TotalOldAllocSize ) , 10 ) )
      ; Wr . PutText ( WrT , Fmt . Pad ( "" , 20 ) )
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Int ( TotalOldAllocCt - TotalOldCollectCt ) , 10 ) 
          )
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Int ( TotalOldAllocSize - TotalOldCollectSize ) , 10 ) 
          )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "New " ) 
      ; Wr . PutText ( WrT , "Totals:  " ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( TotalNewAllocCt ) , 10 ) )
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Int ( TotalNewAllocSize ) , 10 ) )
      ; Wr . PutText ( WrT , Fmt . Pad ( "" , 20 ) )
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Int ( TotalNewAllocCt - TotalNewCollectCt ) , 10 ) 
          )
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Int ( TotalNewAllocSize - TotalNewCollectSize ) , 10 ) 
          )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "-------------------------------------------" )
      ; Wr . PutText ( WrT , "-------------------------------" )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT )    
      END (* IF *) 
    END DisplayObjs 

; PROCEDURE CheckpointObjs ( ) 

  = BEGIN 
      DisplayObjs ( TextClass . OldObjs , TextClass . NewObjs ) 
    ; IF TextClass . OldObjs # NIL 
      THEN 
        IF GOldObjs # NIL 
        THEN 
          FOR RObj := FIRST ( TextClass . Obj ) TO LAST ( TextClass . Obj ) 
          DO 
            WITH WS = GOldObjs ^ [ RObj ] , WL = TextClass . OldObjs [ RObj ] 
            DO 
              INC ( WS . AllocCt , WL . AllocCt ) 
            ; INC ( WS . AllocSize , WL . AllocSize ) 
            ; INC ( WS . AllocChars , WL . AllocChars ) 
            ; INC ( WS . CollectCt , WL . CollectCt ) 
            ; INC ( WS . CollectSize , WL . CollectSize ) 
            ; INC ( WS . CollectChars , WL . CollectChars ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; TextClass . InitObjs ( TextClass . OldObjs ^ ) 
      END (* IF *) 
    ; IF TextClass . NewObjs # NIL 
      THEN 
        IF GNewObjs # NIL 
        THEN 
          FOR RObj := FIRST ( TextClass . Obj ) TO LAST ( TextClass . Obj ) 
          DO 
            WITH WS = GNewObjs ^ [ RObj ] , WL = TextClass . NewObjs [ RObj ] 
            DO 
              INC ( WS . AllocCt , WL . AllocCt ) 
            ; INC ( WS . AllocSize , WL . AllocSize ) 
            ; INC ( WS . AllocChars , WL . AllocChars ) 
            ; INC ( WS . CollectCt , WL . CollectCt ) 
            ; INC ( WS . CollectSize , WL . CollectSize ) 
            ; INC ( WS . CollectChars , WL . CollectChars ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; TextClass . InitObjs ( TextClass . NewObjs ^ ) 
      END (* IF *) 
    END CheckpointObjs 

; VAR GLastCheckpointCt : INTEGER := 0

; PROCEDURE PrintCheckpoint ( Label : TEXT ; OperationCt : INTEGER ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF OperationCt > GLastCheckpointCt 
      THEN 
        ShowExactProgress ( OperationCt ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "For the last " ) 
      ; Wr . PutText ( WrT , Fmt . Int ( OperationCt - GLastCheckpointCt ) ) 
      ; Wr . PutText ( WrT , " " ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , ":" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; CheckpointOps ( ) 
      ; CheckpointObjs ( ) 
      ; GLastCheckpointCt := OperationCt 
      END (* IF *) 
    END PrintCheckpoint 

; PROCEDURE DisplayInt ( Label : TEXT ; Value : INTEGER )

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( WrT , "        " ) 
    ; Wr . PutText ( WrT , Label ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( Value ) , 7 ) ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END DisplayInt 

; PROCEDURE DisplayAvgReal ( Label : TEXT ; Value : REAL ; Count : INTEGER )

  = VAR Result : REAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF Count > 0 
      THEN 
        Result := Value / FLOAT ( Count ) 
      ; Wr . PutText ( WrT , "Average " ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , ": " ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad ( Fmt . Real ( Result , Fmt . Style . Fix , 2 ) , 10 )
          ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (* IF *) 
    END DisplayAvgReal 

; PROCEDURE DisplayAvgInt ( Label : TEXT ; Value : INTEGER ; Count : INTEGER )

  = BEGIN 
      DisplayAvgReal ( Label , FLOAT ( Value ) , Count ) 
    END DisplayAvgInt

; PROCEDURE DisplayTextMeasures ( READONLY StoredTexts : ARRAY OF TEXT ) 

  = VAR Depth , MaxDepth , DepthTot : INTEGER := 0  
  ; VAR LeafCt , MaxLeafCt , LeafCtTot : INTEGER := 0
  ; VAR Imbalance , ImbalanceTot : REAL := 0.0
  ; VAR LengthDiff , LengthDiffTot : INTEGER := 0 
  ; VAR TextCt : INTEGER := 0

  ; BEGIN 
      FOR RI := FIRST ( StoredTexts ) TO LAST ( StoredTexts )   
      DO 
        WITH WT = StoredTexts [ RI ] 
        DO IF WT # NIL 
        THEN 
            TextUtils . MeasureText 
              ( WT , Depth , LeafCt , Imbalance , LengthDiff ) 
          ; INC ( DepthTot , Depth )
          ; MaxDepth := MAX ( MaxDepth , Depth )  
          ; INC ( LeafCtTot , LeafCt ) 
          ; MaxLeafCt := MAX ( MaxLeafCt , LeafCt ) 
          ; ImbalanceTot := ImbalanceTot + Imbalance 
          ; INC ( LengthDiffTot , LengthDiff ) 
          ; INC ( TextCt ) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *)
    ; IF TextCt > 0 
      THEN
        DisplayAvgInt  ( "depth            " , DepthTot , TextCt ) 
      ; DisplayInt     ( "max depth        " , MaxDepth ) 
      ; DisplayAvgInt  ( "leaf count       "  , LeafCtTot , TextCt ) 
      ; DisplayInt     ( "max leaf count   " , MaxLeafCt ) 
      ; DisplayAvgReal ( "imbalance        " , ImbalanceTot , TextCt ) 
      ; DisplayAvgInt  ( "length difference" , LengthDiffTot , TextCt ) 
      END (* IF *)  
    END DisplayTextMeasures 

; PROCEDURE SingleTextMeasures ( Value : TEXT ) 

  = VAR Depth : INTEGER := 0  
  ; VAR LeafCt : INTEGER := 0
  ; VAR Imbalance : REAL := 0.0
  ; VAR LengthDiff : INTEGER := 0 
  ; VAR LIterCt : INTEGER
  ; VAR Unused : INTEGER := 0 

  ; BEGIN 
      IF Value # NIL 
      THEN 
        TextUtils . MeasureText 
          ( Value , Depth , LeafCt , Imbalance , LengthDiff ) 

; IF TextClass . NewOps # NIL 
  THEN 
    LIterCt := TextClass . NewOps ^ [ TextClass . Op . Cat ] . CurIterCt 
  ; IF LIterCt > Depth  
    THEN
      Unused := 149
    END (* IF *) 
  END (* IF *) 

; IF Imbalance > 1.1 
  THEN
    Unused := 237
  END (* IF *) 
      END (* IF *) 
    END SingleTextMeasures 

; CONST WideMean = 32768
; CONST WideHalfInterval = 32768 

; PROCEDURE DenseChar ( ) : CHAR  

  = BEGIN 
      RETURN VAL ( RandV . integer ( 32 , 126 ) , CHAR )
    END DenseChar 

; PROCEDURE DenseWide ( ) : WIDECHAR  

  = BEGIN 
      RETURN 
        VAL 
          ( RandV . integer 
              ( WideMean - WideHalfInterval , WideMean + WideHalfInterval - 1 )
          , WIDECHAR
          )
    END DenseWide 

; PROCEDURE StopCollection ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      RTCollector . Disable ( ) 
    ; Wr . PutText 
        ( PWrT , "Garbage collection disabled for better timing." ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END StopCollection 

; PROCEDURE ResumeCollection ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . PutText ( PWrT , "Garbage collection enabled and triggered ... " ) 
    ; Wr . Flush ( PWrT ) 
    ; RTCollector . Enable ( ) 
    ; RTCollectorSRC . StartCollection ( ) 
    ; RTCollectorSRC . FinishCollection ( ) 
    ; Thread . Pause ( 2.0D0 ) 
      (* ^Hopefully, this will get weakRef cleanups all processed. *) 
    ; Wr . PutText ( PWrT , "Garbage collection completed." ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END ResumeCollection 

; PROCEDURE FillBase ( N : CARDINAL )

  = VAR LSs : INTEGER 
  ; VAR LC1 , LC2 : CHAR
  ; VAR LW1 , LW2 : WIDECHAR
  ; VAR LIntLower , LIntUpper : INTEGER 
  ; VAR LIsWide : BOOLEAN 
  ; VAR LOldText : TEXT 
  ; VAR LNewText : TEXT 
  ; VAR LCharsRef : REF ARRAY OF CHAR
  ; VAR LWidesRef : REF ARRAY OF WIDECHAR

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      ResetProgress ( ) 
    ; GFillCt := 0 
    ; GLastCheckpointCt := 0  
    ; GOldTexts [ 0 ] := ""
    ; GNewTexts [ 0 ] := "" 
    ; GStoredTextCt := 1 
    ; StopCollection ( ) 
    ; FOR RI := 1 TO N 
      DO 
        IF GStoredTextCt >= MaxTexts 
        THEN LSs := RandV . integer ( 1 , MaxTexts )   
        ELSE
          LSs := GStoredTextCt 
        ; INC ( GStoredTextCt ) 
        END (* IF *)
      ; LIsWide := RandV . integer ( 0 , 100000 ) > 95000
        (* Bias initial strings heavily to CHAR, since repeated operations
           will push results monotonically toward wide.
        *)  
      ; IF GBaseLength = 1 OR ( GBaseLength = 0 AND RandV . boolean ( ) ) 
        THEN (* Generate a singleton TEXT. *) 
          IF LIsWide 
          THEN 
            LW1 := DenseWide ( ) 
          ; IF GDoOld 
            THEN 
              TextClass . Old := TRUE
            ; StartTimingOld ( ) 
            ; LOldText := Text . FromWideChar ( LW1 )
            ; StopTimingOld ( ) 
            ; CheckText ( LOldText , "Fill-singleton-wide-old" , GTotalTextCt )
            END (* IF *) 
          ; IF GDoNew 
            THEN 
              TextClass . Old := FALSE
            ; StartTimingNew ( ) 
            ; LNewText := Text . FromWideChar ( LW1 )
            ; StopTimingNew ( ) 
            ; TextClass . Old := TRUE
            ; CheckText ( LNewText , "Fill-singleton-wide-new" , GTotalTextCt )
            END (* IF *) 
          ELSE (* NOT IsWide *)  
            LC1 := DenseChar ( ) 
          ; IF GDoOld 
            THEN 
              TextClass . Old := TRUE
            ; StartTimingOld ( ) 
            ; LOldText := Text . FromChar ( LC1 )
            ; StopTimingOld ( ) 
            ; CheckText ( LOldText , "Fill-singleton-char-old" , GTotalTextCt )
            END (* IF *) 
          ; IF GDoNew 
            THEN 
              TextClass . Old := FALSE
            ; StartTimingNew ( ) 
            ; LNewText := Text . FromChar ( LC1 )
            ; StopTimingNew ( ) 
            ; TextClass . Old := TRUE
            ; CheckText ( LNewText , "Fill-singleton-char-new" , GTotalTextCt )
            END (* IF *) 
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareTexts 
              ( LOldText , LNewText , "Fill-singleton" , GTotalTextCt )  
          END (* IF *) 
        ELSE (* Generate a range of chars. *) 
          IF LIsWide 
          THEN 
            LW1 := DenseWide ( ) 
          ; LIntLower := ORD ( LW1 )
          ; LIntLower 
              := MIN ( LIntLower , ORD ( LAST ( WIDECHAR ) ) - GBaseLength ) 
          ; LW1 := VAL ( LIntLower , WIDECHAR ) 
          ; IF GBaseLength > 0 
            THEN LIntUpper := GBaseLength 
            ELSE 
              LIntUpper 
                := RandV . integer 
                     ( LIntLower 
                     , MIN ( LIntLower + 25 , ORD ( LAST ( WIDECHAR ) ) ) 
                     ) 
            END (* IF *) 
          ; LW2 := VAL ( LIntUpper , WIDECHAR ) 
          ; IF LIntLower < LIntUpper 
            THEN
              LWidesRef 
                := NEW ( REF ARRAY OF WIDECHAR , LIntUpper - LIntLower )
            ; FOR RI := FIRST ( LWidesRef ^ ) TO LAST ( LWidesRef ^ ) 
              DO 
                LWidesRef ^ [ RI ] := LW1
              ; INC ( LW1 ) 
              END (* FOR *) 
            ; IF GDoOld 
              THEN 
                TextClass . Old := TRUE
              ; StartTimingOld ( ) 
              ; LOldText := Text . FromWideChars ( LWidesRef ^ )
              ; StopTimingOld ( ) 
              ; CheckText ( LOldText , "Fill-range-wide-old" , GTotalTextCt )
              END (* IF *) 
            ; IF GDoNew 
              THEN 
                TextClass . Old := FALSE
              ; StartTimingNew ( ) 
              ; LNewText := Text . FromWideChars ( LWidesRef ^ )
              ; StopTimingNew ( ) 
              ; TextClass . Old := TRUE
              ; CheckText ( LNewText , "Fill-range-wide-new" , GTotalTextCt )
              END (* IF *) 
            END (* IF *) 
          ELSE (* NOT IsWide *)  
            LC1 := DenseChar ( ) 
          ; LIntLower := ORD ( LC1 )
          ; LIntLower 
              := MIN ( LIntLower , ORD ( LAST ( CHAR ) ) - GBaseLength ) 
          ; LC1 := VAL ( LIntLower , CHAR ) 
          ; IF GBaseLength > 0 
            THEN LIntUpper := GBaseLength 
            ELSE 
              LIntUpper 
                := RandV . integer 
                     ( LIntLower 
                     , MIN ( LIntLower + 25 , 126 )
                     )
            END (* IF *) 
          ; LC2 := VAL ( LIntUpper , CHAR ) 
          ; IF LIntLower < LIntUpper 
            THEN
              LCharsRef 
                := NEW ( REF ARRAY OF CHAR , LIntUpper - LIntLower )
            ; FOR RI := FIRST ( LCharsRef ^ ) TO LAST ( LCharsRef ^ ) 
              DO 
                LCharsRef ^ [ RI ] := LC1
              ; INC ( LC1 ) 
              END (* FOR *) 
            ; IF GDoOld 
              THEN 
                TextClass . Old := TRUE
              ; StartTimingOld ( ) 
              ; LOldText := Text . FromChars ( LCharsRef ^ )
              ; StopTimingOld ( ) 
              ; CheckText ( LOldText , "Fill-range-char-old" , GTotalTextCt )
              END (* IF *) 
            ; IF GDoNew 
              THEN 
                TextClass . Old := FALSE
              ; StartTimingNew ( ) 
              ; LNewText := Text . FromChars ( LCharsRef ^ )
              ; StopTimingNew ( ) 
              ; TextClass . Old := TRUE
              ; CheckText ( LNewText , "Fill-range-char-new" , GTotalTextCt )
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareTexts 
              ( LOldText , LNewText , "Fill-range" , GTotalTextCt )  
          END (* IF *) 
        END (* IF *) 
      ; GOldTexts [ LSs ] := LOldText   
      ; GNewTexts [ LSs ] := LNewText   
      ; GTextNos [ LSs ] := GTotalTextCt 
      ; INC ( GTotalTextCt ) 
      ; NoteProgress ( (* VAR *) GFillCt ) 
      END (* FOR *)  
    ; ShowExactProgress ( GFillCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    ; ResumeCollection ( ) 
    END FillBase 

; PROCEDURE InnerCat 
    ( OldOp1 , OldOp2 , NewOp1 , NewOp2 : TEXT 
    ; VAR OldResult , NewResult : TEXT 
    ; TextNo1 , TextNo2 : INTEGER 
    ) 

  = BEGIN 
      IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( ) 
      ; OldResult := Text . Cat ( OldOp1 , OldOp2 )   
      ; StopTimingOld ( ) 
      ; CheckText ( OldResult , "Cat-old" , GTotalTextCt ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; NewResult := Text.Cat ( NewOp1 , NewOp2 )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      ; CheckText ( NewResult , "Cat-new" , GTotalTextCt ) 
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts ( OldOp1 , NewOp1 , "Cat-op1" , TextNo1 )  
      ; CompareTexts ( OldOp2 , NewOp2 , "Cat-op2" , TextNo2 )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareTexts ( OldResult , NewResult , "Cat" , GTotalTextCt ) 
      END (* IF *) 
    END InnerCat 

; PROCEDURE RandSs ( ) : CARDINAL 
  (* Prevent the leaf strings from overpredominating the Cat strings,
     early during Concatenations, which would result in too many trees 
     of very low leaf count.
  *)   

  = VAR LLo , LHi , LResult : INTEGER 

  ; BEGIN 
      LLo := MAX ( 0 , GFillCt - 1 - ( GStoredTextCt - GFillCt ) ) 
    ; LHi := GStoredTextCt - 1 
    ; LResult := RandV . integer ( LLo , LHi )  
    ; RETURN LResult 
    END RandSs 

; PROCEDURE DoCat ( Ss : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 , LOldResult : TEXT 
  ; VAR LNewOp1 , LNewOp2 , LNewResult : TEXT 

  ; BEGIN 
      LSs1  := RandSs ( ) 
    ; LSs2  := RandSs ( ) 
    ; LOldOp1 := GOldTexts [ LSs1 ] 
    ; LOldOp2 := GOldTexts [ LSs2 ] 
    ; LNewOp1 := GNewTexts [ LSs1 ] 
    ; LNewOp2 := GNewTexts [ LSs2 ] 
    ; InnerCat 
        ( LOldOp1 , LOldOp2 , LNewOp1 , LNewOp2 
        , LOldResult , LNewResult 
        , GTextNos [ LSs1 ] ,GTextNos [ LSs2 ] 
        ) 
    ; GOldTexts [ Ss ] := LOldResult
    ; GNewTexts [ Ss ] := LNewResult
    END DoCat 

; PROCEDURE DoSub ( Ss : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp , LOldResult : TEXT 
  ; VAR LNewOp , LNewResult : TEXT 
  ; VAR LLo , LOpLen , LLen : CARDINAL 

  ; BEGIN 
      LSs  := RandSs ( )    
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN LOpLen := Text . Length ( LOldOp ) 
      ELSIF GDoNew 
      THEN LOpLen := Text . Length ( LNewOp ) 
      ELSE LOpLen := 0 
      END (* IF *) 
    ; IF LOpLen > 0 
      THEN 
        LLo := RandV . integer ( 0 , LOpLen - 1 ) 
      ; LLen := RandV . integer ( 1 , LOpLen - LLo )
      (* No need to test out-of-bounds substrings, as the old Text.Sub
         seems to work, and the new one is unchanged in this regard.
      *) 
      ; IF GDoOld 
        THEN 
          TextClass . Old := TRUE 
        ; StartTimingOld ( ) 
        ; LOldResult := Text . Sub ( LOldOp , LLo , LLen )  
        ; StopTimingOld ( ) 
        ; CheckText ( LOldResult , "Sub-old" , GTotalTextCt ) 
        END (* IF *) 
      ; IF GDoNew 
        THEN 
          TextClass . Old := FALSE 
        ; StartTimingNew ( ) 
        ; LNewResult 
            := Text . Sub ( LNewOp , LLo , LLen )  
        ; StopTimingNew ( ) 
        ; TextClass . Old := TRUE
        ; CheckText ( LNewResult , "Sub-new" , GTotalTextCt ) 
        END (* IF *) 
      ; IF GDoCompareOperands 
        THEN 
          CompareTexts ( LOldOp , LNewOp , "Sub-op" , GTextNos [ LSs ] ) 
        END (* IF *) 
      ; IF GDoCompareResults 
        THEN 
          CompareTexts ( LOldResult , LNewResult , "Sub" , GTotalTextCt ) 
        END (* IF *) 
      ELSE 
        LOldResult := LOldOp 
      ; LNewResult := LNewOp 
      END (* IF *) 
    ; GOldTexts [ Ss ] := LOldResult
    ; GNewTexts [ Ss ] := LNewResult
    END DoSub 

; PROCEDURE RandOperations ( N : CARDINAL ; MinSs : CARDINAL ) 

  = VAR LN : INTEGER 
  ; VAR Unused : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      ResetProgress ( ) 
    ; GOperationCt := 0
    ; GLastCheckpointCt := 0  
    ; StopCollection ( ) 
    ; FOR RI := 1 TO N 
      DO
        IF GStoredTextCt >= MaxTexts 
        THEN 
          LN := RandV . integer ( MinSs , MaxTexts )   
        ELSE
          LN := GStoredTextCt 
        END (* IF *)
; IF RI > 50000 
  THEN 
    Unused := 492
  END 
      ; CASE RandV . integer ( 0 , 9 ) <* NOWARN *> 
        OF 0 .. 8 => DoCat ( LN ) 
        | 9 => DoSub ( LN )  
        END (* CASE *) 
      ; GTextNos [ LN ] := GTotalTextCt 
      ; INC ( GTotalTextCt ) 
      ; IF GStoredTextCt < MaxTexts 
        THEN 
          INC ( GStoredTextCt ) 
        END (* IF *)
      ; NoteProgress ( GOperationCt ) 
      ; IF RI MOD StatsInterval = 0 
        THEN
          ResumeCollection ( ) 
        ; PrintCheckpoint ( "operations" , GOperationCt ) 
        ; StopCollection ( ) 
        END (* IF *) 
      END (* FOR *) 
    ; ShowExactProgress ( GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    ; ResumeCollection ( ) 
    ; PrintCheckpoint ( "operations" , GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END RandOperations    

; PROCEDURE AsymCats ( N : CARDINAL ; MinSs : CARDINAL ; LToR : BOOLEAN ) 

  = VAR LN , LSs : INTEGER 
  ; VAR LCatCt : CARDINAL 
  ; VAR LLeafCt : INTEGER
  ; VAR LOldOp , LNewOp , LOldPartial , LNewPartial , LOldResult , LNewResult 
        : TEXT 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      ResetProgress ( ) 
    ; GOperationCt := 0
    ; GLastCheckpointCt := 0  
    ; LCatCt := 0  
    ; LLeafCt := 0 
    ; StopCollection ( ) 
    ; WHILE LCatCt < N (* Thru enough trees to give N Cat operations. *) 
      DO 
        IF GStoredTextCt >= MaxTexts 
        THEN 
          LN := RandV . integer ( MinSs , MaxTexts )   
        ELSE
          LN := GStoredTextCt 
        END (* IF *)
      ; LLeafCt := RandV . integer ( 20 , 500 )  
      ; LSs := RandV . integer ( 0 , GFillCt - 1 )   
      ; LOldPartial := GOldTexts [ LSs ] 
      ; LNewPartial := GNewTexts [ LSs ] 
      ; DEC ( LLeafCt ) 
      ; LOOP (* Thru additional leaves of one tree. *) 
          IF LLeafCt <= 0 OR LCatCt >= N 
          THEN
            GOldTexts [ LN ] := LOldPartial
          ; GNewTexts [ LN ] := LNewPartial
          ; EXIT 
          ELSE 
            LSs := RandV . integer ( 0 , GFillCt - 1 )   
          ; LOldOp := GOldTexts [ LSs ] 
          ; LNewOp := GNewTexts [ LSs ] 
          ; IF LToR 
            THEN 
              InnerCat 
                ( LOldPartial , LOldOp , LNewPartial , LNewOp 
                , LOldResult , LNewResult 
                , GTextNos [ LSs ] , GTextNos [ LSs ] 
                ) 
            ELSE 
              InnerCat 
                ( LOldOp , LOldPartial , LNewOp , LNewPartial 
                , LOldResult , LNewResult 
                , GTextNos [ LSs ] , GTextNos [ LSs ] 
                ) 
            END (* IF *) 
; SingleTextMeasures ( LNewResult ) 
          ; LOldPartial := LOldResult 
          ; LNewPartial := LNewResult 
          ; DEC ( LLeafCt ) 
          ; INC ( LCatCt ) 
          ; NoteProgress ( GOperationCt ) 
          ; IF LCatCt MOD StatsInterval = 0 
            THEN
              ResumeCollection ( ) 
            ; PrintCheckpoint ( "concatenations" , GOperationCt ) 
            ; StopCollection ( ) 
            END (* IF *) 
          END (* IF *)  
        END (* LOOP *) 
      ; GTextNos [ LN ] := GTotalTextCt 
      ; INC ( GTotalTextCt ) 
      ; IF GStoredTextCt < MaxTexts 
        THEN 
          INC ( GStoredTextCt ) 
        END (* IF *)
      END (* WHILE *) 
    ; ShowExactProgress ( GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    ; ResumeCollection ( ) 
    ; PrintCheckpoint ( "concatenations" , GOperationCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END AsymCats    

; PROCEDURE DoEmpty ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LOldResult , LNewResult : BOOLEAN  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( ) 
      ; LOldResult := Text . Empty ( LOldOp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . Empty ( LNewOp )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts ( LOldOp , LNewOp , "Empty-op" , GTextNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool ( LOldResult , LNewResult , "Empty" , GTextNos [ LSs ] )
      END (* IF *) 
    END DoEmpty

; PROCEDURE DoHasWideChars ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LOldResult , LNewResult : BOOLEAN  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( ) 
      ; LOldResult := Text . HasWideChars ( LOldOp ) 
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . HasWideChars ( LNewOp )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts 
          ( LOldOp , LNewOp , "HasWideChars-op" , GTextNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        (* CompareBool 
          ( LOldResult , LNewResult , "HasWideChars" , GTextNos [ LSs ] ) *) 
         (* ^Until we do some serious rework, only a FALSE from HasWideChars 
            is certain, so we can't check this right now.  Instead, use
            the weaker checks below, on each of the TEXTs. 
         *) 
        IF LOldResult 
        THEN
          (* The old HasWideChars is buggy.  It can report true when the
             internal representation is capable of holding a wide character,
             even though none is present.  If there is a substring, it can
             be even worse.  If the base string of the substring is capable
             of containing a wide char, then it will report TRUE.  So in
             this case, we just keep quiet. 
           *) 
        ELSE 
          CheckNoWideChars ( LOldOp , "HasWidechars-old" , GTextNos [ LSs ] ) 
        END (* IF *)  
      ; IF LNewResult 
        THEN
          CheckHasWideChars ( LNewOp , "HasWidechars-new" , GTextNos [ LSs ] ) 
        ELSE 
          CheckNoWideChars ( LNewOp , "HasWidechars-new" , GTextNos [ LSs ] ) 
        END (* IF *)  
      END (* IF *) 
    END DoHasWideChars  

; PROCEDURE DoLength ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LOldResult , LNewResult : INTEGER  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( )
      ; LOldResult := Text . Length ( LOldOp )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . Length ( LNewOp )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts ( LOldOp , LNewOp , "Length-op" , GTextNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareInt ( LOldResult , LNewResult , "Length" , GTextNos [ LSs ] )
      END (* IF *) 
    END DoLength

; PROCEDURE DoHash ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LOldResult , LNewResult : INTEGER  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( )
      ; LOldResult := Text . Hash ( LOldOp )  
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . Hash ( LNewOp )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts ( LOldOp , LNewOp , "Hash-op" , GTextNos [ LSs ] ) 
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareInt ( LOldResult , LNewResult , "Hash" , GTextNos [ LSs ] )
      END (* IF *) 
    END DoHash

; PROCEDURE DoEqual ( MinSs : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 : TEXT 
  ; VAR LNewOp1 , LNewOp2 : TEXT 
  ; VAR LOldResult , LNewResult : BOOLEAN 

  ; BEGIN 
      LSs1  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LSs2  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp1 := GOldTexts [ LSs1 ] 
    ; LOldOp2 := GOldTexts [ LSs2 ] 
    ; LNewOp1 := GNewTexts [ LSs1 ] 
    ; LNewOp2 := GNewTexts [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( ) 
      ; LOldResult := Text . Equal ( LOldOp1 , LOldOp2 )   
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . Equal ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts ( LOldOp1 , LNewOp1 , "Equal-op1" , GTextNos [ LSs1 ] )  
      ; CompareTexts ( LOldOp2 , LNewOp2 , "Equal-op2" , GTextNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareBool 
          ( LOldResult , LNewResult , "Equal" 
          , GTextNos [ LSs1 ] , GTextNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoEqual

; PROCEDURE DoCompare ( MinSs : CARDINAL ) 

  = VAR LSs1 , LSs2 : CARDINAL 
  ; VAR LOldOp1 , LOldOp2 : TEXT 
  ; VAR LNewOp1 , LNewOp2 : TEXT 
  ; VAR LOldResult , LNewResult : [ - 1 .. 1 ]  

  ; BEGIN 
      LSs1  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LSs2  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp1 := GOldTexts [ LSs1 ] 
    ; LOldOp2 := GOldTexts [ LSs2 ] 
    ; LNewOp1 := GNewTexts [ LSs1 ] 
    ; LNewOp2 := GNewTexts [ LSs2 ] 
    ; IF GDoOld 
      THEN 
        TextClass . Old := TRUE 
      ; StartTimingOld ( ) 
      ; LOldResult := Text . Compare ( LOldOp1 , LOldOp2 )   
      ; StopTimingOld ( ) 
      END (* IF *) 
    ; IF GDoNew 
      THEN 
        TextClass . Old := FALSE 
      ; StartTimingNew ( ) 
      ; LNewResult := Text . Compare ( LNewOp1 , LNewOp2 )  
      ; StopTimingNew ( ) 
      ; TextClass . Old := TRUE
      END (* IF *) 
    ; IF GDoCompareOperands 
      THEN 
        CompareTexts 
          ( LOldOp1 , LNewOp1 , "Compare-op1" , GTextNos [ LSs1 ] )  
      ; CompareTexts 
          ( LOldOp2 , LNewOp2 , "Compare-op2" , GTextNos [ LSs2 ] )  
      END (* IF *) 
    ; IF GDoCompareResults 
      THEN 
        CompareInt   
          ( LOldResult , LNewResult , "Compare" 
          , GTextNos [ LSs1 ] , GTextNos [ LSs2 ] 
          ) 
      END (* IF *) 
    END DoCompare

; PROCEDURE DoGetChar ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LLo , LOpLen : CARDINAL
  ; VAR LOldChar , LNewChar : CHAR  
  ; VAR LOldWide , LNewWide : WIDECHAR  
  ; VAR LIsWide : BOOLEAN := FALSE  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        LOpLen := Text . Length ( LOldOp ) 
      ; LIsWide := Text . HasWideChars ( LOldOp )
      ELSIF GDoNew
      THEN 
        LOpLen := Text . Length ( LNewOp ) 
      ; LIsWide := Text . HasWideChars ( LNewOp )
      END (* IF *) 
    ; IF LOpLen >= 1 
      THEN 
        LLo := RandV . integer ( 0 , LOpLen - 1 ) 
      ; IF LIsWide
        THEN (* Test GetWideChars *) 
          IF GDoOld 
          THEN 
            TextClass . Old := TRUE 
          ; StartTimingOld ( ) 
          ; LOldWide := Text . GetWideChar ( LOldOp , LLo )  
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; StartTimingNew ( ) 
          ; LNewWide := Text . GetWideChar ( LNewOp , LLo )  
          ; StopTimingNew ( ) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareInt 
              ( ORD ( LOldWide ) , ORD ( LNewWide ) 
              , "GetWideChar" , GTextNos [ LSs ] 
              ) 
          END (* IF *) 
        ELSE (* No wide chars.  Test GetChars. *) 
          IF GDoOld 
          THEN 
            TextClass . Old := TRUE 
          ; StartTimingOld ( ) 
          ; LOldChar := Text . GetChar ( LOldOp , LLo )  
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; StartTimingNew ( ) 
          ; LNewChar := Text . GetChar ( LNewOp , LLo )  
          ; StopTimingNew ( ) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareInt 
              ( ORD ( LOldChar ) , ORD ( LNewChar ) 
              , "GetChar" , GTextNos [ LSs ] 
              ) 
          END (* IF *) 
        END (* IF *) 
      ; IF GDoCompareOperands 
        THEN 
          CompareTexts 
            ( LOldOp , LNewOp , "Get[Wide]Chars-op" , GTextNos [ LSs ] ) 
        END (* IF *) 
      END (* IF *) 
    END DoGetChar 

; PROCEDURE DoSetChars ( MinSs : CARDINAL ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LLo , LOpLen , LLen : CARDINAL
  ; VAR LOldChars , LNewChars : REF ARRAY OF CHAR  
  ; VAR LOldWides , LNewWides : REF ARRAY OF WIDECHAR  
  ; VAR LIsWide : BOOLEAN := FALSE  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN 
        LOpLen := Text . Length ( LOldOp ) 
      ; LIsWide := Text . HasWideChars ( LOldOp )
      ELSIF GDoNew
      THEN 
        LOpLen := Text . Length ( LNewOp ) 
      ; LIsWide := Text . HasWideChars ( LNewOp )
      END (* IF *) 
    ; IF LOpLen >= 2 
      THEN 
        LLo := RandV . integer ( 0 , LOpLen - 1 ) 
      ; LLen := LOpLen - LLo 
      (* Do we need to test out-of-bounds substrings? *)  
      ; IF LIsWide  
        THEN (* Test SetWideChars *) 
          LOldWides := NEW ( REF ARRAY OF WIDECHAR , LLen ) 
        ; LNewWides := NEW ( REF ARRAY OF WIDECHAR , LLen ) 
        ; IF GDoOld 
          THEN 
            TextClass . Old := TRUE 
          ; StartTimingOld ( ) 
          ; Text . SetWideChars ( LOldWides ^ , LOldOp , LLo )  
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; StartTimingNew ( ) 
          ; Text . SetWideChars ( LNewWides ^ , LNewOp , LLo )  
          ; StopTimingNew ( ) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareWides 
              ( LOldWides^ , LNewWides ^ , "SetWideChars" , GTextNos [ LSs ] )
          END (* IF *) 
        ELSE (* No wide chars.  Test SetChars. *) 
          LOldChars := NEW ( REF ARRAY OF CHAR , LLen ) 
        ; LNewChars := NEW ( REF ARRAY OF CHAR , LLen ) 
        ; IF GDoOld 
          THEN 
            TextClass . Old := TRUE 
          ; StartTimingOld ( ) 
          ; Text . SetChars ( LOldChars ^ , LOldOp , LLo )  
          ; StopTimingOld ( ) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; StartTimingNew ( ) 
          ; Text . SetChars ( LNewChars ^ , LNewOp , LLo )  
          ; StopTimingNew ( ) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareChars 
              ( LOldChars^ , LNewChars ^ , "SetChars" , GTextNos [ LSs ] ) 
          END (* IF *) 
        END (* IF *) 
      ; IF GDoCompareOperands 
        THEN 
          CompareTexts 
            ( LOldOp , LNewOp , "Set[Wide]Chars-op" , GTextNos [ LSs ] ) 
        END (* IF *) 
      END (* IF *) 
    END DoSetChars 

; PROCEDURE DoFindChar 
    ( Msg : TEXT ; MinSs : CARDINAL ; Present , LToR : BOOLEAN ) 

  = VAR LSs : CARDINAL 
  ; VAR LOldOp : TEXT 
  ; VAR LNewOp : TEXT 
  ; VAR LLo , LOpLen , LCharPos : CARDINAL 
  ; VAR LChar : CHAR := '\000'  
  ; VAR LWide : WIDECHAR 
  ; VAR LOldFoundPos , LNewFoundPos : INTEGER  
  ; VAR LIsWide : BOOLEAN := FALSE  

  ; BEGIN 
      LSs  := RandV . integer ( MinSs , GStoredTextCt - 1 )   
    ; LOldOp := GOldTexts [ LSs ] 
    ; LNewOp := GNewTexts [ LSs ] 
    ; IF GDoOld 
      THEN LOpLen := Text . Length ( LOldOp )  
      ELSIF GDoNew  
      THEN LOpLen := Text . Length ( LNewOp )  
      END (* IF *) 
    ; IF LOpLen >= 2 
      THEN 
        LLo := RandV . integer ( 0 , LOpLen - 1 ) 
      ; LCharPos := RandV . integer ( LLo , LOpLen - 1 ) 
      ; IF LIsWide 
        THEN (* Test FindWideChar *)
          IF NOT Present 
          THEN LWide := VAL ( 0 , WIDECHAR )  
          ELSIF GDoOld 
          THEN LWide := Text . GetWideChar ( LOldOp , LCharPos ) 
          ELSIF GDoNew 
          THEN LWide := Text . GetWideChar ( LNewOp , LCharPos ) 
          END (* IF *) 
        ; IF GDoOld 
          THEN
            TextClass . Old := TRUE 
          ; IF LToR THEN  
              StartTimingOld ( ) 
            ; LOldFoundPos := Text . FindWideChar ( LOldOp , LWide , LLo )  
            ; StopTimingOld ( ) 
            ELSE 
              StartTimingOld ( ) 
            ; LOldFoundPos := Text . FindWideCharR ( LOldOp , LWide , LLo )  
            ; StopTimingOld ( ) 
            END (* IF *) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; IF LToR THEN  
              StartTimingNew ( ) 
            ; LNewFoundPos := Text . FindWideChar ( LNewOp , LWide , LLo )  
            ; StopTimingNew ( ) 
            ELSE 
              StartTimingNew ( ) 
            ; LNewFoundPos := Text . FindWideCharR ( LNewOp , LWide , LLo )  
            ; StopTimingNew ( ) 
            END (* IF *) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareInt 
              ( LOldFoundPos , LNewFoundPos 
              , "FindWideChar" , GTextNos [ LSs ] 
              ) 
          END (* IF *) 
        ELSE (* No wide chars.  Test FindChar. *) 
          IF NOT Present 
          THEN LChar := '\000' 
          ELSIF GDoOld 
          THEN LChar := Text . GetChar ( LOldOp , LCharPos ) 
          ELSIF GDoNew 
          THEN LChar := Text . GetChar ( LNewOp , LCharPos ) 
          END (* IF *) 
        ; IF GDoOld 
          THEN 
            TextClass . Old := TRUE 
          ; IF LToR THEN  
              StartTimingOld ( ) 
            ; LOldFoundPos := Text . FindChar ( LOldOp , LChar , LLo )  
            ; StopTimingOld ( ) 
            ELSE 
              StartTimingOld ( ) 
            ; LOldFoundPos := Text . FindCharR ( LOldOp , LChar , LLo )  
            ; StopTimingOld ( ) 
            END (* IF *) 
          END (* IF *) 
        ; IF GDoNew 
          THEN 
            TextClass . Old := FALSE 
          ; IF LToR THEN  
              StartTimingNew ( ) 
            ; LNewFoundPos := Text . FindChar ( LNewOp , LChar , LLo )  
            ; StopTimingNew ( ) 
            ELSE 
              StartTimingNew ( ) 
            ; LNewFoundPos := Text . FindCharR ( LNewOp , LChar , LLo )  
            ; StopTimingNew ( ) 
            END (* IF *) 
          ; TextClass . Old := TRUE
          END (* IF *) 
        ; IF GDoCompareResults 
          THEN 
            CompareInt 
              ( LOldFoundPos , LNewFoundPos , Msg , GTextNos [ LSs ] ) 
          END (* IF *) 
        END (* IF *) 
      ; IF GDoCompareOperands 
        THEN 
          CompareTexts 
            ( LOldOp , LNewOp , "Find[Wide]Chars-op" , GTextNos [ LSs ] ) 
        END (* IF *) 
      END (* IF *) 
    END DoFindChar 

; PROCEDURE Queries ( N : CARDINAL ; MinSs : CARDINAL ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      ResetProgress ( ) 
    ; GTestCt := 0 
    ; GLastCheckpointCt := 0  
    ; StopCollection ( ) 
    ; FOR RI := 1 TO N 
      DO
        CASE RandV . integer ( 0 , 11 ) <* NOWARN *> 
        OF 0 => DoEmpty ( MinSs ) 
        | 1 => DoLength ( MinSs )  
        | 2 => DoHash ( MinSs )  
        | 3 => DoHasWideChars ( MinSs )
        | 4 => DoEqual ( MinSs )  
        | 5 => DoCompare ( MinSs )  
        | 6 => DoGetChar ( MinSs )  
        | 7 => DoSetChars ( MinSs )  
        | 8 
        => DoFindChar 
             ( "FindChar, present" , MinSs , Present := TRUE , LToR := TRUE ) 
        | 9 
        => DoFindChar 
             ( "FindCharR, present" , MinSs , Present := TRUE , LToR := FALSE ) 
        | 10 
        => DoFindChar 
             ( "FindChar, absent" , MinSs , Present := FALSE , LToR := TRUE ) 
        | 11 
        => DoFindChar 
             ( "FindCharR, absent" , MinSs , Present := FALSE , LToR := FALSE )
        END (* CASE *) 
      ; NoteProgress ( GTestCt ) 
      ; IF RI MOD StatsInterval = 0 
        THEN
          ResumeCollection ( ) 
        ; PrintCheckpoint ( "queries" , GTestCt ) 
        ; StopCollection ( ) 
        END (* IF *) 
      END (* FOR *) 
    ; ShowExactProgress ( GTestCt ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    ; ResumeCollection ( ) 
    ; IF N MOD StatsInterval # 0 
      THEN 
        PrintCheckpoint ( "queries" , GTestCt ) 
      END (* IF *) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . Flush ( PWrT ) 
    END Queries    

; PROCEDURE Work ( ) 

  = VAR OpString : TEXT 

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
      ELSIF NOT ( GDoOld OR GDoNew ) 
      THEN  
        WL ( "No operations specified." ) 
      ; Wr . Flush ( PWrT ) 
      ELSE 
        IF NOT ( GDoOld AND GDoNew )
        THEN 
          GDoCompareOperands := FALSE 
        ; GDoCompareResults := FALSE 
        END (*  IF *)  
      ; Wr . PutText ( PWrT , "Estimating overhead of timing." ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      ; EstimateTimingOverhead ( ) 

      ; TextClass . AllocOps ( TextClass . OldOps ) 
      ; TextClass . AllocOps ( TextClass . NewOps ) 
      ; TextClass . AllocObjs ( TextClass . OldObjs ) 
      ; TextClass . AllocObjs ( TextClass . NewObjs ) 

      ; TextClass . AllocOps ( GOldOps ) 
      ; TextClass . AllocOps ( GNewOps ) 
      ; TextClass . AllocObjs ( GOldObjs ) 
      ; TextClass . AllocObjs ( GNewObjs ) 

      ; TextClass . Old := TRUE
      ; TextClass . CollectStats := FALSE 

      ; IF WrT # PWrT 
        THEN 
          Wr . PutText 
            ( WrT , "Generating initial leaf strings." )
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT ) 
        END (* IF *) 
      ; Wr . PutText 
          ( PWrT , "Generating initial leaf strings." ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      ; FillBase ( BaseTextCt ) 
      ; Wr . PutText ( WrT , Wr . EOL )
      ; RTCollector . Collect ( ) 
 
      ; IF GDoUnbal 
        THEN OpString := "Highly unbalanced concatenations."
        ELSE OpString := "Operations that produce new strings." 
        END (* IF *) 
      ; IF WrT # PWrT 
        THEN 
          Wr . PutText ( WrT , OpString ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT ) 
        END (* IF *) 
      ; Wr . PutText ( PWrT , OpString ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      ; IF GDoUnbal 
        THEN AsymCats ( OperationCt , BaseTextCt , GDoLToR ) 
        ELSE RandOperations ( OperationCt , BaseTextCt ) 
        END (* IF *) 

      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "Up through all operations:   " ) 
      ; DisplayOps ( GOldOps , GNewOps )
      ; DisplayObjs ( GOldObjs , GNewObjs )
      ; INC ( GTotalCt , GOperationCt )

      ; IF WrT # PWrT 
        THEN 
          Wr . PutText ( WrT , "Queries on strings." ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . Flush ( WrT ) 
        END (* IF *) 
      ; Wr . PutText ( PWrT , "Queries on strings." ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      ; IF GDoUnbal 
        THEN 
          Queries ( QueryCt , GFillCt ) 
        ELSE 
          Queries ( QueryCt , 0 ) 
        END (* IF *) 

      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "For the entire run:   " ) 
      ; DisplayOps ( GOldOps , GNewOps )
      ; DisplayObjs ( GOldObjs , GNewObjs )

      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText 
          ( WrT , "------- Concatenation tree statistics: ------------------" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "For old texts: ----------------------" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; IF GDoUnbal 
        THEN 
          DisplayTextMeasures 
            ( SUBARRAY ( GOldTexts , GFillCt , GStoredTextCt - GFillCt ) ) 
        ELSE 
          DisplayTextMeasures ( SUBARRAY ( GOldTexts , 0 , GStoredTextCt ) ) 
        END (* IF *) 
      ; Wr . PutText ( WrT , "For new texts: ----------------------" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; IF GDoUnbal 
        THEN 
          DisplayTextMeasures 
            ( SUBARRAY ( GNewTexts , GFillCt , GStoredTextCt - GFillCt ) ) 
        ELSE 
          DisplayTextMeasures ( SUBARRAY ( GNewTexts , 0 , GStoredTextCt ) ) 
        END (* IF *) 
      ; Wr . PutText 
          ( WrT , "---------------------------------------------------------" ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; INC ( GTotalCt , GTestCt ) 
      ; Wr . PutText ( WrT , "Total tests failed:   " ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( GFailureCt ) , 7 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , "Total tests executed: " ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( GTotalCt ) , 7 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; Wr . PutText 
          ( WrT , "Estimated seconds of instrumentation overhead per call: " ) 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GTimingOverheadTick ) ) 
      ; Wr . PutText ( WrT , ", using Time: " ) 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GTimingOverheadTime ) ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; Wr . PutText ( WrT , "Seconds spent in old Text procedures: " ) 
      ; GOldTextsTick 
          := GOldTextsTick 
             - FLOAT ( GOldTimedCt , LONGREAL ) * GTimingOverheadTick 
      ; GOldTextsTime 
          := GOldTextsTime 
             - FLOAT ( GOldTimedCt , LONGREAL ) * GTimingOverheadTime 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GOldTextsTick ) ) 
      ; Wr . PutText ( WrT , ", using Time: " ) 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GOldTextsTime ) ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; Wr . PutText ( WrT , "Seconds spent in new Text procedures: " ) 
      ; GNewTextsTick 
          := GNewTextsTick 
             - FLOAT ( GNewTimedCt , LONGREAL ) * GTimingOverheadTick 
      ; GNewTextsTime 
          := GNewTextsTime 
             - FLOAT ( GNewTimedCt , LONGREAL ) * GTimingOverheadTime 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GNewTextsTick ) ) 
      ; Wr . PutText ( WrT , ", using Time: " ) 
      ; Wr . PutText ( WrT , Fmt . LongReal ( GNewTextsTime ) ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . Flush ( WrT ) 
      ; Wr . PutText ( PWrT , Wr . EOL ) 
      ; Wr . Flush ( PWrT ) 
      END (* IF *) 
    END Work  

; BEGIN 
    TextClass . InitInstrumentation ( ) 
  ; Work ( ) 
  END Test 
. 
