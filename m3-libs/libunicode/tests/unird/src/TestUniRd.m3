(* Test program for UniRd. *) 
MODULE TestUniRd EXPORTS Main 

; IMPORT FileRd
; IMPORT FileWr 
; IMPORT Fmt
; IMPORT OSError 
; IMPORT Pipe  
; IMPORT Rd 
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; FROM UniCodec IMPORT Widechar  
; FROM UniEncoding IMPORT Encoding
; IMPORT UniRd 
; FROM UniRd IMPORT Range , RangeInfo 
; IMPORT UniWr 
; IMPORT Word 
; IMPORT Wr 

; CONST WchNull = VAL ( 0 , Widechar ) 

; CONST WchCR = VAL ( ORD ( '\r' ) , Widechar ) 
; CONST WchLF = VAL ( ORD ( '\n' ) , Widechar ) 
; CONST WchFF = VAL ( ORD ( W'\f' ) , Widechar ) 
; CONST WchVT = VAL ( ORD ( W'\t' ) , Widechar ) 
; CONST WchNEL = VAL ( ORD ( W'\X0085' ) , Widechar ) 
; CONST WchLS = VAL ( ORD ( W'\X2028' ) , Widechar ) 
; CONST WchPS = VAL ( ORD ( W'\X2029' ) , Widechar ) 

; CONST WchMax16 = VAL ( 16_FFFF , Widechar ) 

; CONST WchA = VAL ( ORD ( W'A' ) , Widechar ) 
; CONST WchB = VAL ( ORD ( W'B' ) , Widechar ) 
; CONST WchC = VAL ( ORD ( W'C' ) , Widechar ) 
; CONST WchD = VAL ( ORD ( W'D' ) , Widechar ) 
; CONST WchE = VAL ( ORD ( W'E' ) , Widechar ) 
; CONST WchF = VAL ( ORD ( W'F' ) , Widechar ) 
; CONST WchG = VAL ( ORD ( W'G' ) , Widechar ) 
; CONST WchH = VAL ( ORD ( W'H' ) , Widechar ) 
; CONST WchI = VAL ( ORD ( W'I' ) , Widechar ) 
; CONST WchJ = VAL ( ORD ( W'J' ) , Widechar ) 
; CONST WchK = VAL ( ORD ( W'K' ) , Widechar ) 
; CONST WchL = VAL ( ORD ( W'L' ) , Widechar ) 
; CONST WchM = VAL ( ORD ( W'M' ) , Widechar ) 
; CONST WchN = VAL ( ORD ( W'N' ) , Widechar ) 
; CONST WchO = VAL ( ORD ( W'O' ) , Widechar ) 
; CONST WchP = VAL ( ORD ( W'P' ) , Widechar ) 
; CONST WchQ = VAL ( ORD ( W'Q' ) , Widechar ) 
; CONST WchR = VAL ( ORD ( W'R' ) , Widechar ) 
; CONST WchS = VAL ( ORD ( W'S' ) , Widechar ) 
; CONST WchT = VAL ( ORD ( W'T' ) , Widechar ) 
; CONST WchU = VAL ( ORD ( W'U' ) , Widechar ) 
; CONST WchV = VAL ( ORD ( W'V' ) , Widechar ) 
; CONST WchW = VAL ( ORD ( W'W' ) , Widechar ) 
; CONST WchX = VAL ( ORD ( W'X' ) , Widechar ) 
; CONST WchY = VAL ( ORD ( W'Y' ) , Widechar ) 
; CONST WchZ = VAL ( ORD ( W'Z' ) , Widechar ) 

; TYPE KindTyp 
    = { EOF 
      , CharsReady 
      , GetWideChar 
      , GetWideSub 
      , GetSub
      , GetWideSubLine 
      , GetSubLine 
      , GetText 
      , GetLine 
      , UnGetCodePoint 
      , Index 
      , AvgBytesPerChar 
      , Total 
      } 

; PROCEDURE KindImage ( K : KindTyp ) : TEXT 

  = BEGIN 
      CASE K 
      OF KindTyp . EOF => RETURN "EOF" 
      | KindTyp . CharsReady => RETURN "CharsReady" 
      | KindTyp . GetWideChar => RETURN "GetWideChar" 
      | KindTyp . GetWideSub => RETURN "GetWideSub" 
      | KindTyp . GetSub => RETURN "GetSub"
      | KindTyp . GetWideSubLine => RETURN "GetWideSubLine" 
      | KindTyp . GetSubLine => RETURN "GetSubLine" 
      | KindTyp . GetText => RETURN "GetText" 
      | KindTyp . GetLine => RETURN "GetLine" 
      | KindTyp . UnGetCodePoint => RETURN "UnGetCodePoint" 
      | KindTyp . Index => RETURN "Index" 
      | KindTyp . AvgBytesPerChar => RETURN "AvgBytesPerChar" 
      | KindTyp . Total => RETURN "Total" 
      END (* CASE *) 
    END KindImage 

; TYPE ArrOutcomeTyp = ARRAY BOOLEAN OF CARDINAL 
; CONST ZeroOutcomes = ArrOutcomeTyp { 0 , 0 } 

; TYPE CountsTyp = ARRAY KindTyp OF ArrOutcomeTyp

; VAR Counts := CountsTyp { ZeroOutcomes , .. }  

; CONST C1 = 18
; CONST C2 = 10 

; PROCEDURE PadLeft ( text : TEXT ; length : CARDINAL ) : TEXT 

  = BEGIN 
      RETURN Fmt . Pad ( text , length , align := Fmt . Align . Left ) 
    END PadLeft 

; PROCEDURE DumpCounts ( ) 

  = BEGIN 
      WL ( PadLeft ( "Tests of" , C1 ) 
           & Fmt . Pad ( "Succeeded" , C2 ) 
           & Fmt . Pad ( "Failed" , C2 ) 
           )  
    ; FOR RK := FIRST ( KindTyp ) TO LAST ( KindTyp ) 
      DO WITH WRow = Counts [ RK ] 
        DO IF WRow [ FALSE ] # 0 OR WRow [ TRUE ] # 0 
              OR RK = KindTyp . Total 
          THEN 
            WL ( PadLeft ( KindImage ( RK ) , C1 ) 
                 & Fmt . Pad ( IntImage ( WRow [ TRUE ] ) , C2 ) 
                 & Fmt . Pad ( IntImage ( WRow [ FALSE ] ) , C2 ) 
               ) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    ; IF Counts [ KindTyp . Total , FALSE ] = 0 
      THEN WL ( "All tests passed." ) 
      ELSE WL ( "################# Some tests FAILED! #################" )
      END (* IF *) 
    END DumpCounts 

; PROCEDURE IntImage ( I : INTEGER ) : TEXT 

  = BEGIN 
      RETURN Fmt . Int ( I ) 
    END IntImage 

; PROCEDURE BoolImage ( B : BOOLEAN ) : TEXT 

  = BEGIN 
      IF B 
      THEN RETURN "TRUE" 
      ELSE RETURN "FALSE"
      END (* IF *) 
    END BoolImage 

; PROCEDURE WchImage ( Wch : Widechar ) : TEXT 

  = VAR T1 , T2 : TEXT 

  ; BEGIN 
      T1 := "16_" & Fmt . Int ( ORD ( Wch ) , 16 )  
    ; IF ORD ( FIRST ( CHAR ) ) <= ORD ( Wch ) 
         AND ORD ( Wch ) <= ORD ( LAST ( CHAR ) ) 
      THEN 
        T2 := " (W\'" & Fmt . Char ( VAL ( ORD ( Wch ) , CHAR ) ) & "\')"
      ELSE T2 := ""
      END (* IF *) 
    ; RETURN T1 & T2   
    END WchImage 

; PROCEDURE ChImage ( Ch : CHAR ) : TEXT 

  = VAR T1 , T2 : TEXT 

  ; BEGIN 
      T1 := "16_" & Fmt . Int ( ORD ( Ch ) , 16 )  
    ; T2 := " (\'" & Fmt . Char ( VAL ( ORD ( Ch ) , CHAR ) ) & "\')"
    ; RETURN T1 & T2   
    END ChImage 

; PROCEDURE RangeInfoImage ( Info : RangeInfo ) : TEXT 

  = BEGIN 
      RETURN "Range(RangeInfo{" 
             & WchImage ( Info . Wch ) 
             & ","
             & IntImage ( Info . Location ) 
             & "})"
    END RangeInfoImage 

; PROCEDURE WL ( L : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( Stdio . stdout , L ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END WL 

; VAR GWrFile , GRdFile : Pipe . T 
; VAR GWr : Wr . T 
; VAR GRd : Rd . T 
; VAR GRdStream : UniRd . T 
; VAR GWrStream : UniWr . T 
; VAR GEnc : Encoding 

; PROCEDURE OpenPipe ( Enc : Encoding ) 

  = <* FATAL OSError . E *> 
    BEGIN 
      Pipe . Open ( (*VAR*) GRdFile , GWrFile ) 
    ; GWr := NEW ( FileWr . T ) . init ( GWrFile ) 
    ; GWrStream := UniWr . New ( GWr , Enc ) 
    ; GRd := NEW ( FileRd . T ) . init ( GRdFile ) 
    ; GRdStream := UniRd . New ( GRd , Enc ) 
    END OpenPipe 

; PROCEDURE WriteClose ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . Close ( GWr )
    END WriteClose

; PROCEDURE ReadClose ( ) 

  = <* FATAL Thread . Alerted , Rd . Failure *> 
    BEGIN 
      Rd . Close ( GRd )
    END ReadClose

; PROCEDURE WriteChar ( Wch : Widechar ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      UniWr . PutWideChar ( GWrStream , Wch ) 
    ; Wr . Flush ( UniWr . Sink ( GWrStream ) ) 
    END WriteChar 

; PROCEDURE WriteChars 
    ( Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull ) 

  = PROCEDURE WOne ( Wch : Widechar ) 

    = BEGIN 
        IF Wch # WchNull 
        THEN WriteChar ( Wch ) 
        END  (* IF *) 
      END WOne 

  ; BEGIN (* WriteChars. *) 
      WOne ( Wch1 )
    ; WOne ( Wch2 )
    ; WOne ( Wch3 )
    ; WOne ( Wch4 )
    ; WOne ( Wch5 )
    ; WOne ( Wch6 )
    END WriteChars 

; PROCEDURE EOF ( ) : BOOLEAN 

  = BEGIN 
(* This is tricky.  Our pipe is intermittent, and we can't distinguish
   EOF from just no chars ready, in any normal way.  But we know EOF will
   only happen when we close GWr. *) 
      RETURN Wr . Closed ( GWr ) 
    END EOF 

; PROCEDURE MakeCRLFPostponed ( ) 

  = VAR LCt : CARDINAL 
  ; LBuff : ARRAY [ 0 .. 0 ] OF Widechar 

  ; <* FATAL Thread . Alerted , Rd . Failure *> 
    BEGIN 
      WriteChars ( WchCR , WchLF ) 
    ; LCt := UniRd . GetWideSubLine ( GRdStream , LBuff ) 
    ; <* ASSERT LCt = 0 *> 
    END MakeCRLFPostponed 

; PROCEDURE MakePostponed ( Wch : Widechar ) 
  (* PRE: Wch # WchLF. *) 

  = <* FATAL Thread . Alerted , Rd . EndOfFile , Rd . Failure *> 
    BEGIN 
      WriteChars ( WchCR , Wch ) 
    ; EVAL UniRd . GetWideChar ( GRdStream ) 
    END MakePostponed 

; PROCEDURE ExpectEOF 
    ( FKind : KindTyp ; Expected : BOOLEAN := TRUE ) 

  = VAR LSucceeded : BOOLEAN 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *)  
    ; IF EOF ( )  
      THEN
        IF NOT Expected 
        THEN 
          WL ( "UniRd.EOF is TRUE, but expected FALSE." ) 
        ; LSucceeded := FALSE 
        END  (* IF *) 
      ELSE 
        IF Expected 
        THEN 
          WL ( "UniRd.EOF is FALSE, but expected TRUE." ) 
        ; LSucceeded := FALSE 
        END  (* IF *) 
      END (* IF *) 
    ; INC ( Counts [ FKind , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectEOF 

(* Tests of GetWideChar. *) 

; PROCEDURE ExpectGetWideChar 
    ( ExpWch : Widechar ; ExpRaiseEndOfFile := FALSE ) 
  : BOOLEAN (* Success *) 

  = VAR LOldIndex , LNewIndex : Word . T 
  ; VAR LConsumed : CARDINAL 
  ; VAR LWch : Widechar 
  ; VAR LSucceeded : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN 
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; TRY 
        LWch := UniRd . GetWideChar ( GRdStream ) 
      EXCEPT 
        Rd . EndOfFile 
        => IF NOT ExpRaiseEndOfFile 
           THEN 
             WL ( "GetWideChar raised Rd.EndOfFile, when not expected." ) 
           ; LSucceeded := FALSE 
           END (* IF *) 
        ; INC ( Counts [ KindTyp . GetWideChar , LSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
        ; Wr . Flush ( Stdio . stdout ) 
        ; RETURN LSucceeded 
      END (* EXCEPT *) 
    ; IF LWch # ExpWch 
      THEN 
        WL ( "Got " & WchImage ( LWch )  
             & ", but expected " & WchImage ( ExpWch ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # 1 
      THEN 
        WL ( "GetWideChar consumed " & IntImage ( LConsumed ) 
             & ", but it should always be 1." 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; IF ExpRaiseEndOfFile 
      THEN 
        WL ( "GetWideChar did not raise Rd.EndOfFile, when expected." ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LConsumed > 0 THEN ExpectUnGet ( LWch , "GetWideChar" ) END (* IF *)  

    ; INC ( Counts [ KindTyp . GetWideChar , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    ; RETURN LSucceeded 
    END ExpectGetWideChar 

; PROCEDURE TestGetWideChar ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 
    ; MakeCRLFPostponed ( ) 
    ; EVAL ExpectGetWideChar ( WchCR ) 
    ; EVAL ExpectGetWideChar ( WchLF ) 
    ; MakePostponed ( WchA ) 
    ; EVAL ExpectGetWideChar ( WchA ) 
    ; MakePostponed ( WchMax16 ) 
    ; EVAL ExpectGetWideChar ( WchMax16 ) 
    ; WriteChar ( WchB ) 
    ; EVAL ExpectGetWideChar ( WchB ) 
    ; WriteChar ( WchMax16 ) 
    ; EVAL ExpectGetWideChar ( WchMax16 ) 
    ; WriteClose ( ) 
    ; ExpectEOF ( KindTyp . GetWideChar ) 
    ; EVAL ExpectGetWideChar ( WchNull , ExpRaiseEndOfFile := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideChar 

; CONST WcharsNumber = 6 
; CONST WcharsLast = WcharsNumber - 1 
; TYPE ArrWch = ARRAY [ 0 .. WcharsLast ] OF Widechar 

; PROCEDURE ExpectGetWideSub 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrWch
  ; VAR LExpArr : ArrWch 
  ; VAR LLastWch : Widechar 
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; LCt := UniRd . GetWideSub ( GRdStream , SUBARRAY ( LBuff , 0, BuffSize ) )
    ; IF LCt # Consumed 
      THEN 
        WL ( "GetWideSub returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetWideSub consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArr := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; FOR RI := FIRST ( ArrWch ) TO BuffSize - 1  
      DO 
        IF LExpArr [ RI ] # WchNull 
        THEN 
          IF LBuff [ RI ] # LExpArr [ RI ] 
          THEN 
            WL ( "Char position" & IntImage ( RI )
                 & ", got " & WchImage ( LBuff [ RI ] )  
                 & "from GetWideSub, but expected " 
                 & WchImage ( LExpArr [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := LBuff [ RI ] 
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after GetWideSub was " & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LCt > 0 THEN ExpectUnGet ( LLastWch , "GetWideSub" ) END (* IF *) 

    ; INC ( Counts [ KindTyp . GetWideSub , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetWideSub 

; PROCEDURE TestGetWideSub ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetWideSub ( 0 , 0 ) 
    ; ExpectGetWideSub ( 2 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetWideSub ( 1 , 1 , WchCR ) 
    ; ExpectGetWideSub ( 1 , 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetWideSub ( 1 , 1 , WchCR ) 
    ; ExpectGetWideSub ( 2 , 2 , WchLF , WchA ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetWideSub ( 2 , 2 , WchLF , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetWideSub ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetWideSub ( 1 , 1 , WchLF ) 

    ; WriteChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetWideSub ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; ExpectGetWideSub ( 1 , 1 , WchI ) 
    ; MakePostponed ( WchMax16 ) 
    ; ExpectGetWideSub ( 1 , 1 , WchMax16 ) 

    ; WriteChar ( WchJ ) 
    ; ExpectGetWideSub ( 1 , 1 , WchJ ) 
    ; WriteChar ( WchMax16 ) 
    ; ExpectGetWideSub ( 1 , 1 , WchMax16 ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; ExpectGetWideSub ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetWideSub ( 1 , 1 , WchM ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSub ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetWideSub ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideSub

; TYPE ArrCh = ARRAY [ 0 .. WcharsLast ] OF CHAR  

; PROCEDURE ExpectGetSub 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ; ExpRangePos : INTEGER 
        := - 1 (* Which means Range is expected not to be raised. *) 
    ; ExpRangeWch : Widechar := WchNull 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrCh
  ; VAR LExpArrWch : ArrWch  
  ; VAR LLastWch : Widechar 
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; TRY 
        LCt 
          := UniRd . GetSub ( GRdStream , SUBARRAY ( LBuff , 0, BuffSize ) )
      EXCEPT 
         Range ( ERangeInfo ) 
      => IF ExpRangePos < 0 
         THEN 
           WL ( "GetSub unexpectedly raised " & RangeInfoImage ( ERangeInfo ) 
              ) 
         ; LSucceeded := FALSE  
         ELSIF ERangeInfo # RangeInfo { ExpRangeWch , ExpRangePos } 
         THEN 
           WL ( "GetSub raised " & RangeInfoImage ( ERangeInfo ) 
                & ", expected " 
                & RangeInfoImage ( RangeInfo { ExpRangeWch , ExpRangePos } ) 
              ) 
         ; LSucceeded := FALSE  
         END (* IF *) 
      ; INC ( Counts [ KindTyp . GetSub , LSucceeded ] )  
      ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
      ; Wr . Flush ( Stdio . stdout ) 
      ; RETURN 
      END (* EXCEPT *) 

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetSub returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetSub consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; FOR RI := FIRST ( ArrCh ) TO BuffSize - 1  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          IF ORD ( LBuff [ RI ] ) # ORD ( LExpArrWch [ RI ] )  
          THEN 
            WL ( "Char position" & IntImage ( RI )
                 & ", got " & ChImage ( LBuff [ RI ] )  
                 & " from GetSub, but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := VAL ( ORD ( LBuff [ RI ] ) , Widechar )  
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after GetSub was " & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; IF ExpRangePos >= 0 
      THEN 
        WL ( "GetSub did not raise Range, but expected " 
              & RangeInfoImage ( RangeInfo { ExpRangeWch , ExpRangePos } ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LCt > 0 THEN ExpectUnGet ( LLastWch , "GetSub" ) END (* IF *) 

    ; INC ( Counts [ KindTyp . GetSub , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetSub 

; PROCEDURE TestGetSub ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 

    ; WriteChars ( WchA , WchB , WchMax16 ) 
    ; ExpectGetSub 
        ( 3 , 2 , WchA , WchB , WchMax16 
        , ExpRangePos := 2 , ExpRangeWch := WchMax16 
        )

    ; MakePostponed ( WchMax16 ) 
    ; ExpectGetSub 
        ( 1 , 1 , WchMax16 , ExpRangePos := 0 , ExpRangeWch := WchMax16 ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSub ( 0 , 0 ) 
    ; ExpectGetSub ( 2 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSub ( 1 , 1 , WchCR ) 
    ; ExpectGetSub ( 1 , 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetSub ( 1 , 1 , WchCR ) 
    ; ExpectGetSub ( 2 , 2 , WchLF , WchA ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetSub ( 2 , 2 , WchLF , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetSub ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetSub ( 1 , 1 , WchLF ) 

    ; WriteChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetSub ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; ExpectGetSub ( 1 , 1 , WchI ) 

    ; WriteChar ( WchJ ) 
    ; ExpectGetSub ( 1 , 1 , WchJ ) 
    ; WriteChar ( WchK ) 
    ; ExpectGetSub ( 1 , 1 , WchK ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; ExpectGetSub ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetSub ( 1 , 1 , WchM ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetSub ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetSub ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetSub

; PROCEDURE ExpectGetWideSubLine 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ; ExpRangePos : INTEGER 
        := - 1 (* Which means Range is expected not to be raised. *) 
    ; ExpRangeWch : Widechar := WchNull 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LLastWch : Widechar 
  ; VAR LBuff : ArrWch
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; LCt 
        := UniRd . GetWideSubLine 
             ( GRdStream , SUBARRAY ( LBuff , 0, BuffSize ) )

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetWideSubLine returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetWideSubLine consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; FOR RI := FIRST ( ArrCh ) TO BuffSize - 1  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          IF LBuff [ RI ] # LExpArrWch [ RI ]  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & WchImage ( LBuff [ RI ] )  
                 & " from GetWideSubLine, but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := LBuff [ RI ] 
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after GetWideSubLine was " & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; IF ExpRangePos >= 0 
      THEN 
        WL ( "GetWideSubLine did not raise Range, but expected " 
              & RangeInfoImage ( RangeInfo { ExpRangeWch , ExpRangePos } ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LCt > 0 THEN ExpectUnGet ( LLastWch , "GetWideSubLine" ) END (* IF *) 

    ; INC ( Counts [ KindTyp . GetWideSubLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetWideSubLine 

; PROCEDURE TestGetWideSubLine ( ) 

  = BEGIN 
RETURN 
; 
      OpenPipe ( GEnc ) 

    ; WriteChars ( WchA , WchB , WchMax16 ) 
    ; ExpectGetWideSubLine ( 3 , 3 , WchA , WchB , WchMax16 )

    ; MakePostponed ( WchMax16 ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchMax16 ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetWideSubLine ( 0 , 0 ) 
    ; ExpectGetWideSubLine ( 1 , 0 ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetWideSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchCR , WchLF ) 
    (* We can't read up to EOF with our intermittent pipe, without closing. *)
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 2 , 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 4 , 2 , WchCR , WchLF , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetWideSubLine ( 4 , 1 , WchLF ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetWideSubLine ( 4 , 3 , WchC , WchD , WchE ) 
    ; ExpectGetWideSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; WriteChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetWideSubLine ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchI ) 

    ; WriteChars ( WchJ , WchK ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchJ , WchK ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchM ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetWideSubLine ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideSubLine

; PROCEDURE ExpectGetSubLine 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ; ExpRangePos : INTEGER 
        := - 1 (* Which means Range is expected not to be raised. *) 
    ; ExpRangeWch : Widechar := WchNull 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrCh
  ; VAR LExpArrWch : ArrWch  
  ; VAR LLastWch : Widechar 
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; TRY 
        LCt 
          := UniRd . GetSubLine ( GRdStream , SUBARRAY ( LBuff , 0, BuffSize ) )
      EXCEPT 
         Range ( ERangeInfo ) 
      => IF ExpRangePos < 0 
         THEN 
           WL ( "GetSubLine unexpectedly raised " 
                 & RangeInfoImage ( ERangeInfo ) 
              ) 
         ; LSucceeded := FALSE  
         ELSIF ERangeInfo # RangeInfo { ExpRangeWch , ExpRangePos } 
         THEN 
           WL ( "GetSubLine raised " & RangeInfoImage ( ERangeInfo ) 
                & ", expected " 
                & RangeInfoImage ( RangeInfo { ExpRangeWch , ExpRangePos } ) 
              ) 
         ; LSucceeded := FALSE  
         END (* IF *) 
      ; INC ( Counts [ KindTyp . GetSubLine , LSucceeded ] )  
      ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
      ; Wr . Flush ( Stdio . stdout ) 
      ; RETURN 
      END (* EXCEPT *) 

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetSubLine returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetSubLine consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; FOR RI := FIRST ( ArrCh ) TO BuffSize - 1  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          IF ORD ( LBuff [ RI ] ) # ORD ( LExpArrWch [ RI ] )  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & ChImage ( LBuff [ RI ] )  
                 & " from GetSubLine, but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := VAL ( ORD ( LBuff [ RI ] ) , Widechar )  
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after GetSubLine was " & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; IF ExpRangePos >= 0 
      THEN 
        WL ( "GetSubLine did not raise Range, but expected " 
              & RangeInfoImage ( RangeInfo { ExpRangeWch , ExpRangePos } ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LCt > 0 THEN ExpectUnGet ( LLastWch , "GetSubLine" ) END (* IF *) 

    ; INC ( Counts [ KindTyp . GetSubLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetSubLine 

; PROCEDURE TestGetSubLine ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 

    ; WriteChars ( WchA , WchB , WchMax16 ) 
    ; ExpectGetSubLine 
        ( 3 , 2 , WchA , WchB , WchMax16 
        , ExpRangePos := 2 , ExpRangeWch := WchMax16 
        )

    ; MakePostponed ( WchMax16 ) 
    ; ExpectGetSubLine 
        ( 1 , 1 , WchMax16 , ExpRangePos := 0 , ExpRangeWch := WchMax16 ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSubLine ( 0 , 0 ) 
    ; ExpectGetSubLine ( 1 , 0 ) 
    ; ExpectGetSubLine ( 2 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetSubLine ( 2 , 2 , WchCR , WchLF ) 
    (* We can't read up to EOF with our intermittent pipe, without closing. *)
    ; WriteClose ( ) 
    ; ExpectGetSubLine ( 2 , 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; WriteClose ( ) 
    ; ExpectGetSubLine ( 4 , 2 , WchCR , WchLF , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetSubLine ( 4 , 1 , WchLF ) 
    ; ExpectGetSubLine ( 1 , 1 , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetSubLine ( 4 , 3 , WchC , WchD , WchE ) 
    ; ExpectGetSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; WriteChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetSubLine ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; ExpectGetSubLine ( 1 , 1 , WchI ) 

    ; WriteChars ( WchJ , WchK ) 
    ; ExpectGetSubLine ( 2 , 2 , WchJ , WchK ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; ExpectGetSubLine ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetSubLine ( 1 , 1 , WchM ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetSubLine ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetSubLine ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetSubLine

; PROCEDURE ExpectGetText 
    ( Len : [ 0 .. WcharsNumber ] 
    ; ExpLen : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LLen : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LText : TEXT 
  ; VAR LWch : Widechar 
  ; VAR LLastWch : Widechar 
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; LText := UniRd . GetText ( GRdStream , Len ) 

    ; LLen := Text . Length ( LText ) 
    ; IF LLen # ExpLen 
      THEN 
        WL ( "GetText" & " returned a text of length" 
             & IntImage ( LLen ) 
             & ", expected " & IntImage ( ExpLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LLen 
      THEN 
        WL ( "GetText" & " consumed " & IntImage ( LConsumed ) 
             & ", but returned text of length" & IntImage ( LLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; FOR RI := FIRST ( ArrWch ) TO LAST ( LExpArrWch )  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          LWch := VAL ( ORD ( Text . GetWideChar ( LText , RI ) ) , Widechar )
        ; IF LWch # LExpArrWch [ RI ]  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & WchImage ( LWch )  
                 & " from " & "GetText" & ", but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := LWch  
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after " & "GetText" & " was " 
             & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LLen > 0 THEN ExpectUnGet ( LLastWch , "GetText" ) END (* IF *) 

    ; INC ( Counts [ KindTyp . GetText , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetText 

; PROCEDURE TestGetText ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetText ( 0 , 0 ) 
    ; ExpectGetText ( 2 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetText ( 1 , 1 , WchCR ) 
    ; ExpectGetText ( 1 , 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetText ( 1 , 1 , WchCR ) 
    ; ExpectGetText ( 2 , 2 , WchLF , WchA ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetText ( 2 , 2 , WchLF , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetText ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetText ( 1 , 1 , WchLF ) 

    ; WriteChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetText ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; ExpectGetText ( 1 , 1 , WchI ) 
    ; MakePostponed ( WchMax16 ) 
    ; ExpectGetText ( 1 , 1 , WchMax16 ) 

    ; WriteChar ( WchJ ) 
    ; ExpectGetText ( 1 , 1 , WchJ ) 
    ; WriteChar ( WchMax16 ) 
    ; ExpectGetText ( 1 , 1 , WchMax16 ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; ExpectGetText ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetText ( 1 , 1 , WchM ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetText ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
 (* ; ExpectGetText ( 1 , 0 , ExpEOF := TRUE ) *) 
(* CHECK^ Why does this block when the one above does not and same pattern
          in TestGetWideSub does not? *) 
    ; ReadClose ( ) 
    END TestGetText

; PROCEDURE ExpectGetLine 
    ( ExpLen : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ; ExpEndOfFile : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LLen : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LText : TEXT 
  ; VAR LWch : Widechar 
  ; VAR LLastWch : Widechar 
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

; VAR LWchExp : Widechar 
; VAR LWchExpOrd : INTEGER 
; VAR LWchOrd : INTEGER 
; VAR LBitsize , LLast : INTEGER 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := UniRd . Index ( GRdStream ) 
    ; TRY 
        LText := UniRd . GetLine ( GRdStream ) 
      EXCEPT 
        Rd . EndOfFile 
        => IF NOT ExpEndOfFile 
          THEN 
            WL ( "GetLine" & " unexpectedly raised EndOfFile." ) 
          ; LSucceeded := FALSE  
          END (* IF *) 
        ; INC ( Counts [ KindTyp . GetLine , LSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
        ; Wr . Flush ( Stdio . stdout ) 
        ; RETURN 
      END (* EXCEPT *) 

    ; LLen := Text . Length ( LText ) 
    ; IF LLen # ExpLen 
      THEN 
        WL ( "GetLine" & " returned a text of length " 
             & IntImage ( LLen ) 
             & ", expected " & IntImage ( ExpLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := UniRd . Index ( GRdStream ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LLen 
      THEN 
        WL ( "GetLine" & " consumed " & IntImage ( LConsumed ) 
             & ", but returned text of length" & IntImage ( LLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch := ArrWch { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 } 
    ; LLastWch := WchNull 
    ; FOR RI := FIRST ( ArrWch ) TO LAST ( LExpArrWch )  
      DO
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          LWch := VAL ( ORD ( Text . GetWideChar ( LText , RI ) ) , Widechar )
; LWchExp := LExpArrWch [ RI ]
; LWchExpOrd := ORD ( LWchExp ) 
; LWchOrd := ORD ( LWch ) 
; LBitsize := BITSIZE ( WIDECHAR ) 
; LLast := ORD( LAST ( WIDECHAR ) )  
        ; IF LWch # LExpArrWch [ RI ]  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & WchImage ( LWch )  
                 & " from " & "GetLine" & ", but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
        ; LLastWch := LWch 
        END (* IF *) 
      END (* FOR *) 

    ; LEOF := EOF ( )  
    ; IF LEOF # ExpEOF 
      THEN 
        WL ( "EOF after " & "GetLine" & " was " 
             & BoolImage ( LEOF ) 
             & ", but expected " & BoolImage ( ExpEOF ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 
    ; IF LConsumed > 0 THEN ExpectUnGet ( LLastWch , "GetLine" ) END (*  IF *) 

    ; INC ( Counts [ KindTyp . GetLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetLine 

; PROCEDURE ExpectUnGet ( Wch : Widechar ; PrevOpName : TEXT ) 

  = VAR LPreIndex , LFinalIndex : CARDINAL 
  ; VAR LWch : Widechar 
  ; VAR LSucceeded : BOOLEAN 

  ; <* FATAL Thread . Alerted , Rd . Failure , Wr . Failure *> 
    BEGIN 
      IF Wch = WchNull THEN RETURN END (* IF *) 
    ; LFinalIndex := UniRd . Index ( GRdStream ) 
    ; LSucceeded := UniRd . UnGetCodePoint ( GRdStream ) 
    ; IF NOT LSucceeded 
      THEN 
        WL ( "UnGetCodePoint, after " & PrevOpName & " failed unexpectedly." ) 
      ELSE 
        LPreIndex := UniRd . Index ( GRdStream ) 
      ; LSucceeded := LPreIndex + 1 = LFinalIndex 
      ; IF NOT LSucceeded 
        THEN
          WL ( "UnGetCodePoint, after " & PrevOpName 
               & " left index of " & IntImage ( LPreIndex ) 
               & " expected " & IntImage ( LFinalIndex - 1 ) 
             ) 
        (* But fall thru' to try readback anyway. *) 
        END (* IF *) 
      ; TRY 
          LWch := UniRd . GetWideChar ( GRdStream ) 
        EXCEPT 
          Rd . EndOfFile 
          => WL ( "GetWideChar, after UnGetCodePoint, after " & PrevOpName
                  & " unexpectedly raised EndOfFile." 
                ) 
          ; LSucceeded := FALSE  

          ; INC ( Counts [ KindTyp . GetLine , LSucceeded ] )  
          ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
          ; Wr . Flush ( Stdio . stdout ) 
          ; RETURN 
        END (* EXCEPT *) 
      ; IF LWch # Wch 
        THEN
          WL ( "GetWideChar, after UnGetCodePoint, after " & PrevOpName
               & " returned " & WchImage ( LWch )
               & " but expected " & WchImage ( Wch )
             ) 
        ; LSucceeded := FALSE 
        END (* IF *) 
      END (* IF *) 
    ; INC ( Counts [ KindTyp . UnGetCodePoint , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectUnGet 

; PROCEDURE TestGetLine ( ) 

  = BEGIN 
      OpenPipe ( GEnc ) 

    ; WriteChars ( WchA , WchB , WchMax16 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , WchA , WchB , WchMax16 , ExpEOF := TRUE )
    ; OpenPipe ( GEnc ) 

    ; MakePostponed ( WchMax16 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchMax16 , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetLine ( 2 , WchCR , WchLF ) 

    ; MakePostponed ( WchLF ) 
    ; ExpectGetLine ( 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetLine ( 2 , WchCR , WchLF ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChar ( WchCR ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchCR , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchCR , WchA ) 
    ; ExpectGetLine ( 1 , WchCR ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchLF , WchNEL , WchVT , WchFF , WchPS , WchLS ) 
    ; ExpectGetLine ( 1 , WchLF ) 
    ; ExpectGetLine ( 1 , WchNEL )
    ; ExpectGetLine ( 1 , WchVT )
    ; ExpectGetLine ( 1 , WchFF )
    ; ExpectGetLine ( 1 , WchPS )
    ; ExpectGetLine ( 1 , WchLS )

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetLine ( 1 , WchLF ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchB , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetLine ( 5 , WchC , WchD , WchE , WchCR , WchLF ) 

    ; MakePostponed ( WchI ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , WchI , ExpEOF := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , WchK , WchL , WchM , ExpEOF := TRUE ) 
    ; ExpectGetLine ( 0 , ExpEndOfFile := TRUE ) 
    ; OpenPipe ( GEnc ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetLine ( 0 , ExpEndOfFile := TRUE ) 

    ; ReadClose ( ) 
    END TestGetLine

; CONST EncodingMax 
    = ARRAY Encoding OF Widechar 
        { (* Null      *) W'\X0000'  
        , (* Internal  *) W'\X0000'
        , (* ISO8859_1 *) W'\X00FF'
        , (* CM3WC     *) W'\XFFFF'
        , (* UCS2      *) LAST ( Widechar ) 
        , (* UCS2LE    *) LAST ( Widechar ) 
        , (* UCS2BE    *) LAST ( Widechar ) 
        , (* UTF8      *) LAST ( Widechar ) 
        , (* UTF16     *) LAST ( Widechar ) 
        , (* UTF16LE   *) LAST ( Widechar ) 
        , (* UTF16BE   *) LAST ( Widechar )
        , (* UTF32     *) LAST ( Widechar ) 
        , (* UTF32LE   *) LAST ( Widechar ) 
        , (* UTF32BE   *) LAST ( Widechar ) 
        } 

; PROCEDURE ExpectUnGetCodePoint ( Wch : Widechar ; Enc : Encoding ) 

  = VAR LWrStartCIndex , LWrMidCIndex , LWrFinalCIndex : Word . T 
    (* ^CHAR indexes in the Sink Wr.T. *) 
  ; VAR LRdStartCIndex , LRdMidCIndex , LRdFinalCIndex , LRdCurrentCIndex : Word . T 
    (* ^CHAR indexes in the Source Rd.T. *) 
  ; VAR LRdStartUIndex , LRdFinalUIndex , LRdCurrentUIndex : Word . T 
    (* ^Unicode code point indexes in the Source Rd.T. *) 
  ; VAR EUPhaseName : TEXT  
  ; VAR EUUngetCCt : CARDINAL 
  ; VAR EUSucceeded : BOOLEAN 

  ; CONST SideChar1 = '\x3D' 
  ; CONST SideChar2 = '\xD6'
  ; CONST SideChar3 = '\x59'
  ; CONST SideChar4 = '\xE3'

  ; PROCEDURE FlushCHARs ( ) 
    (* Consume any unread/ungotten CHARS placed by this test. *)  

    = VAR LCIndex : CARDINAL 
    ; VAR LCh : CHAR 

    ; <* FATAL Rd . EndOfFile , Rd . Failure , Thread . Alerted  *> 
      BEGIN (* FlushCHARs *) 
        LOOP 
          LCIndex := Rd . Index ( GRd ) 
        ; IF LCIndex >= LRdFinalCIndex 
          THEN EXIT
          ELSE
            LCh := Rd . GetChar ( GRd ) 
          END (* IF *) 
        END (* LOOP *) 
      END FlushCHARs 

  ; PROCEDURE ReadBack ( AreTesting : BOOLEAN (* Treat problems as test failures. *) )
    : BOOLEAN (* Successful read back. *) 
    (* Read leading CHARS, code point trailing CHARs, and apply various checks. 
       IF AreTesting, treat the checks as tests, otherwise, threat this as just
       setup for ungetting. 
    *) 

    = VAR RBStepName : TEXT 
    ; VAR RBWch : Widechar 
    ; VAR RBSucceeded : BOOLEAN 

    ; PROCEDURE ReadBackCHAR ( ExpCh : CHAR ) : BOOLEAN (* Success *) 
      RAISES { Rd . EndOfFile } 
      (* Read back and check one CHAR. *) 

      = VAR LCh : CHAR 
      ; VAR LSucceeded : BOOLEAN

      ; <* FATAL Thread . Alerted , Rd . Failure *> 
        BEGIN (* ReadBackCHAR *)
          LCh := Rd . GetChar ( GRd ) 
        ; LSucceeded := LCh = ExpCh 
        ; IF NOT LSucceeded  
          THEN 
            WL ( EUPhaseName & "UnGetCodePoint, rereading ungotten " & RBStepName 
                 & ", got " & ChImage ( LCh )  
                 & ", but expected " & ChImage ( ExpCh ) 
               ) 
          END (* IF *) 
        ; IF AreTesting 
          THEN 
            INC ( Counts [ KindTyp . UnGetCodePoint , LSucceeded ] )  
          ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
          END (* IF *) 
        ; RETURN LSucceeded 
        END ReadBackCHAR 

    ; BEGIN (* ReadBack *)
        TRY 
        (* Read back leading CHARs: *) 
          RBStepName := "CHAR1" 
        ; RBSucceeded := ReadBackCHAR ( SideChar1 )  
        ; RBStepName := "CHAR2" 
        ; IF RBSucceeded 
          THEN 
            RBSucceeded := ReadBackCHAR ( SideChar2 )  
          END (* IF *) 

        (* Read Back the code point: *) 
        ; IF RBSucceeded 
          THEN 
            RBWch := UniRd . GetWideChar ( GRdStream ) 
          ; IF RBWch # Wch   
            THEN 
              WL ( EUPhaseName & "UnGetCodePoint, rereading ungotten code point, " 
                   & "got " & WchImage ( RBWch )  
                   & ", but expected " & WchImage ( Wch ) 
                 ) 
            END (* IF *) 
          ; IF AreTesting 
            THEN 
              INC ( Counts [ KindTyp . UnGetCodePoint , RBSucceeded ] )  
            ; INC ( Counts [ KindTyp . Total , RBSucceeded ] )  
            END (* IF *) 
          ; LRdCurrentCIndex := Rd . Index ( GRd ) 
          ; IF LRdCurrentCIndex # LRdMidCIndex 
            THEN 
              RBSucceeded := FALSE 
            ; WL ( EUPhaseName & "UnGetCodePoint, rereading ungotten code point, " 
                   & "got CHAR index " & IntImage ( LRdCurrentCIndex )  
                   & ", but expected " & IntImage ( LRdMidCIndex ) 
                 ) 
            END (* IF *) 
          ; IF AreTesting 
            THEN 
              INC ( Counts [ KindTyp . UnGetCodePoint , RBSucceeded ] )  
            ; INC ( Counts [ KindTyp . Total , RBSucceeded ] )  
            END (* IF *) 
          END (* IF *) 

        (* ReadBack the trailing CHARs: *) 
        ; RBStepName := "CHAR3" 
        ; IF RBSucceeded 
          THEN 
            RBSucceeded := ReadBackCHAR ( SideChar3 )  
          END (* IF *) 
        ; RBStepName := "CHAR4" 
        ; IF RBSucceeded 
          THEN 
            RBSucceeded := ReadBackCHAR ( SideChar4 )  
          END (* IF *) 
        ; LRdCurrentCIndex := Rd . Index ( GRd ) 
        ; IF LRdCurrentCIndex # LRdFinalCIndex 
          THEN 
            RBSucceeded := FALSE 
          ; WL ( EUPhaseName & "UnGetCodePoint, rereading trailing CHARs, " 
                 & "got CHAR index " & IntImage ( LRdCurrentCIndex )  
                 & ", but expected " & IntImage ( LRdFinalCIndex ) 
               ) 
          END (* IF *) 
        ; IF AreTesting 
          THEN 
            INC ( Counts [ KindTyp . UnGetCodePoint , RBSucceeded ] )  
          ; INC ( Counts [ KindTyp . Total , RBSucceeded ] )  
          END (* IF *) 

        (* Catch end of file: *) 
        EXCEPT Rd . EndOfFile 
        => RBSucceeded := FALSE 
        ; WL ( EUPhaseName & "UnGetCodePoint, EOF while rereading ungotten " & RBStepName )
        ; IF AreTesting 
          THEN 
            INC ( Counts [ KindTyp . UnGetCodePoint , RBSucceeded ] )  
          ; INC ( Counts [ KindTyp . Total , RBSucceeded ] )  
          ; FlushCHARs ( ) 
          ; Wr . Flush ( Stdio . stdout ) 
          END (* IF *) 
        END (* EXCEPT *) 
      ; IF NOT RBSucceeded THEN FlushCHARs ( ) END (* IF *) 
      ; RETURN RBSucceeded 
      END ReadBack 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN (* ExpectUnGetCodePoint *) 
      IF Wch <= EncodingMax [ Enc ] 
      THEN 

      (* Write the Expected code point, surrounded by some plain CHARs, written
         by bypassing the UniRd.T. *) 
        OpenPipe ( Enc ) 
      ; LWrStartCIndex := Wr . Index ( GWr ) 
      ; Wr . PutChar ( GWr , SideChar1 ) 
      ; Wr . PutChar ( GWr , SideChar2 ) 
      ; WriteChar ( Wch ) (* The code point to be tested. *) 
      ; LWrMidCIndex := Wr . Index ( GWr ) 
      ; Wr . PutChar ( GWr , SideChar3 ) 
      ; Wr . PutChar ( GWr , SideChar4 )
      ; LWrFinalCIndex := Wr . Index ( GWr ) 
      ; Wr . Flush ( UniWr . Sink ( GWrStream ) ) 

      ; LRdStartCIndex := Rd . Index ( GRd ) 
      ; LRdMidCIndex := LRdStartCIndex + ( LWrMidCIndex - LWrStartCIndex ) 
      ; LRdFinalCIndex := LRdStartCIndex + ( LWrFinalCIndex - LWrStartCIndex ) 
      ; LRdFinalUIndex := LRdStartCIndex + ( LWrFinalCIndex - LWrStartCIndex ) 

      ; EUPhaseName := "Setting up for " 
      ; EUSucceeded := ReadBack ( AreTesting := FALSE ) 
      ; LRdFinalUIndex := UniRd . Index ( GRdStream ) 

      (* Unget trailing CHARs: *) 
      ; IF EUSucceeded 
        THEN 
          EUUngetCCt := Rd . UnGetCharMulti ( GRd , 2 ) 
        ; IF EUUngetCCt # 2 
          THEN 
            WL ( "UnGetCodePoint, ungetting trailing CHARS, ungot " 
                 & IntImage ( EUUngetCCt ) 
                 & ", but expected " & IntImage ( 2 ) 
               ) 
          ; EUSucceeded := FALSE 
          END (* IF *) 
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )  
        ; LRdCurrentCIndex := Rd . Index ( GRd ) 
        ; IF LRdCurrentCIndex # LRdMidCIndex 
          THEN
            EUSucceeded := FALSE 
          ; WL ( "UnGetCodePoint, ungetting trailing CHARs, " 
                 & "got CHAR index " & IntImage ( LRdCurrentCIndex )  
                 & ", but expected " & IntImage ( LRdMidCIndex ) 
               ) 
          END (* IF *) 
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )  
        ; Wr . Flush ( Stdio . stdout ) 
        END (* IF *) 

      (* Unget the code point: *) 
      ; IF EUSucceeded 
        THEN 
          LRdStartUIndex := UniRd . Index ( GRdStream ) 
        ; EUSucceeded := UniRd . UnGetCodePoint ( GRdStream ) 
        ; LRdCurrentUIndex := UniRd . Index ( GRdStream ) 
        ; IF NOT EUSucceeded 
          THEN
            WL ( "UnGetCodePoint returned failure" ) 
          END (* IF *) 
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )
        ; LRdCurrentUIndex := UniRd . Index ( GRdStream ) 
        ; IF LRdCurrentUIndex # LRdFinalUIndex - 1  
          THEN
            EUSucceeded := FALSE 
          ; WL ( "UnGetCodePoint, ungetting the code point, " 
                 & "got Unicode index " & IntImage ( LRdCurrentUIndex )  
                 & ", but expected " & IntImage ( LRdFinalUIndex - 1 ) 
               ) 
          END (* IF *)   
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )
        ; Wr . Flush ( Stdio . stdout ) 
        END (* IF *) 
        
      (* Unget leading CHARs: *) 
      ; IF EUSucceeded 
        THEN 
          EUUngetCCt := Rd . UnGetCharMulti ( GRd , 2 ) 
        ; IF EUUngetCCt # 2 
          THEN 
            WL ( "UnGetCodePoint, ungetting leading CHARS, ungot " 
                 & IntImage ( EUUngetCCt ) 
                 & ", but expected " & IntImage ( 2 ) 
               ) 
          ; EUSucceeded := FALSE 
          END (* IF *) 
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )  
        ; LRdCurrentCIndex := Rd . Index ( GRd ) 
        ; IF LRdCurrentCIndex # LRdStartCIndex 
          THEN
            EUSucceeded := FALSE 
          ; WL ( "UnGetCodePoint, ungetting leading CHARs, " 
                 & "got CHAR index " & IntImage ( LRdCurrentCIndex )  
                 & ", but expected " & IntImage ( LRdStartCIndex ) 
               ) 
          END (* IF *) 
        ; INC ( Counts [ KindTyp . UnGetCodePoint , EUSucceeded ] )  
        ; INC ( Counts [ KindTyp . Total , EUSucceeded ] )  
        ; Wr . Flush ( Stdio . stdout ) 
        END (* IF *) 

      ; EUPhaseName := "Testing " 
      ; EUSucceeded := ReadBack ( AreTesting := TRUE  ) 
      ; Wr . Flush ( Stdio . stdout ) 
      ; WriteClose ( ) 
      ; ReadClose ( ) 
      END (* IF *) 
    END ExpectUnGetCodePoint 

; PROCEDURE TestUnGetCodePoint ( Enc : Encoding ) 

  = BEGIN 
      ExpectUnGetCodePoint ( W'\U000000' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U00007F' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U000080' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U0000FF' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U000100' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U0007FF' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U000800' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U00FFFF' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U010000' , Enc ) 
    ; ExpectUnGetCodePoint ( W'\U10FFFF' , Enc ) 
    END TestUnGetCodePoint 

; PROCEDURE Work ( ) 

  = BEGIN 
      Counts := CountsTyp { ZeroOutcomes , .. }  
    ; GEnc := Encoding . UTF8 
    ; TestGetWideChar ( ) 
    ; TestGetWideSub ( ) 
    ; TestGetSub ( ) 
    ; TestGetWideSubLine ( ) 
    ; TestGetSubLine ( ) 
    ; TestGetText ( ) 
    ; TestGetLine ( ) 
    ; TestUnGetCodePoint ( GEnc ) 
    ; DumpCounts ( ) 
    END Work 

; BEGIN (* TestUniRd *) 
    Work ( ) 
  END TestUniRd 
. 
