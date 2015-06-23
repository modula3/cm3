MODULE TestRd EXPORTS Main 

; IMPORT FileRd
; IMPORT FileWr 
; IMPORT Fmt
; IMPORT OSError
; IMPORT Pipe  
; IMPORT Rd 
; IMPORT Stdio
; IMPORT Text 
; FROM Thread IMPORT Alerted 
; IMPORT Word 
; IMPORT Wr 

; IMPORT SmallBuffRd 

; TYPE Widechar = WIDECHAR  

; CONST WchNull = VAL ( 0 , Widechar ) 

; CONST WchCR = VAL ( ORD ( '\r' ) , Widechar ) 
; CONST WchLF = VAL ( ORD ( '\n' ) , Widechar ) 
; CONST WchFF = VAL ( ORD ( W'\f' ) , Widechar ) 
; CONST WchVT = VAL ( ORD ( W'\t' ) , Widechar ) 
; CONST WchNEL = VAL ( ORD ( W'\x0085' ) , Widechar ) 

; CONST WchMax8 = VAL ( 16_FF , Widechar ) 
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
(* 
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
*) 

; TYPE KindTyp 
    = { EOF 
      , CharsReady 
      , UnGetCharMulti 
      , GetWideChar 
      , GetWideSub 
      , GetSub
      , GetWideSubLine 
      , GetSubLine 
      , GetText 
      , GetLine 
      , Index 
      , AvgBytesPerChar 
      , Total 
      } 

; PROCEDURE KindImage ( K : KindTyp ) : TEXT 

  = BEGIN 
      CASE K 
      OF KindTyp . EOF => RETURN "EOF" 
      | KindTyp . CharsReady => RETURN "CharsReady" 
      | KindTyp . UnGetCharMulti => RETURN "UnGetCharMulti" 
      | KindTyp . GetWideChar => RETURN "GetWideChar" 
      | KindTyp . GetWideSub => RETURN "GetWideSub" 
      | KindTyp . GetSub => RETURN "GetSub"
      | KindTyp . GetWideSubLine => RETURN "GetWideSubLine" 
      | KindTyp . GetSubLine => RETURN "GetSubLine" 
      | KindTyp . GetText => RETURN "GetText" 
      | KindTyp . GetLine => RETURN "GetLine" 
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

  = VAR T1 , T2 , T3 , T4 : TEXT 

  ; BEGIN 
      T1 := "16_" & Fmt . Int ( ORD ( Wch ) , 16 )
    ; T2 := " (W\'"
    ; T4 := "\')"
    ; IF Wch = WchCR THEN T3 := "\\r" 
      ELSIF Wch = WchLF THEN T3 := "\\n" 
      ELSIF Wch = WchVT THEN T3 := "\\t" 
      ELSIF Wch = WchFF THEN T3 := "\\f" 
      ELSIF ORD ( W' ' ) <= ORD ( Wch ) AND ORD ( Wch ) <= ORD ( W'~' ) 
      THEN T3 :=  Fmt . Char ( VAL ( ORD ( Wch ) , CHAR ) ) 
      ELSE 
        T1 := ""
      ; T2 := ""
      ; T3 := "" 
      END (* IF *) 
    ; RETURN T1 & T2 & T3 & T4    
    END WchImage 

; PROCEDURE ChImage ( Ch : CHAR ) : TEXT 

  = VAR T1 , T2 , T3 , T4 : TEXT 

  ; BEGIN 
      T1 := "16_" & Fmt . Int ( ORD ( Ch ) , 16 )  
    ; T2 := " (\'"
    ; T4 := "\')"
    ; IF Ch = '\r' THEN T3 := "\\r" 
      ELSIF Ch = '\n' THEN T3 := "\\n" 
      ELSIF Ch = '\t' THEN T3 := "\\t" 
      ELSIF Ch = '\f' THEN T3 := "\\f" 
      ELSIF ' ' <= Ch AND Ch <= '~'  
      THEN T3 :=  Fmt . Char ( Ch ) 
      ELSE 
        T1 := ""
      ; T2 := ""
      ; T3 := "" 
      END (* IF *) 
    ; RETURN T1 & T2 & T3 & T4    
    END ChImage 

(*
; PROCEDURE RangeInfoImage ( Info : RangeInfo ) : TEXT 

  = BEGIN 
      RETURN "Range(RangeInfo{" 
             & WchImage ( Info . Wch ) 
             & ","
             & IntImage ( Info . Location ) 
             & "})"
    END RangeInfoImage 
*) 

; PROCEDURE WL ( L : TEXT ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . PutText ( Stdio . stdout , L ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END WL 

; VAR GWrFile , GRdFile : Pipe . T 
; VAR GWr : Wr . T 
; VAR GRd : Rd . T 

; PROCEDURE OpenPipe ( ) 

  = <* FATAL OSError . E *> 
    BEGIN 
      Pipe . Open ( (*VAR*) GRdFile , GWrFile ) 
    ; GWr := NEW ( FileWr . T ) . init ( GWrFile ) 
    ; GRd := NEW ( FileRd . T ) . init ( GRdFile ) 
    END OpenPipe 

; PROCEDURE WriteClose ( ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . Close ( GWr )
    END WriteClose

; PROCEDURE ReadClose ( ) 

  = <* FATAL Rd . Failure , Alerted *> 
    BEGIN 
      Rd . Close ( GRd )
    END ReadClose

; PROCEDURE WriteChar ( Wch : Widechar ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . PutChar ( GWr , VAL ( ORD ( Wch ) , CHAR ) ) 
    ; Wr . Flush ( GWr ) 
    END WriteChar 

; PROCEDURE WriteChars 
    ( Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ) 

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
    ; WOne ( Wch7 )
    ; WOne ( Wch8 )
    ; WOne ( Wch9 )
    ; WOne ( Wch10 )
    END WriteChars 

; PROCEDURE PostponeChars 
    ( Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ) 

  = VAR LCt : CARDINAL := 0
  ; VAR LCh : CHAR
  ; VAR LSucceeded : BOOLEAN := TRUE  

  ; PROCEDURE POne ( Wch : Widechar ) 

    = BEGIN 
        IF Wch # WchNull 
        THEN 
          WriteChar ( Wch ) 
        ; INC ( LCt ) 
        END  (* IF *) 
      END POne 

  ; <* FATAL Rd . Failure , Rd . EndOfFile , Wr . Failure , Alerted *> 
    BEGIN (* PostponeChars. *)
      LCt := 0 
    ; POne ( Wch1 )
    ; POne ( Wch2 )
    ; POne ( Wch3 )
    ; POne ( Wch4 )
    ; POne ( Wch5 )
    ; POne ( Wch6 )
    ; POne ( Wch7 )
    ; POne ( Wch8 )
    ; POne ( Wch9 )
    ; POne ( Wch10 )
    (* Read them, then unget them one at a time. *) 
    ; FOR RI := 1 TO LCt 
      DO LCh := Rd . GetChar ( GRd ) 
      END (* FOR *) 
    ; FOR RI := 1 TO LCt 
      DO 
        LSucceeded := Rd . UnGetCharMulti ( GRd ) = 1 
      ; IF NOT LSucceeded 
        THEN 
          WL ( "UnGetCharMulti(1) failed." ) 
        END (* IF *) 
      ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
      ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
      END (* FOR *) 
    (* Read them again, then unget them all at once. *) 
    ; FOR RI := 1 TO LCt 
      DO LCh := Rd . GetChar ( GRd ) 
      END (* FOR *) 
    ; LSucceeded := Rd . UnGetCharMulti ( GRd , LCt ) = LCt 
    ; IF NOT LSucceeded 
      THEN 
        WL ( "UnGetCharMulti(" & Fmt . Int ( LCt ) &  ") failed." ) 
      END (* IF *) 
    ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  

    ; Wr . Flush ( Stdio . stdout ) 
    END PostponeChars 

; PROCEDURE MakeCRLFPostponed ( ) 

  = BEGIN 
      PostponeChars ( WchCR , WchLF ) 
    END MakeCRLFPostponed 

; PROCEDURE MakePostponed ( Wch : Widechar ) 
  (* PRE: Wch # WchLF. *) 

  = BEGIN 
      PostponeChars ( Wch ) 
    END MakePostponed 

; PROCEDURE WriteWChar ( Wch : Widechar ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . PutWideChar ( GWr , Wch ) 
    ; Wr . Flush ( GWr ) 
    END WriteWChar 

; PROCEDURE WriteWChars 
    ( Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ) 

  = PROCEDURE WrOneWide ( Wch : Widechar ) 

    = BEGIN 
        IF Wch # WchNull 
        THEN WriteWChar ( Wch ) 
        END  (* IF *) 
      END WrOneWide 

  ; BEGIN (* WriteWChars. *) 
      WrOneWide ( Wch1 )
    ; WrOneWide ( Wch2 )
    ; WrOneWide ( Wch3 )
    ; WrOneWide ( Wch4 )
    ; WrOneWide ( Wch5 )
    ; WrOneWide ( Wch6 )
    ; WrOneWide ( Wch7 )
    ; WrOneWide ( Wch8 )
    ; WrOneWide ( Wch9 )
    ; WrOneWide ( Wch10 )
    END WriteWChars 

; PROCEDURE PostponeWChars 
    ( Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ) 

  = VAR LCt : CARDINAL := 0
  ; VAR LWch : Widechar 
  ; VAR LSucceeded : BOOLEAN := TRUE  

  ; PROCEDURE POneWide ( Wch : Widechar ) 

    = BEGIN 
        IF Wch # WchNull 
        THEN 
          WriteWChar ( Wch ) 
        ; INC ( LCt ) 
        END  (* IF *) 
      END POneWide 

  ; <* FATAL Rd . Failure , Rd . EndOfFile , Wr . Failure , Alerted *> 
    BEGIN (* PostponeWChars. *) 
      LCt := 0 
    ; POneWide ( Wch1 )
    ; POneWide ( Wch2 )
    ; POneWide ( Wch3 )
    ; POneWide ( Wch4 )
    ; POneWide ( Wch5 )
    ; POneWide ( Wch6 )
    ; POneWide ( Wch7 )
    ; POneWide ( Wch8 )
    ; POneWide ( Wch9 )
    ; POneWide ( Wch10 )

    (* Read them, then unget them one at a time. *) 
    ; FOR RI := 1 TO LCt (* Widechars. *)  
      DO LWch := Rd . GetWideChar ( GRd ) 
      END (* FOR *) 
    ; FOR RI := 1 TO LCt * 2 (* CHARs *) 
      DO 
        LSucceeded := Rd . UnGetCharMulti ( GRd ) = 1  
      ; IF NOT LSucceeded 
        THEN 
          WL ( "UnGetCharMulti(1) failed." ) 
        END (* IF *) 
      ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
      ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
      END (* FOR *) 

    (* Read them again, then unget them all at once. *) 
    ; FOR RI := 1 TO LCt (* Widechars. *)  
      DO LWch := Rd . GetWideChar ( GRd ) 
      END (* FOR *) 
    ; LSucceeded := Rd . UnGetCharMulti ( GRd , LCt * 2 ) = LCt * 2  
    ; IF NOT LSucceeded 
      THEN 
        WL ( "UnGetCharMulti(" & Fmt . Int ( LCt * 2 ) & ") failed." ) 
      END (* IF *) 
    ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
 
   ; Wr . Flush ( Stdio . stdout ) 
    END PostponeWChars 

; PROCEDURE MakeWCRLFPostponed ( ) 

  = BEGIN 
      PostponeWChars ( WchCR , WchLF ) 
    END MakeWCRLFPostponed 

; PROCEDURE MakeWPostponed ( Wch : Widechar ) 
  (* PRE: Wch # WchLF. *) 

  = BEGIN 
      PostponeWChars ( Wch ) 
    END MakeWPostponed 

; PROCEDURE EOF ( ) : BOOLEAN 

  = BEGIN 
(* This is tricky.  Our pipe is intermittent, and we can't distinguish
   EOF from just no chars ready, in any normal way.  But we know EOF will
   only happen when we close GWr. *) 
      RETURN Wr . Closed ( GWr ) 
    END EOF 

; PROCEDURE ExpectEOF 
    ( FKind : KindTyp ; Expected : BOOLEAN := TRUE ) 

  = VAR LSucceeded : BOOLEAN 

  ; <* FATAL Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *)  
    ; IF EOF ( )  
      THEN
        IF NOT Expected 
        THEN 
          WL ( "Rd.EOF is TRUE, but expected FALSE." ) 
        ; LSucceeded := FALSE 
        END  (* IF *) 
      ELSE 
        IF Expected 
        THEN 
          WL ( "Rd.EOF is FALSE, but expected TRUE." ) 
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

  = VAR LOldIndex , LNewIndex : Word . T 
  ; VAR LConsumed : CARDINAL 
  ; VAR LWch : Widechar 
  ; VAR LSucceeded : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN 
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; TRY 
        LWch := Rd . GetWideChar ( GRd ) 
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
        ; RETURN 
      END (* EXCEPT *) 
    ; IF LWch # ExpWch 
      THEN 
        WL ( "Got " & WchImage ( LWch )  
             & ", but expected " & WchImage ( ExpWch ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # 2 
      THEN 
        WL ( "GetWideChar consumed " & IntImage ( LConsumed ) 
             & ", but it should always be 2." 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; IF ExpRaiseEndOfFile 
      THEN 
        WL ( "GetWideChar did not raise Rd.EndOfFile, when expected." ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; INC ( Counts [ KindTyp . GetWideChar , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetWideChar 

; PROCEDURE TestGetWideChar ( ) 

  = BEGIN 
      OpenPipe ( ) 
    ; MakeWCRLFPostponed ( ) 
    ; ExpectGetWideChar ( WchCR ) 
    ; ExpectGetWideChar ( WchLF ) 
    ; MakeWPostponed ( WchA ) 
    ; ExpectGetWideChar ( WchA ) 
    ; MakeWPostponed ( WchMax16 ) 
    ; ExpectGetWideChar ( WchMax16 ) 
    ; WriteWChar ( WchB ) 
    ; ExpectGetWideChar ( WchB ) 
    ; WriteWChar ( WchMax16 ) 
    ; ExpectGetWideChar ( WchMax16 ) 
    ; WriteClose ( ) 
    ; ExpectEOF ( KindTyp . GetWideChar ) 
    ; ExpectGetWideChar ( WchNull , ExpRaiseEndOfFile := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideChar 

; CONST WcharsNumber = 10 
; CONST WcharsLast = WcharsNumber - 1 
; TYPE ArrWch = ARRAY [ 0 .. WcharsLast ] OF Widechar 

; PROCEDURE ExpectGetWideSub 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrWch
  ; VAR LExpArr : ArrWch 
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; LCt := Rd . GetWideSub ( GRd , SUBARRAY ( LBuff , 0, BuffSize ) )
    ; IF LCt # Consumed 
      THEN 
        WL ( "GetWideSub returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt *2  
      THEN 
        WL ( "GetWideSub consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArr 
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LBuff ) TO BuffSize - 1  
      DO 
        IF LExpArr [ RI ] # WchNull 
        THEN 
          IF LBuff [ RI ] # LExpArr [ RI ] 
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & WchImage ( LBuff [ RI ] )  
                 & "from GetWideSub, but expected " 
                 & WchImage ( LExpArr [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
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

    ; INC ( Counts [ KindTyp . GetWideSub , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetWideSub 

; PROCEDURE TestGetWideSub ( ) 

  = BEGIN 
      OpenPipe ( ) 

    ; MakeWCRLFPostponed ( ) 
    ; ExpectGetWideSub ( 0 , 0 ) 
    ; ExpectGetWideSub ( 2 , 2 , WchCR , WchLF ) 

    ; MakeWCRLFPostponed ( ) 
    ; ExpectGetWideSub ( 1 , 1 , WchCR ) 
    ; ExpectGetWideSub ( 1 , 1 , WchLF ) 

    ; MakeWCRLFPostponed ( ) 
    ; WriteWChars ( WchA ) 
    ; ExpectGetWideSub ( 1 , 1 , WchCR ) 
    ; ExpectGetWideSub ( 2 , 2 , WchLF , WchA ) 

    ; WriteWChars ( WchLF , WchB ) 
    ; ExpectGetWideSub ( 2 , 2 , WchLF , WchB ) 

    ; WriteWChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetWideSub ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetWideSub ( 1 , 1 , WchLF ) 

    ; WriteWChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetWideSub ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakeWPostponed ( WchI ) 
    ; ExpectGetWideSub ( 1 , 1 , WchI ) 
    ; MakeWPostponed ( WchMax16 ) 
    ; ExpectGetWideSub ( 1 , 1 , WchMax16 ) 

    ; WriteWChar ( WchJ ) 
    ; ExpectGetWideSub ( 1 , 1 , WchJ ) 
    ; WriteWChar ( WchMax16 ) 
    ; ExpectGetWideSub ( 1 , 1 , WchMax16 ) 

    ; WriteWChars ( WchK , WchL , WchM  ) 
    ; ExpectGetWideSub ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetWideSub ( 1 , 1 , WchM ) 

    ; WriteWChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSub ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetWideSub ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideSub

; TYPE ArrCh = ARRAY [ 0 .. WcharsLast ] OF CHAR  

; PROCEDURE ExpectGetSub 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrCh
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; LCt := Rd . GetSub ( GRd , SUBARRAY ( LBuff , 0 , BuffSize ) )

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetSub returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetSub consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch 
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LBuff ) TO BuffSize - 1  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          IF ORD ( LBuff [ RI ] ) # ORD ( LExpArrWch [ RI ] )  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & ChImage ( LBuff [ RI ] )  
                 & " from GetSub, but expected " 
                 & ChImage ( VAL ( ORD ( LExpArrWch [ RI ] ) , CHAR ) ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
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

    ; INC ( Counts [ KindTyp . GetSub , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetSub 

; PROCEDURE TestGetSub ( ) 

  = BEGIN 
      OpenPipe ( ) 

    ; WriteChars ( WchA , WchB , WchMax8 ) 
    ; ExpectGetSub ( 3 , 3 , WchA , WchB , WchMax8 )

    ; MakePostponed ( WchMax8 ) 
    ; ExpectGetSub  ( 1 , 1 , WchMax8 ) 

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
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrWch
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; LCt := Rd . GetWideSubLine ( GRd , SUBARRAY ( LBuff , 0, BuffSize ) )

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetWideSubLine returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed DIV 2 # LCt 
      THEN 
        WL ( "GetWideSubLine consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

     ; LExpArrWch 
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LBuff ) TO BuffSize - 1  
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

    ; INC ( Counts [ KindTyp . GetWideSubLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetWideSubLine 

; PROCEDURE TestGetWideSubLine ( ) 

  = BEGIN 
      OpenPipe ( ) 

    ; WriteWChars ( WchA , WchB , WchMax16 ) 
    ; ExpectGetWideSubLine ( 3 , 3 , WchA , WchB , WchMax16 )

    ; MakeWPostponed ( WchMax16 ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchMax16 ) 

    ; MakeWCRLFPostponed ( ) 
    ; ExpectGetWideSubLine ( 0 , 0 ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchCR ) 
    ; ExpectGetWideSubLine ( 2 , 1 , WchLF ) 

    ; MakeWCRLFPostponed ( ) 
    ; ExpectGetWideSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; MakeWCRLFPostponed ( ) 
    ; WriteWChars ( WchA ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchCR , WchLF ) 
    (* We can't read up to EOF with our intermittent pipe, without closing. *)
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 2 , 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; MakeWCRLFPostponed ( ) 
    ; WriteWChars ( WchA ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 4 , 2 , WchCR , WchLF , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteWChars ( WchLF , WchB ) 
    ; ExpectGetWideSubLine ( 4 , 1 , WchLF ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchB ) 

    ; WriteWChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetWideSubLine ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetWideSubLine ( 4 , 1 , WchLF ) 

    ; WriteWChars ( WchF , WchG , WchH , WchCR , WchLF ) 
    ; ExpectGetWideSubLine ( 5 , 5 , WchF , WchG , WchH , WchCR , WchLF ) 

    ; MakeWPostponed ( WchI ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchI ) 

    ; WriteWChars ( WchJ , WchK ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchJ , WchK ) 

    ; WriteWChars ( WchK , WchL , WchM  ) 
    ; ExpectGetWideSubLine ( 2 , 2 , WchK , WchL ) 
    ; ExpectGetWideSubLine ( 1 , 1 , WchM ) 

    ; WriteWChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetWideSubLine ( 4 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetWideSubLine ( 1 , 0 , ExpEOF := TRUE ) 
    ; ReadClose ( ) 
    END TestGetWideSubLine

; PROCEDURE ExpectGetSubLine 
    ( BuffSize : [ 0 .. WcharsNumber ] 
    ; Consumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LCt : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LBuff : ArrCh
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; LCt := Rd . GetSubLine ( GRd , SUBARRAY ( LBuff , 0, BuffSize ) )

    ; IF LCt # Consumed 
      THEN 
        WL ( "GetSubLine returned " & IntImage ( LCt ) 
             & ", expected " & IntImage ( Consumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LCt 
      THEN 
        WL ( "GetSubLine consumed " & IntImage ( LConsumed ) 
             & ", but returned " & IntImage ( LCt ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch  
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LBuff ) TO BuffSize - 1  
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          IF ORD ( LBuff [ RI ] ) # ORD ( LExpArrWch [ RI ] )  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & ChImage ( LBuff [ RI ] )  
                 & " from GetSubLine, but expected " 
                 & ChImage ( VAL ( ORD ( LExpArrWch [ RI ] ) , CHAR ) ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
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

    ; INC ( Counts [ KindTyp . GetSubLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetSubLine 

; PROCEDURE TestGetSubLine ( ) 

  = BEGIN 
      OpenPipe ( ) 

    ; WriteChars ( WchA , WchB , WchMax8 ) 
    ; ExpectGetSubLine ( 3 , 3 , WchA , WchB , WchMax8 )

    ; MakePostponed ( WchMax8 ) 
    ; ExpectGetSubLine ( 1 , 1 , WchMax8 ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSubLine ( 0 , 0 ) 
    ; ExpectGetSubLine ( 1 , 1 , WchCR ) 
    ; ExpectGetSubLine ( 2 , 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetSubLine ( 4 , 2 , WchCR , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetSubLine ( 2 , 2 , WchCR , WchLF ) 
    (* We can't read up to EOF with our intermittent pipe, without closing. *)
    ; WriteClose ( ) 
    ; ExpectGetSubLine ( 2 , 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; WriteClose ( ) 
    ; ExpectGetSubLine ( 4 , 2 , WchCR , WchLF , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetSubLine ( 4 , 1 , WchLF ) 
    ; ExpectGetSubLine ( 1 , 1 , WchB ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetSubLine ( 4 , 4 , WchC , WchD , WchE , WchCR ) 
    ; ExpectGetSubLine ( 4 , 1 , WchLF ) 

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
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LLen : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LText : TEXT 
  ; VAR LWch : Widechar 
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; LText := Rd . GetText ( GRd , Len ) 

    ; LLen := Text . Length ( LText ) 
    ; IF LLen # ExpLen 
      THEN 
        WL ( "GetText" & " returned a text of length " 
             & IntImage ( LLen ) 
             & ", expected " & IntImage ( ExpLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # LLen 
      THEN 
        WL ( "GetText" & " consumed " & IntImage ( LConsumed ) 
             & ", but returned text of length" & IntImage ( LLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch 
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LExpArrWch ) TO MIN ( LLen - 1 , LAST ( LExpArrWch ) ) 
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

    ; INC ( Counts [ KindTyp . GetText , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetText 

; PROCEDURE TestGetText ( ) 

  = BEGIN 
      OpenPipe ( ) 

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
    ; MakePostponed ( WchMax8 ) 
    ; ExpectGetText ( 1 , 1 , WchMax8 ) 

    ; WriteChar ( WchJ ) 
    ; ExpectGetText ( 1 , 1 , WchJ ) 
    ; WriteChar ( WchMax8 ) 
    ; ExpectGetText ( 1 , 1 , WchMax8 ) 

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
    ; ExpConsumed : [ 0 .. WcharsNumber ] 
    ; Wch1 , Wch2 , Wch3 , Wch4 , Wch5 , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
      : Widechar := WchNull 
    ; ExpEOF : BOOLEAN := FALSE 
    ; ExpEndOfFile : BOOLEAN := FALSE 
    ) 

  = VAR LOldIndex , LNewIndex , LConsumed , LLen : CARDINAL 
  ; VAR LSucceeded : BOOLEAN 
  ; VAR LText : TEXT 
  ; VAR LWch : Widechar 
  ; VAR LExpArrWch : ArrWch  
  ; VAR LEOF : BOOLEAN 

  ; <* FATAL Rd . Failure , Wr . Failure , Alerted *> 
    BEGIN
      LSucceeded := TRUE (* May change. *) 
    ; LOldIndex := Rd . Index ( GRd ) 
    ; TRY 
        LText := Rd . GetLine ( GRd ) 
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
        WL ( "GetLine returned a text of length " 
             & IntImage ( LLen ) 
             & ", expected " & IntImage ( ExpLen ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LNewIndex := Rd . Index ( GRd ) 
    ; LConsumed := LNewIndex - LOldIndex 
    ; IF LConsumed # ExpConsumed  
      THEN 
        WL ( "GetLine consumed " & IntImage ( LConsumed ) 
             & ", but returned text of length" & IntImage ( ExpConsumed ) 
           ) 
      ; LSucceeded := FALSE 
      END (* IF *) 

    ; LExpArrWch 
        := ArrWch 
             { Wch1 , Wch2 , Wch3 , Wch4 , Wch5 
             , Wch6 , Wch7 , Wch8 , Wch9 , Wch10 
             } 
    ; FOR RI := FIRST ( LExpArrWch ) 
          TO MIN ( LLen - 1 , LAST ( LExpArrWch ) )   
      DO 
        IF LExpArrWch [ RI ] # WchNull 
        THEN 
          LWch := VAL ( ORD ( Text . GetWideChar ( LText , RI ) ) , Widechar )
        ; IF LWch # LExpArrWch [ RI ]  
          THEN 
            WL ( "Char position " & IntImage ( RI )
                 & ", got " & WchImage ( LWch )  
                 & " from " & "GetLine" & ", but expected " 
                 & WchImage ( LExpArrWch [ RI ] ) 
               ) 
          ; LSucceeded := FALSE 
          END (* IF *) 
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

    ; INC ( Counts [ KindTyp . GetLine , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    ; Wr . Flush ( Stdio . stdout ) 
    END ExpectGetLine 

; PROCEDURE TestGetLine ( ) 

  = BEGIN 
      OpenPipe ( ) 

    ; WriteChars ( WchA , WchB , WchMax8 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , 3 , WchA , WchB , WchMax8 , ExpEOF := TRUE )
    ; OpenPipe ( ) 

    ; MakePostponed ( WchMax8 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , 1 , WchMax8 , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; MakeCRLFPostponed ( ) 
    ; ExpectGetLine ( 0 , 2 ) 

    ; MakePostponed ( WchLF ) 
    ; ExpectGetLine ( 0 , 1 , WchLF ) 

    ; MakeCRLFPostponed ( ) 
    ; WriteChars ( WchA ) 
    ; ExpectGetLine ( 0 , 2 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , 1 , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChar ( WchCR ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , 1 , WchCR , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchCR , WchA ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 2 , 2 , WchCR , WchA , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchLF , WchNEL , WchVT , WchFF ) 
    ; ExpectGetLine ( 0 , 1 ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , 3 , WchNEL , WchVT , WchFF , ExpEOF := TRUE )
    ; OpenPipe ( ) 

    ; WriteChars ( WchLF , WchB ) 
    ; ExpectGetLine ( 0 , 1 , WchLF ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , 1 , WchB , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchC , WchD , WchE , WchCR , WchLF ) 
    ; ExpectGetLine ( 3 , 5 , WchC , WchD , WchE ) 

    ; MakePostponed ( WchI ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 1 , 1 , WchI , ExpEOF := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchK , WchL , WchM  ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , 3 , WchK , WchL , WchM , ExpEOF := TRUE ) 
    ; ExpectGetLine ( 0 , 0 , ExpEndOfFile := TRUE ) 
    ; OpenPipe ( ) 

    ; WriteChars ( WchN , WchO , WchP  ) 
    ; WriteClose ( ) 
    ; ExpectGetLine ( 3 , 3 , WchN , WchO , WchP , ExpEOF := TRUE ) 
    ; ExpectGetLine ( 0 , 0 , ExpEndOfFile := TRUE ) 

    ; ReadClose ( ) 
    END TestGetLine

; VAR GIndex : CARDINAL 

; PROCEDURE Fwd ( Ct : CARDINAL ) 

  = VAR LSucceeded : BOOLEAN := TRUE (* May change. *) 
  ; VAR LCh : CHAR 
  ; VAR LIndex : CARDINAL 

  ; <* FATAL Rd . Failure , Rd . EndOfFile , Alerted *> 
    BEGIN 
      FOR RRdCt := 1 TO Ct 
      DO 
        LCh := Rd . GetChar ( GRd ) 
      ; IF ORD ( LCh ) # GIndex 
        THEN 
          WL ( "UnGetCharMulti, got char " & ChImage ( LCh ) 
               & " at GIndex " & IntImage ( GIndex ) 
             ) 
        ; LSucceeded := FALSE 
        END (* IF *) 
      ; INC ( GIndex ) 
      ; GUnGotCt := MAX ( 0 , GUnGotCt - 1 ) 
      ; LIndex := Rd . Index ( GRd ) 
      ; IF LIndex # GIndex 
        THEN 
          WL ( "UnGetCharMulti-Fwd, Rd.Index = " & IntImage ( LIndex ) 
               & ", but Test.GIndex = " & IntImage ( GIndex ) 
             ) 
        ; LSucceeded := FALSE 
        END (* IF *) 
      END (* FOR *)   
    ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    END Fwd

; PROCEDURE Back ( Ct : CARDINAL ) 

  = VAR LSucceeded : BOOLEAN := TRUE (* May change. *) 
  ; VAR VAR LToUnGetCt : CARDINAL 
  ; VAR LUgSucceeded : BOOLEAN 
  ; VAR LCh : CHAR 

  ; <* FATAL Rd . Failure , Rd . EndOfFile , Alerted *> 
    BEGIN 
      LToUnGetCt := MIN ( Ct , MIN ( GIndex , Rd . UnGetCapacity ) )  
    ; FOR RI := 1 TO LToUnGetCt 
      DO
        LUgSucceeded := Rd . UnGetCharMulti ( GRd ) = 1 
      ; IF LUgSucceeded 
        THEN
          DEC ( GIndex ) 
        ; INC ( GUnGotCt ) 
        ELSE  
          WL ( "UnGetCharMulti failed expectedly at GIndex " 
               & IntImage ( GIndex ) 
               & " GUnGotCt " & IntImage ( GUnGotCt ) 
             ) 
        ; LSucceeded := FALSE 
        END (* IF *) 
      END (* FOR *) 
    (* This should be one too many: *) 
    ; LUgSucceeded := Rd . UnGetCharMulti ( GRd ) = 1 
    ; IF LUgSucceeded 
      THEN 
        LCh := Rd . GetChar ( GRd )
      ; <* ASSERT GUnGotCt > 0 *> 
        WL ( "More than promised UnGetCharMulti at Index "
             & IntImage ( GIndex ) & ", which is OK. "
           )  
      ELSE (* We really expected this. *) 
      END (* IF *)  
    ; INC ( Counts [ KindTyp . UnGetCharMulti , LSucceeded ] )  
    ; INC ( Counts [ KindTyp . Total , LSucceeded ] )  
    END Back

; PROCEDURE Cycle ( Ct : CARDINAL ) 

  = BEGIN 
      Fwd ( Ct ) 
    ; Back ( Ct ) 
    ; Fwd ( Ct ) 
    END Cycle

; PROCEDURE Advance ( Ct : CARDINAL ) 

  = BEGIN 
      Fwd ( Ct ) 
    ; Back ( Ct ) 
    ; Fwd ( Ct + 1 ) 
    END Advance

; VAR GUnGotCt : CARDINAL  

; PROCEDURE TestUnGetCharMulti ( ) 

  = <* FATAL Rd . Failure , Alerted *> 
    BEGIN
      GRd := SmallBuffRd . New ( )  
    ; GIndex := 0 
    ; GUnGotCt := 0 
    ; Back ( 0 ) 
    ; FOR RI := 1 TO 10 
      DO Cycle ( RI ) 
      END (* FOR *) 
    ; FOR RI := 1 TO 10 DO Advance ( 1 ) END (* FOR *) 
    ; FOR RI := 1 TO 10 DO Advance ( 2 ) END (* FOR *) 
    ; FOR RI := 1 TO 10 DO Advance ( 3 ) END (* FOR *) 
    ; FOR RI := 1 TO 10 DO Advance ( 4 ) END (* FOR *) 
    ; Rd . Close ( GRd ) 
    END TestUnGetCharMulti 

; PROCEDURE Work ( ) 

  = BEGIN 
      Counts := CountsTyp { ZeroOutcomes , .. }  
    ; TestGetWideChar ( ) 
    ; TestGetWideSub ( ) 
    ; TestGetSub ( ) 
    ; TestGetWideSubLine ( ) 
    ; TestGetSubLine ( ) 
    ; TestGetText ( ) 
    ; TestGetLine ( ) 
    ; TestUnGetCharMulti ( )
    ; DumpCounts ( ) 
    END Work 

; BEGIN (* TestRd *) 
    Work ( ) 
  END TestRd 
. 
