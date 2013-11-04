UNSAFE MODULE PickleTestWideChar EXPORTS Main 

; IMPORT Fingerprint
; IMPORT Fmt
; IMPORT IO 
; IMPORT Params 
; IMPORT Pickle2 AS Pickle 
; IMPORT Rd 
; IMPORT RTType 
; IMPORT RTTypeFP 
; IMPORT RTTypeSRC 
; IMPORT RT0
; IMPORT Stdio 
; IMPORT Text , TextLiteral , Text16 , Text16Short , Text8 , Text8CString 
         , Text8Short , TextCat , TextSub , TextClass 
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT LibDep
; IMPORT WcDep 

; CONST IntPad = BITSIZE ( INTEGER ) DIV 8 * 2 (* Hex digits per byte. *) 

; VAR LogWrT : Wr . T 
; VAR PWrT : Wr . T 

; VAR GTestCt : CARDINAL := 0 
; VAR GFailureCt : CARDINAL := 0 
; VAR GWriteWas16Ct : INTEGER := 0 
; VAR GWriteWas21Ct : INTEGER := 0  

; VAR DefaultFileName : TEXT := "PickleTestWideChar.pkl"
; VAR FileName : TEXT := DefaultFileName 
; VAR FileNameSeen : BOOLEAN := FALSE 

; VAR OptionSeen : BOOLEAN := FALSE 

; VAR DoShowFPs : BOOLEAN := FALSE   
; VAR DoShowRefTypes : BOOLEAN := FALSE   
; VAR DoWrite : BOOLEAN := FALSE   
; VAR DoRead : BOOLEAN := FALSE   
; VAR DoDisplayVersion : BOOLEAN := FALSE   
; VAR DoDisplayHelp : BOOLEAN := FALSE   

; VAR DoWriteT : BOOLEAN := FALSE
; VAR DoWriteTPA : BOOLEAN := FALSE 
; VAR DoWriteTAA : BOOLEAN := FALSE  
; VAR DoWriteTA : BOOLEAN := FALSE 
; VAR DoWriteTOA : BOOLEAN := FALSE 
; VAR DoWriteRecTexts : BOOLEAN := FALSE 
; VAR DoWriteTopTexts : BOOLEAN := FALSE 
; VAR Bit16 : BOOLEAN := FALSE

; PROCEDURE SetDoAll ( ) 

  = BEGIN 
      DoWrite := TRUE
    ; DoWriteT := TRUE
    ; DoWriteTPA := TRUE 
    ; DoWriteTAA := TRUE  
    ; DoWriteTA := TRUE 
    ; DoWriteTOA := TRUE 
    ; DoWriteRecTexts := TRUE 
    ; DoWriteTopTexts := TRUE 
    ; DoRead := TRUE
    END SetDoAll 

; PROCEDURE GetParams ( ) 

  = VAR LParam : TEXT 
  ; VAR LChar : CHAR 

  ; BEGIN 
      FileName := DefaultFileName 
    ; FileNameSeen := FALSE 
    ; OptionSeen := FALSE 
    ; DoShowFPs := FALSE 
    ; DoShowRefTypes := FALSE 
    ; DoWrite := FALSE
    ; DoRead := FALSE
    ; DoDisplayVersion := FALSE
    ; DoDisplayHelp := FALSE
    ; DoWriteT := FALSE
    ; DoWriteTPA := FALSE 
    ; DoWriteTAA := FALSE  
    ; DoWriteTA := FALSE 
    ; DoWriteTOA := FALSE 
    ; DoWriteRecTexts := FALSE
    ; DoWriteTopTexts := FALSE  
    ; Bit16 := FALSE

    ; IF Params . Count < 2 
      THEN
        SetDoAll ( ) 
      ELSE 
        FOR RParamNo := 1 TO Params . Count - 1 
        DO
          LParam := Params . Get ( RParamNo ) 
        ; IF Text . GetChar ( LParam , 0 ) = '-' 
          THEN
            OptionSeen := TRUE 
          ; FOR RCharNo := 1 TO Text . Length ( LParam ) - 1  
            DO
              LChar := Text . GetChar ( LParam , RCharNo ) 
            ; CASE LChar 
              OF 'a' => SetDoAll ( ) 
              | 'c' => DoWriteT := TRUE 
              | 'f' => DoShowFPs := TRUE 
              | 'g' => DoShowRefTypes := TRUE 
              | 'h' => DoDisplayHelp := TRUE 
              | 'o' => DoWriteTOA := TRUE 
              | 'p' => DoWriteTPA := TRUE 
              | 'P' => DoWriteTAA := TRUE 
              | 'r' => DoRead := TRUE  
              | 't' => DoWriteRecTexts := TRUE  
              | 'T' => DoWriteTopTexts := TRUE  
              | 'v' => DoDisplayVersion := TRUE 
              | 'w' => DoWrite := TRUE 
              | 'y' => DoWriteTA := TRUE  
              ELSE 
                WL ( "Invalid option character: \'" 
                     & Text . FromChar ( LChar )  & "\'"  
                   ) 
              ; DoDisplayHelp := TRUE 
              END 
            END (* FOR *) 
          ELSIF Text . Equal ( LParam , "?" ) 
          THEN DoDisplayHelp := TRUE 
          ELSIF NOT FileNameSeen 
          THEN 
            FileName := LParam 
          ; FileNameSeen := TRUE 
          ELSE
            WL ( "Extra parameter: \"" & LParam & "\"" ) 
          ; DoDisplayHelp := TRUE 
          END (* IF *)  
        END (* FOR *)  
      ; IF NOT OptionSeen
        THEN SetDoAll ( ) 
        END (* IF *) 
      END (* IF *) 
    END GetParams 

; PROCEDURE WL ( T : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , T ) 
    ; Wr . PutText ( PWrT , Wr . EOL ) 
    END WL 

; PROCEDURE DisplayHelp ( ) 

  = VAR LName : TEXT 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      LName := Params . Get ( 0 ) 
    ; WL ( "Usage: " & LName & " {-{option}} <FileName> " ) 
    ; WL ( "  <FileName> is the name of pickle file to write and/or read." ) 
    ; WL ( "    (Default file name is \"" & DefaultFileName & " \"." ) 
    ; WL ( "  Options are: " ) 
    ; WL ( "  -a or empty options:" ) 
    ; WL ( "     Write (almost) all and read: -w, -c, -o, -p, -P, -t, -T, -y, -r." ) 
    ; WL ( "  -c Include a record including some WIDECHAR fields." ) 
    ; WL ( "  -f Show fingerprints for certain builtin types." ) 
    ; WL ( "  -g Show fingerprints for all reference types." ) 
    ; WL ( "  -o Include an open array of WIDECHARs." ) 
    ; WL ( "  -p Include a packed-21 array of one WIDECHAR." ) 
    ; WL ( "  -P Include a packed-32 array of WIDECHARs." ) 
    ; WL ( "  -h, -? Display help text and exit." ) 
    ; WL ( "  -t Include a record with several WIDECHARs." ) 
    ; WL ( "  -T Include several top-level TEXTs." ) 
    ; WL ( "  -y Include a fixed array of TEXTs." ) 
    ; WL ( "  -r Read a previously written file of pickles and verify its contents." ) 
    ; WL ( "  -v Display version and exit." ) 
    ; WL ( "  -w Write a file containing pickles. (done before any reading.)" ) 
    ; Wr . Flush ( PWrT ) 
    END DisplayHelp 

; PROCEDURE DisplayVersion ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      WL ( "Test program for pickling full Unicode WIDECHAR, version 1.0" ) 
    ; Wr . Flush ( PWrT ) 
    END DisplayVersion  

; PROCEDURE PI ( Val : INTEGER )

  = BEGIN
      Wr . PutText ( LogWrT , "16_" )
    ; Wr . PutText
        ( LogWrT
        , Fmt . Pad ( Fmt . Unsigned ( Val ) , IntPad , '0' )
        )
    END PI

; PROCEDURE PFP ( READONLY FP : Fingerprint . T ) 

  = VAR I : INTEGER 

  ; BEGIN 
      I := 0 
    ; Wr . PutText ( LogWrT , "{" )  
    ; LOOP 
        Wr . PutText ( LogWrT , "16_" )  
      ; Wr . PutText 
          ( LogWrT 
          , Fmt . Pad ( Fmt . Unsigned ( FP . byte [ I ] ) , 2 , '0' ) 
          )   
      ; IF I >= LAST ( FP . byte ) 
        THEN EXIT
        ELSE 
          Wr . PutText ( LogWrT , "," )  
        ; INC ( I ) 
        END (* IF *) 
      END (* LOOP *) 
    ; Wr . PutText ( LogWrT , "}" )  
    END PFP 

; PROCEDURE ShowFP ( Label : TEXT ; TC : INTEGER ) 

  = VAR FP : Fingerprint . T 
  ; VAR TDefn : RT0 . TypeDefn 

  ; BEGIN 
      FP := RTTypeFP . ToFingerprint ( TC ) 
    ; Wr . PutText ( LogWrT , "Fingerprint of " )  
    ; Wr . PutText ( LogWrT , Label )  
    ; Wr . PutText ( LogWrT , " = " )  
    ; PFP ( FP ) 
    ; TDefn := RTType . Get ( TC ) 
    ; Wr . PutText ( LogWrT , ", UID = " ) 
    ; PI ( TDefn ^ . selfID ) 
    ; Wr . PutText ( LogWrT , Wr . EOL )  
    END ShowFP 

; PROCEDURE PType ( TC : INTEGER (* TYPECODE *) ) 

  = VAR TDefn : RT0 . TypeDefn 

  ; BEGIN 
      Wr . PutText ( LogWrT , Fmt . Pad ( Fmt . Int ( TC ) , 5 ) )  
    ; IF 0 <= TC AND TC <= RTType.MaxTypecode ( ) 
      THEN 
        TDefn := RTType . Get ( TC ) 
      ; Wr . PutText ( LogWrT , " 16_" )  
      ; Wr . PutText 
          ( LogWrT 
          , Fmt . Pad ( Fmt . Unsigned ( TDefn ^ . selfID ) , IntPad , '0' )
          ) 
      ; Wr . PutText ( LogWrT , " " )  
      ; PFP ( LOOPHOLE ( TDefn ^ . fp , Fingerprint . T ) )  
      ; Wr . PutText ( LogWrT , " " )  
      ; Wr . PutText ( LogWrT , RTTypeSRC . TypecodeName ( TC ) )  
      ; Wr . PutText ( LogWrT , " " )  
      END (* IF *)       
    ; Wr . PutText ( LogWrT , Wr . EOL )  
    END PType 

; PROCEDURE ShowAllRefTypes ( ) 

  = BEGIN 
      IF DoShowRefTypes 
      THEN 
        Wr . PutText ( LogWrT , "Reference typecodes and their fingerprints:" )
      ; Wr . PutText ( LogWrT , Wr . EOL )  
      ; FOR I := 0 TO RTType.MaxTypecode ( )
        DO
          PType ( I ) 
        END (* FOR *) 
      ; Wr . Flush ( LogWrT ) 
      END (* IF *) 
    END ShowAllRefTypes 

; PROCEDURE WriteAdj ( Val : INTEGER ) : INTEGER 

  = VAR LResult : INTEGER 

  ; BEGIN 
      IF ORD ( LAST ( WIDECHAR ) ) = 16_FFFF 
         AND Val > 16_FFFF
      THEN LResult := Val - 16_100000
      ELSE LResult := Val 
      END (* IF *) 
    ; RETURN LResult 
    END WriteAdj

; PROCEDURE WriteAdjWc ( Val : INTEGER ) : WIDECHAR  

  = BEGIN 
      RETURN VAL ( WriteAdj ( Val ) , WIDECHAR ) 
    END WriteAdjWc

; PROCEDURE NoteWriteWas16 ( ) 

  = BEGIN 
      INC ( GWriteWas16Ct )  
    END NoteWriteWas16 

; PROCEDURE NoteWriteWas21 ( ) 

  = BEGIN 
      INC ( GWriteWas21Ct )  
    END NoteWriteWas21 

; PROCEDURE WcIntsAsExpected ( Got : INTEGER ; VAR Exp : INTEGER ) : BOOLEAN 

  = VAR LResult : BOOLEAN 

  ; BEGIN 
      IF Exp > 16_FFFF 
      THEN (* Expected value outside of 16-bit WIDECHAR range. *)  
        IF Got = Exp - 16_100000
        THEN
          NoteWriteWas16 ( )  
        ; Exp := Exp - 16_100000
        ; LResult := TRUE 
        ELSIF ORD ( LAST ( WIDECHAR ) ) = 16_FFFF 
        THEN (* We are reading on a 16-bit WIDECHAR system. *) 
          Exp := 16_FFFD 
        ; IF Got = 16_FFFD (* Unicode substitution char. *) 
          THEN (* Hi value was substituted during read. *) 
            NoteWriteWas21 ( )  
          ; LResult := TRUE 
          ELSE LResult := FALSE 
          END (* IF *) 
        ELSE (* Reading on Unicode-WIDECHAR system. *) 
          IF Got = Exp 
          THEN 
            NoteWriteWas21 ( )  
          ; LResult := TRUE 
          ELSE LResult := FALSE 
          END (* IF *) 
        END (* IF *) 
      ELSE 
        LResult := Got = Exp 
      END (* IF *) 
    ; RETURN LResult 
    END WcIntsAsExpected 

; PROCEDURE CheckInt 
    ( Name : TEXT ; Got , Exp : INTEGER ; IsWc : BOOLEAN := TRUE ) 

  = VAR OK : BOOLEAN 

  ; BEGIN 
      Wr . PutText ( LogWrT , Name )  
    ; Wr . PutText ( LogWrT , " = " )  
    ; PI ( Got ) 
    ; Wr . PutText ( LogWrT , "=" )  
    ; Wr . PutText ( LogWrT , Fmt . Int ( Got ) )  
    ; IF IsWc 
      THEN OK := WcIntsAsExpected ( Got , Exp ) 
      ELSE OK := Got = Exp
      END (* IF *) 
    ; IF OK 
      THEN 
        Wr . PutText ( LogWrT , ", as expected." )  
      ELSE 
        Wr . PutText ( LogWrT , ", EXPECTED: " )  
      ; PI ( Exp ) 
      ; Wr . PutText ( LogWrT , "=" )  
      ; Wr . PutText ( LogWrT , Fmt . Int ( Exp ) )
      ; INC ( GFailureCt )  
      END (* IF *) 
    ; Wr . PutText ( LogWrT , Wr . EOL )  
    ; INC ( GTestCt ) 
    END CheckInt 

; TYPE T 
  = RECORD 
      A : BITS 8 FOR [ 0 .. 255 ] 
    ; B : (* BITS 32 FOR *) INTEGER 
    ; D : [ 0 .. 255 ] 
    ; E : [ -128 .. 127] 
    ; F : [ 0 .. 65535 ] 
    ; G : [ - 32768 .. 32767 ] 
    ; H : [ 0 .. 16_7FFFFFFF ] 
    ; I : WIDECHAR 
    ; Pad : INTEGER 
    ; J : BITS 21 FOR WIDECHAR 
    END

; TYPE RefT = REF T 

; CONST AVal = 254
; CONST BVal = ( ( 1 * 256 + 2 ) * 256 + 4 ) * 256 + 8  
; CONST DVal = 237
; CONST EVal = - 94
; CONST FVal = 42782
; CONST GVal = - 26677
; CONST HVal = 16_0f4dc379
; CONST IVal = 16_10FFFF
; CONST JVal = IVal 

; PROCEDURE InitT ( ) : RefT 

  = VAR R : RefT 

  ; BEGIN 
      R := NEW ( RefT ) 
    ; R . A := AVal 
    ; R . B := BVal 
    ; R . D := DVal 
    ; R . E := EVal 
    ; R . F := FVal 
    ; R . G := GVal 
    ; R . H := HVal 
    ; R . I := WriteAdjWc ( IVal ) 
    ; R . J := WriteAdjWc ( JVal ) 
    ; RETURN R 
    END InitT  

; PROCEDURE CheckT ( R : RefT ) 

  = BEGIN 
      CheckInt ( "A" , R . A , AVal , IsWc := FALSE ) 
    ; CheckInt ( "B" , R . B , BVal , IsWc := FALSE ) 
    ; CheckInt ( "D" , R . D , DVal , IsWc := FALSE ) 
    ; CheckInt ( "E" , R . E , EVal , IsWc := FALSE ) 
    ; CheckInt ( "F" , R . F , FVal , IsWc := FALSE ) 
    ; CheckInt ( "G" , R . G , GVal , IsWc := FALSE ) 
    ; CheckInt ( "H" , R . H , HVal , IsWc := FALSE ) 
    ; CheckInt ( "ORD(I)" , ORD ( R . I ) , ORD ( IVal ) ) 
    ; CheckInt ( "ORD(J)" , ORD ( R . J ) , ORD ( JVal ) ) 
    ; Wr . Flush ( LogWrT ) 
    END CheckT   

(* Packed array of fixed number of WIDECHAR, with odd packing size. *) 

; CONST BitsPerPackedWC = 21 
(* ; CONST WcPerInt = BITSIZE(INTEGER) DIV BitsPerPackedWC 
   ^This produces a type that changes between 32- and 64-bit machines, 
   and thus pickles that can't be interchanged. 
*) 
; CONST WcPerInt = 32 DIV BitsPerPackedWC 
; TYPE TPA = ARRAY [ 0 .. WcPerInt - 1 ] OF BITS BitsPerPackedWC FOR WIDECHAR 
; TYPE RefTPA = REF TPA 

; CONST PWVal = 16_10FFF8 

; PROCEDURE InitTPA ( ) : RefTPA 

  = VAR VTPA : RefTPA 
  ; VAR LVal : WIDECHAR 

  ; BEGIN 
      VTPA := NEW ( RefTPA )
    ; LVal := WriteAdjWc ( PWVal )  
    ; FOR RI := 0 TO WcPerInt - 1 
      DO   
        VTPA ^ [ RI ] := LVal 
      ; INC ( LVal ) 
      END (* FOR *) 
    ; RETURN VTPA 
    END InitTPA 

; PROCEDURE CheckTPA ( FRefTPA : RefTPA ) 

  = VAR LVal : INTEGER 

  ; BEGIN 
      LVal := PWVal 
    ; FOR RI := 0 TO WcPerInt - 1 
      DO   
        CheckInt 
          ( "ORD(TPA[" & Fmt . Int ( RI ) & "])" 
          , ORD ( FRefTPA ^ [ RI ] ) 
          , LVal  
          )  
      ; INC ( LVal ) 
      END (* FOR *) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTPA   

(* Packed array of WIDECHAR, with packing that fits a word. *) 
; CONST BitsPerPackedWC2 = 32
; CONST WcPACt = 19 
; TYPE TAA = ARRAY [ 0 .. WcPACt- 1 ] OF BITS BitsPerPackedWC2 FOR WIDECHAR 
; TYPE RefTAA = REF TAA 

; CONST WAVal = 16_10FF80 

; PROCEDURE InitTAA ( ) : RefTAA 

  = VAR VTAA : RefTAA 
  ; VAR LVal : WIDECHAR 

  ; BEGIN 
      VTAA := NEW ( RefTAA )
    ; LVal := WriteAdjWc ( WAVal ) 
    ; FOR RI := 0 TO WcPACt - 1 
      DO   
        VTAA ^ [ RI ] := LVal 
      ; INC ( LVal ) 
      END (* FOR *) 
    ; RETURN VTAA 
    END InitTAA 

; PROCEDURE CheckTAA ( FRefTAA : RefTAA ) 

  = VAR LVal : INTEGER 

  ; BEGIN 
      LVal := WAVal 
    ; FOR RI := 0 TO WcPACt - 1 
      DO   
        CheckInt 
          ( "ORD(TAA[" & Fmt . Int ( RI ) & "])" 
          , ORD ( FRefTAA ^ [ RI ] ) 
          , LVal  
          )  
      ; INC ( LVal ) 
      END (* FOR *) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTAA   

(* Plain array of WIDECHAR. *) 
; CONST WcCt = 13 
; TYPE TA = ARRAY [ 0 .. WcCt- 1 ] OF WIDECHAR 
; TYPE RefTA = REF TA 

; CONST WVal = 16_10FF00 

; PROCEDURE InitTA ( ) : RefTA 

  = VAR VTA : RefTA 
  ; VAR LVal : WIDECHAR 

  ; BEGIN 
      VTA := NEW ( RefTA )
    ; LVal := WriteAdjWc ( WVal )  
    ; FOR RI := 0 TO WcCt - 1 
      DO   
        VTA ^ [ RI ] := LVal 
      ; INC ( LVal ) 
      END (* FOR *) 
    ; RETURN VTA 
    END InitTA 

; PROCEDURE CheckTA ( FRefTA : RefTA ) 

  = VAR LVal : INTEGER 

  ; BEGIN 
      LVal := WVal 
    ; FOR RI := 0 TO WcCt - 1 
      DO   
        CheckInt 
          ( "ORD(TA[" & Fmt . Int ( RI ) & "])" 
          , ORD ( FRefTA ^ [ RI ] ) 
          , LVal  
          )  
      ; INC ( LVal ) 
      END (* FOR *) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTA   

(* Open array of WIDECHAR. *) 
; CONST WcPOCt = 15 
; TYPE TOA = ARRAY OF WIDECHAR 
; TYPE RefToA = REF TOA 

; CONST WOAVal = 16_10FF40 

; PROCEDURE InitTOA ( ) : RefToA 

  = VAR VTOA : RefToA 
  ; VAR LVal : WIDECHAR 

  ; BEGIN 
      VTOA := NEW ( RefToA , WcPOCt )
    ; LVal := WriteAdjWc ( WOAVal )  
    ; FOR RI := 0 TO LAST ( VTOA ^ ) 
      DO   
        VTOA ^ [ RI ] := LVal 
      ; INC ( LVal ) 
      END (* FOR *) 
    ; RETURN VTOA 
    END InitTOA 

; PROCEDURE CheckTOA ( FRefToA : RefToA ) 

  = VAR LVal : INTEGER 

  ; BEGIN 
      LVal := WOAVal 
    ; FOR RI := 0 TO LAST ( FRefToA ^ ) 
      DO   
        CheckInt 
          ( "ORD(TOA[" & Fmt . Int ( RI ) & "])" 
          , ORD ( FRefToA ^ [ RI ] ) 
          , LVal  
          )  
      ; INC ( LVal ) 
      END (* FOR *) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTOA   

(* TEXT. *) 
; TYPE Texts = RECORD 
      T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT 
    END
; TYPE RefToTexts = REF Texts 

; CONST Lit2 = "ABCDE" 
; CONST Lit3 = W"FGHIJK" 
; CONST Lit4 = W"LMNOPQRSTUVWXYZabcdefg" 
; CONST Lit5 = "hijk"
; CONST Lit6 = "lmnopqrstuvwxyz98765"
; CONST Lit7a = W"098765\xFFFF\xFFFC"
; CONST Lit7b = "qwertyuiopasdf"
; CONST Lit7 = Lit7a & Lit7b 

; TYPE RarrWC = REF ARRAY OF WIDECHAR 
; TYPE RarrC = REF ARRAY OF CHAR 

; PROCEDURE ArrayOfChar ( T : TEXT ) : RarrC

  = VAR LLen : CARDINAL := Text . Length ( T ) 
  ; VAR LResult : RarrC 

  ; BEGIN 
      LResult := NEW ( RarrC , LLen )
    ; Text . SetChars ( LResult ^ , T ) 
    ; RETURN LResult   
    END ArrayOfChar

; PROCEDURE MakeText8 ( T : TEXT ) : TEXT 
  (* Or possibly Text8Short, if T is short. *) 

  = VAR LRarrC : RarrC 
  ; VAR LResult : TEXT  

  ; BEGIN 
      LRarrC := ArrayOfChar ( T )
    ; LResult := Text8 . New ( LRarrC ^ )  
    ; RETURN LResult 
    END MakeText8 

; PROCEDURE MakeText8Short ( T : TEXT ) : TEXT 

  = VAR LRarrC : RarrC 
  ; VAR LResult : Text8Short . T 

  ; BEGIN 
      LRarrC := ArrayOfChar ( T )
    ; LResult := Text8Short . New ( LRarrC ^ )  
    ; RETURN LResult 
    END MakeText8Short 

; PROCEDURE ArrayOfWidechar ( T : TEXT ) : RarrWC

  = VAR LLen : CARDINAL := Text . Length ( T ) 
  ; VAR LResult : RarrWC 

  ; BEGIN 
      LResult := NEW ( RarrWC , LLen )
    ; Text . SetWideChars ( LResult ^ , T ) 
    ; RETURN LResult   
    END ArrayOfWidechar

; PROCEDURE MakeText16 ( T : TEXT ) : TEXT 
  (* Or possibly Text16Short, if T is short. *) 

  = VAR LRarrWC : RarrWC 
  ; VAR LResult : TEXT 

  ; BEGIN 
      LRarrWC := ArrayOfWidechar ( T )
    ; LResult := Text16 . New ( LRarrWC ^ )  
    ; RETURN LResult 
    END MakeText16 

; PROCEDURE MakeText16Short ( T : TEXT ) : TEXT 

  = VAR LRarrWC : RarrWC 
  ; VAR LResult : Text16Short . T 

  ; BEGIN 
      LRarrWC := ArrayOfWidechar ( T )
    ; LResult := Text16Short . New ( LRarrWC ^ )  
    ; RETURN LResult 
    END MakeText16Short 

; PROCEDURE MakeTextCat ( Left , Right : TEXT ) : TEXT 

  = VAR LRarrWC : RarrWC 
  ; VAR LRarrC : RarrC 
  ; VAR LLeft , LRight : TEXT  
  ; VAR LResult : TEXT 
        (* ^This could get flattened into a non-TextCat.T. *)   

  ; BEGIN 
      IF Text . HasWideChars ( Left ) 
      THEN 
        LRarrWC := ArrayOfWidechar ( Left ) 
      ; LLeft := Text . FromWideChars ( LRarrWC ^ ) 
      ELSE 
        LRarrC := ArrayOfChar ( Left ) 
      ; LLeft := Text . FromChars ( LRarrC ^ ) 
      END (* IF *) 
    ; IF Text . HasWideChars ( Right ) 
      THEN 
        LRarrWC := ArrayOfWidechar ( Right ) 
      ; LRight := Text . FromWideChars ( LRarrWC ^ ) 
      ELSE 
        LRarrC := ArrayOfChar ( Right ) 
      ; LRight := Text . FromChars ( LRarrC ^ ) 
      END (* IF *) 
    ; LibDep . SetTextClassDotFlatten ( FALSE )    
    ; LResult := TextCat . New ( LLeft , LRight ) 
    ; RETURN LResult 
    END MakeTextCat 

; PROCEDURE InitTexts ( ) : RefToTexts 

  = VAR VTexts : RefToTexts 

  ; BEGIN 
      VTexts := NEW ( RefToTexts )
    ; VTexts . T1 := WcDep . VLit1 
    ; VTexts . T2 := Lit2 
    ; VTexts . T3 := MakeText16Short ( Lit3 ) 
    ; VTexts . T4 := MakeText16 ( Lit4 ) 
    ; VTexts . T5 := MakeText8Short ( Lit5 ) 
    ; VTexts . T6 := MakeText8 ( Lit6 ) 
    ; VTexts . T7 := MakeTextCat ( Lit7a , Lit7b ) 
    ; RETURN VTexts 
    END InitTexts 

; PROCEDURE EscapeInt ( WrT : Wr . T ; Val : INTEGER ; IsWide : BOOLEAN ) 

  = VAR LPadCt : CARDINAL  

  ; BEGIN
      IF Val = ORD ( '\n' ) THEN Wr . PutText ( WrT , "\\\n" )  
      ELSIF Val = ORD ( '\t' ) THEN Wr . PutText ( WrT , "\\\t" )  
      ELSIF Val = ORD ( '\r' ) THEN Wr . PutText ( WrT , "\\\r" )  
      ELSIF Val = ORD ( '\f' ) THEN Wr . PutText ( WrT , "\\\f" )  
      ELSIF Val = ORD ( '\'' ) THEN Wr . PutText ( WrT , "\\\'" )  
      ELSIF Val = ORD ( '\"' ) THEN Wr . PutText ( WrT , "\\\"" )  
      ELSIF Val = ORD ( '\\' ) THEN Wr . PutText ( WrT , "\\\\" )
      ELSIF Val > 16_10FFFF 
      THEN 
        Wr . PutText ( WrT , "<outOfRange>16_" ) 
      ; Wr . PutText ( WrT , Fmt . Unsigned ( Val , 16 ) )    
      ELSIF Val > 16_FFFF 
      THEN 
        Wr . PutText ( WrT , "\\U" ) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Unsigned ( Val , 16 ) , 6 , '0' ) )    
      ELSIF Val > 126 OR Val < 32   
      THEN
        Wr . PutText ( WrT , "\\X" ) 
      ; IF IsWide THEN LPadCt := 4 ELSE LPadCt := 2 END (* IF *) 
      ; Wr . PutText 
          ( WrT , Fmt . Pad ( Fmt . Unsigned ( Val , 16 ) , LPadCt , '0' ) )    
      ELSE Wr . PutChar ( WrT , VAL ( Val , CHAR ) ) 
      END (* IF *) 
    END EscapeInt 

; PROCEDURE ArrImage ( Ref : REF ARRAY OF INTEGER ) : TEXT 

  = VAR LIsWide : BOOLEAN 
  ; VAR LI : INTEGER 
  ; VAR LResult : TEXT 
  ; VAR LWr : Wr . T 
 
  ; BEGIN 
      IF Ref = NIL 
      THEN LResult := "NIL" 
      ELSE 
        LI := FIRST ( Ref ^ ) 
      ; LOOP 
          IF LI > LAST ( Ref ^ ) 
          THEN 
            LIsWide := FALSE 
          ; EXIT 
          ELSIF Ref ^ [ LI ] > 16_FFFF 
          THEN 
            LIsWide := TRUE 
          ; EXIT 
          ELSE INC ( LI ) 
          END (* IF *) 
        END (* LOOP *) 
      ; LWr := TextWr . New ( ) 
      ; IF LIsWide
        THEN Wr . PutText ( LWr , "W" ) 
        END (* IF *) 
      ; Wr . PutText ( LWr , "\"" ) 
      ; FOR RI := FIRST ( Ref ^ ) TO LAST ( Ref ^ ) 
        DO EscapeInt ( LWr , Ref ^ [ RI ] , LIsWide )  
        END (* FOR *) 
      ; Wr . PutText ( LWr , "\"" )
      ; LResult := TextWr . ToText ( LWr )  
      END (* IF *) 
    ; RETURN LResult 
    END ArrImage 

; PROCEDURE TextImage ( TextVal : TEXT ) : TEXT 

  = VAR LIsWide : BOOLEAN 
  ; VAR LI : INTEGER 
  ; VAR LResult : TEXT 
  ; VAR LWr : Wr . T 

  ; BEGIN 
      IF TextVal = NIL 
      THEN LResult := "NIL" 
      ELSE 
        LWr := TextWr . New ( ) 
      ; LIsWide := Text . HasWideChars ( TextVal ) 
      ; IF LIsWide 
        THEN Wr . PutText ( LWr , "W" ) 
        END (* IF *) 
      ; Wr . PutText ( LWr , "\"" ) 
      ; FOR LI := 0 TO Text . Length ( TextVal ) - 1 
        DO EscapeInt 
             ( LWr , ORD ( Text . GetWideChar ( TextVal , LI ) ) , LIsWide )
        END (* FOR *) 
      ; Wr . PutText ( LWr , "\"" )
      ; LResult := TextWr . ToText ( LWr )  
      END (* IF *) 
    ; RETURN LResult 
    END TextImage 

; PROCEDURE TextOrArrImage 
    ( Val : REFANY (* Either TEXT or REF ARRAY OF INTEGER *) ) 
  : TEXT 

  = BEGIN 
      TYPECASE Val 
      OF Text . T ( Txt ) (* NIL is OK here. *) 
      => RETURN TextImage ( Txt ) 
      | REF ARRAY OF INTEGER ( Ref ) 
      => RETURN ArrImage ( Ref ) 
      ELSE RETURN "<Unknown text representation>"
      END (* IF *) 
    END TextOrArrImage 

; PROCEDURE ArrayFromText ( TextVal : TEXT ) : REF ARRAY OF INTEGER 

  = VAR LLen : CARDINAL 
  ; VAR LResult : REF ARRAY OF INTEGER 

  ; BEGIN 
      LLen := Text . Length ( TextVal ) 
    ; LResult := NEW ( REF ARRAY OF INTEGER , LLen ) 
    ; FOR RI := 0 TO LLen - 1 
      DO
        LResult ^ [ RI ] := ORD ( Text . GetWideChar ( TextVal , RI ) ) 
      END (* FOR *) 
    ; RETURN LResult 
    END ArrayFromText 

; PROCEDURE IndepTextEqual 
    ( Got : TEXT ; Exp : REFANY (* Either TEXT or REF ARRAY OF INTEGER *) ) 
  : BOOLEAN 

  = VAR LLen : CARDINAL 
  ; VAR IntGot , IntExp : INTEGER  

  ; BEGIN 
      IF Got = Exp 
      THEN RETURN TRUE 
      ELSIF Got = NIL OR Exp = NIL 
      THEN RETURN FALSE 
      ELSE 
        LLen := Text . Length ( Got ) 
      ; TYPECASE Exp 
        OF NULL => RETURN Got = NIL 

        | Text . T ( ExpText ) 
        => IF LLen # Text . Length ( ExpText )  
          THEN RETURN FALSE 
          ELSE 
            FOR RI := 0 TO LLen - 1 
            DO 
              IntGot := ORD ( Text . GetWideChar ( Got , RI ) )  
            ; IntExp := ORD ( Text . GetWideChar ( ExpText , RI ) )  
            ; IF NOT WcIntsAsExpected ( IntGot , IntExp )   
              THEN RETURN FALSE 
              END (* IF *) 
            END (* FOR *) 
          ; RETURN TRUE 
          END (* IF *) 

        | REF ARRAY OF INTEGER ( Ref ) 
        => IF LLen # NUMBER ( Ref ^ ) 
          THEN RETURN FALSE 
          ELSE 
            FOR RI := 0 TO LLen - 1 
            DO 
              IntGot := ORD ( Text . GetWideChar ( Got , RI ) )  
            ; IntExp := Ref ^ [ RI ]   
            ; IF NOT WcIntsAsExpected ( IntGot , IntExp )   
              THEN RETURN FALSE 
              END (* IF *) 
            END (* FOR *) 
          ; RETURN TRUE 
          END (* IF *) 

        ELSE RETURN FALSE (* Shouldn't happen. *) 
        END (* TYPECASE *) 
      END (* IF *) 
    END IndepTextEqual

; PROCEDURE CheckOneText 
    ( Label , Got , Exp : REFANY ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF Got = NIL 
      THEN IF Exp = NIL  
        THEN  
          Wr . PutText ( LogWrT , Label & " = NIL, as expected." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ELSE 
          Wr . PutText ( LogWrT , Label & " = NIL, but expected " )
        ; Wr . PutText ( LogWrT , TextOrArrImage ( Exp ) )   
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; INC ( GFailureCt )  
        END (* IF *) 
      ELSE IF Exp = NIL  
        THEN  
          Wr . PutText ( LogWrT , Label & " = " )
        ; Wr . PutText ( LogWrT , TextOrArrImage ( Got ) )  
        ; Wr . PutText ( LogWrT , ", but expected NIL." )
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; INC ( GFailureCt )  
        ELSE (* Both non-NIL. *)  
          IF Got = Exp 
          THEN
            Wr . PutText 
              ( LogWrT , Label & "Unexpected reference equality, = " )  
          ; Wr . PutText ( LogWrT , TextOrArrImage ( Got ) )   
          ; Wr . PutText ( LogWrT , Wr . EOL )  
          ; INC ( GFailureCt )  
          ELSE 
            Wr . PutText ( LogWrT , Label & "(" )
          ; TYPECASE Got 
            OF TextLiteral . T ( TLit ) 
            => Wr . PutText ( LogWrT , "Textliteral, not expected,) = " )
            ; INC ( GFailureCt )  
            | Text16.T ( T16 ) 
            => Wr . PutText ( LogWrT , "Text16) = " ) 
            | Text16Short.T ( T16Short ) 
            => Wr . PutText ( LogWrT , "Text16Short) = " ) 
            | Text8CString.T ( T8CString ) 
            => Wr . PutText ( LogWrT , "Text8CString) = " ) 
            | Text8.T ( T8 ) 
            => Wr . PutText ( LogWrT , "Text8) = " ) 
            | Text8Short.T ( T8Short ) 
            => Wr . PutText ( LogWrT , "Text8Short) = " ) 
            | TextCat.T ( TCat ) 
            => Wr . PutText ( LogWrT , "TextCat) = " ) 
            | TextSub.TT ( TSub ) 
            => Wr . PutText ( LogWrT , "TextSub) = " ) 
            ELSE Wr . PutText ( LogWrT , "Text) = " )
            END (* TYPECASE *) 
          ; Wr . PutText ( LogWrT , TextOrArrImage ( Got ) ) 
          ; IF IndepTextEqual ( Got , Exp ) 
            THEN Wr . PutText ( LogWrT , ", as expected." ) 
            ELSE 
              Wr . PutText ( LogWrT , ", but expected " ) 
            ; Wr . PutText ( LogWrT , TextOrArrImage ( Exp ) ) 
            ; INC ( GFailureCt )  
            END (* IF *) 
          ; Wr . PutText ( LogWrT , Wr . EOL )  
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    ; INC ( GTestCt ) 
    END CheckOneText 

; PROCEDURE CheckTexts ( FRefToTexts : RefToTexts ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      CheckOneText ( "Lit1" , FRefToTexts . T1 , WcDep . VArray1 ) 
    ; CheckOneText ( "Lit2" , FRefToTexts . T2 , Lit2 ) 
    ; CheckOneText ( "Lit3" , FRefToTexts . T3 , Lit3 ) 
    ; CheckOneText ( "Lit4" , FRefToTexts . T4 , Lit4 ) 
    ; CheckOneText ( "Lit5" , FRefToTexts . T5 , Lit5 ) 
    ; CheckOneText ( "Lit6" , FRefToTexts . T6 , Lit6 ) 
    ; CheckOneText ( "Lit7a&Lit7b" , FRefToTexts . T7 , Lit7a & Lit7b ) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTexts   

; PROCEDURE CheckTopText ( Got , Exp : REFANY ) 

  = BEGIN 
      WrSep ( ) 
    ; CheckOneText ( "Top level" , Got , Exp ) 
    ; Wr . Flush ( LogWrT ) 
    END CheckTopText 

; PROCEDURE Write ( ) 

  = VAR R : RefT 
  ; VAR VTPA : RefTPA
  ; VAR VTAA : RefTAA
  ; VAR VTOA : RefToA
  ; VAR VTA : RefTA 
  ; VAR VTexts : RefToTexts 
  ; VAR WrT : Wr . T 

  ; <* FATAL Thread . Alerted , Wr . Failure , Pickle . Error *> 
    BEGIN 
      IF DoWrite 
      THEN 
        WrT := IO . OpenWrite ( FileName ) 
      ; IF DoWriteT 
        THEN 
          R := InitT ( ) 
        ; LibDep . PickleWrite( WrT , R , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote record." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteTPA 
        THEN 
          VTPA := InitTPA ( ) 
        ; LibDep . PickleWrite( WrT , VTPA , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote packed array (21) of WIDECHAR." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteTAA 
        THEN 
          VTAA := InitTAA ( ) 
        ; LibDep . PickleWrite( WrT , VTAA , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote packed array (32) of WIDECHAR." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteTA 
        THEN 
          VTA := InitTA ( ) 
        ; LibDep . PickleWrite( WrT , VTA , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote unpacked array of WIDECHAR." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteTOA 
        THEN 
          VTOA := InitTOA ( ) 
        ; LibDep . PickleWrite( WrT , VTOA , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote open array of WIDECHAR." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteRecTexts  
        THEN 
          VTexts := InitTexts ( ) 
        ; LibDep . PickleWrite( WrT , VTexts , Bit16 ) 
        ; Wr . PutText ( LogWrT , "Wrote record of Texts." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; IF DoWriteTopTexts  
        THEN 
          LibDep . PickleWrite( WrT , MakeText16Short ( WcDep . VLit1 ) , Bit16 )
        ; LibDep . PickleWrite( WrT , MakeText16 ( Lit4 ) , Bit16 )
        ; LibDep . PickleWrite( WrT , MakeText8Short ( Lit5 ) , Bit16 )
        ; LibDep . PickleWrite( WrT , MakeText8 ( Lit6 ) , Bit16 )
        ; LibDep . PickleWrite( WrT , MakeTextCat ( Lit7a , Lit7b ) , Bit16 )
        ; Wr . PutText ( LogWrT , "Wrote top level Texts." )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      ; Wr . Close ( WrT ) 
      ; Wr . PutText ( LogWrT , "Wrote " & FileName )  
      ; Wr . PutText ( LogWrT , Wr . EOL )  
      ; Wr . Flush ( LogWrT ) 
      END (* IF *) 
    END Write  

; PROCEDURE WrSep ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      Wr . PutText 
        ( LogWrT , "--------------------------------------------------------" ) 
    ; Wr . PutText ( LogWrT , Wr . EOL )  
    END WrSep 

; PROCEDURE ShowFPs ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF DoShowFPs 
      THEN 
        Wr . PutText ( LogWrT , "Certain fingerprints:" )  
      ; Wr . PutText ( LogWrT , Wr . EOL )  
      ; ShowFP ( "NULL          " , TYPECODE ( NULL ) ) 
      ; ShowFP ( "UNTRACED ROOT " , TYPECODE ( UNTRACED ROOT ) ) 
      ; ShowFP ( "ROOT          " , TYPECODE ( ROOT ) ) 
      ; ShowFP ( "REF WIDECHAR  " , TYPECODE ( REF WIDECHAR ) ) 
   (* ; ShowFP ( "REFANY        " , TYPECODE ( REFANY ) ) *) 
   (* ; ShowFP ( "ADDRESS       " , TYPECODE ( ADDRESS ) ) *) 
      ; ShowFP ( "RefT          " , TYPECODE ( RefT ) ) 
      ; ShowFP ( "TextLiteral.T " , TYPECODE ( TextLiteral.T ) ) 
      ; ShowFP ( "Text16Short.T " , TYPECODE ( Text16Short.T ) ) 
      ; ShowFP ( "Text16.T      " , TYPECODE ( Text16.T ) ) 
      ; ShowFP ( "Text8Short.T  " , TYPECODE ( Text8Short.T ) ) 
      ; ShowFP ( "Text8.T       " , TYPECODE ( Text8.T ) ) 
      ; ShowFP ( "Text8CString.T" , TYPECODE ( Text8CString.T ) ) 
      ; ShowFP ( "TextCat.T     " , TYPECODE ( TextCat.T ) ) 
      ; ShowFP ( "TextSub.TT    " , TYPECODE ( TextSub.TT ) ) 
      ; Wr . Flush ( LogWrT ) 
      END (* IF *) 
    END ShowFPs 

; PROCEDURE Read ( ) 

  = VAR R : REFANY 
  ; VAR RdT : Rd . T 

  ; <* FATAL Thread . Alerted , Wr . Failure , Rd . Failure *> 
    BEGIN 
      IF DoRead 
      THEN 
        GWriteWas16Ct := 0
      ; GWriteWas21Ct := 0 
      ; Wr . PutText ( LogWrT , "Reading " & FileName )  
      ; Wr . PutText ( LogWrT , Wr . EOL )  
      ; Wr . Flush ( LogWrT ) 
      ; RdT := IO . OpenRead ( FileName ) 
      ; IF RdT = NIL 
        THEN 
          Wr . PutText ( LogWrT , "Unable to open " & FileName )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        ELSE
          WHILE NOT Rd . EOF ( RdT ) 
          DO 
            TRY 
              R := Pickle . Read ( RdT ) 
            EXCEPT 
            Pickle . Error ( EMsg ) 
            => Wr . PutText ( LogWrT , "While reading, got Pickle.Error" )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . PutText ( LogWrT , "  ( " )  
            ; Wr . PutText ( LogWrT , EMsg )  
            ; Wr . PutText ( LogWrT , " )" )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 
            ; INC ( GFailureCt )  
            ; INC ( GTestCt )  
            ; RETURN 
            | Rd . EndOfFile 
            => Wr . PutText ( LogWrT , "While reading, got Rd.EndOfFile" )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 
            ; INC ( GFailureCt )  
            ; INC ( GTestCt )  
            ; RETURN 
            END (* EXCEPT *) 
          ; TYPECASE ( R ) 
            OF RefT ( TRefT ) 
            => WrSep ( ) 
            ; CheckT ( TRefT ) 
            ; Wr . PutText ( LogWrT , "Done reading record." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | RefTPA ( TRefTPA ) 
            => WrSep ( ) 
            ; CheckTPA ( TRefTPA )
            ; Wr . PutText ( LogWrT , "Done reading packed array (21) of WIDECHAR." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | RefTAA ( TRefTAA ) 
            => WrSep ( ) 
            ; CheckTAA ( TRefTAA )
            ; Wr . PutText ( LogWrT , "Done reading packed array (32) of WIDECHAR." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | RefTA ( TRefTA ) 
            => WrSep ( ) 
            ; CheckTA ( TRefTA )
            ; Wr . PutText ( LogWrT , "Done reading unpacked array of WIDECHAR." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | RefToA ( TRefToA ) 
            => WrSep ( ) 
            ; CheckTOA ( TRefToA )
            ; Wr . PutText ( LogWrT , "Done reading open array of WIDECHAR." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | RefToTexts ( TRefToTexts ) 
            => WrSep ( ) 
            ; CheckTexts ( TRefToTexts ) 
            ; Wr . PutText ( LogWrT , "Done reading Texts." )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 

            | Text16Short . T => CheckTopText ( R , WcDep . VArray1 ) 
            | Text16 . T => CheckTopText ( R , Lit4 ) 
            | Text8Short . T => CheckTopText ( R , Lit5 ) 
            | Text8 . T => CheckTopText ( R , Lit6 ) 
            | TextCat . T => CheckTopText ( R , Lit7 ) 

            ELSE 
              WrSep ( ) 
            ; Wr . PutText ( LogWrT , "Done reading unknown type object. " )  
            ; Wr . PutText ( LogWrT , Wr . EOL )  
            ; Wr . Flush ( LogWrT ) 
            END (* TYPECASE *)  
          END (* WHILE *) 

        ; INC ( GTestCt ) (* For write-size consistency check. *)  
        ; IF GWriteWas16Ct > 0 AND GWriteWas21Ct > 0  
          THEN 
            WrSep ( ) 
          ; Wr . PutText ( LogWrT , "Got a mixture of " )
          ; Wr . PutText ( LogWrT , Fmt . Int ( GWriteWas16Ct ) )   
          ; Wr . PutText ( LogWrT , "16-bit written and " )  
          ; Wr . PutText ( LogWrT , Fmt . Int ( GWriteWas21Ct ) )   
          ; Wr . PutText ( LogWrT , "21-bit written values for WIDECHARS." )  
          ; Wr . PutText ( LogWrT , Wr . EOL )  
          ; Wr . Flush ( LogWrT )
          ; INC ( GFailureCt )  
          END (* IF *) 

        ; Rd . Close ( RdT ) 
        ; WrSep ( ) 
        ; Wr . PutText ( LogWrT , "Done reading " & FileName )  
        ; Wr . PutText ( LogWrT , Wr . EOL )  
        ; Wr . Flush ( LogWrT ) 
        END (* IF *) 
      END (* IF *) 
    END Read  

; PROCEDURE ReportResults ( ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( LogWrT , "Total Tests:  " )
    ; Wr . PutText ( LogWrT , Fmt . Pad ( Fmt . Int ( GTestCt ) , 5 ) )
    ; Wr . PutText ( LogWrT , Wr . EOL  )
    ; Wr . PutText ( LogWrT , "Failed Tests: " )
    ; Wr . PutText ( LogWrT , Fmt . Pad ( Fmt . Int ( GFailureCt ) , 5 ) )
    ; Wr . PutText ( LogWrT , Wr . EOL  )
    ; Wr . Flush ( LogWrT ) 
    END ReportResults 

; <* FATAL Thread . Alerted , Wr . Failure *> 
  BEGIN 
    WcDep . Init ( ) 
  ; LogWrT := Stdio . stdout 
  ; PWrT := Stdio . stderr 
  ; GTestCt := 0 
  ; GFailureCt := 0 
  ; GetParams ( ) 
  ; IF DoDisplayHelp 
    THEN 
      DisplayVersion ( ) 
    ; DisplayHelp ( ) 
    ELSIF DoDisplayVersion 
    THEN DisplayVersion ( ) 
    ELSE 
      ShowFPs ( ) 
    ; ShowAllRefTypes ( ) 
    ; Write ( )  
    ; Read ( ) 
    ; ReportResults ( ) 
    ; Wr . Flush ( LogWrT ) 
    END (* IF *) 
  END PickleTestWideChar  
. 

