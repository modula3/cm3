UNSAFE MODULE TextUtils 

; IMPORT Fmt , Math 
; IMPORT Text , TextClass , TextLiteral 
; IMPORT Text8Short , Text8 , Text16Short , Text16, TextSub , TextCat 

; PROCEDURE Wide ( T : TEXT ) : BOOLEAN 
  = BEGIN 
      TYPECASE T <* NOWARN *> 

      OF NULL => RETURN TRUE 
      | Text8Short . T 
      , Text8 . T 
      => RETURN FALSE 
      | Text16Short . T 
      , Text16 . T 
      => RETURN TRUE 
      | TextLiteral . T ( TC ) 
      => RETURN TC . cnt < 0 
      | TextSub . TT ( TC )
      => RETURN Wide ( TC . base )  
      | TextCat . T ( TC ) 
      => RETURN TC . a_or_b_wide 
      END (* TYPECASE *) 
    END Wide 

; CONST BytesPerWC = BYTESIZE ( WIDECHAR ) 

; PROCEDURE VerifyText ( T : TEXT ) RAISES { BadInvariant } 

  = BEGIN 
      TYPECASE T <* NOWARN *> 

      OF NULL => RAISE BadInvariant ( "NIL" ) 

      | TextLiteral . T ( TL ) 
      => IF TL . cnt >= 0 
         THEN 
           IF TL . cnt > TextLiteral . MaxBytes - 2 
           THEN
             RAISE BadInvariant 
               ( "TextLiteral,8, cnt = " & Fmt . Int ( TL . cnt  ) 
                 & ", too large."  
               ) 
           ELSIF TL . buf [ TL . cnt ] # 0  
           THEN
             RAISE BadInvariant 
               ( "TextLiteral,8, cnt = " & Fmt . Int ( TL . cnt  ) 
                 & ", no terminating null."  
               ) 
           END (* IF *) 
         ELSE
           IF - TL . cnt * BytesPerWC > TextLiteral . MaxBytes - BytesPerWC 
           THEN
             RAISE BadInvariant 
               ( "TextLiteral,16, cnt = " & Fmt . Int ( TL . cnt  ) 
                 & ", too large."  
               ) 
           ELSIF TL . buf [ - TL . cnt * BytesPerWC ] # 0  
              OR TL . buf [ - TL . cnt * BytesPerWC + 1 ] # 0  
              OR BytesPerWC = 4 
                 AND ( TL . buf [ - TL . cnt * BytesPerWC + 2 ] # 0
                       OR TL . buf [ - TL . cnt * BytesPerWC + 3 ] # 0
                     ) 
           THEN
             RAISE BadInvariant 
               ( "TextLiteral,16, cnt = " & Fmt . Int ( TL . cnt  ) 
                 & ", no terminating null."  
               ) 
           END (* IF *) 
         END (* IF *)            
 
      | Text8Short . T ( TC ) 
      => IF TC . len > Text8Short . MaxLength 
        THEN 
          RAISE BadInvariant 
            ( "Text8Short, len = " & Fmt . Int ( TC . len ) 
              & ", should not exceed " & Fmt . Int ( Text8Short . MaxLength ) 
              & "." 
            ) 
        ELSIF TC . contents [ TC . len ] # '\000' 
        THEN 
          RAISE BadInvariant 
            ( "Text8Short, char at " & Fmt . Int ( TC . len ) 
              & " should be null, but is VAL ( " 
              & Fmt . Int ( ORD ( TC . contents [ TC . len ] ) ) 
              & ", CHAR )." 
            ) 
        END (* IF *) 

      | Text16Short . T ( TC ) 
      => IF TC . len > Text16Short . MaxLength 
        THEN 
          RAISE BadInvariant 
            ( "Text16Short, len = " & Fmt . Int ( TC . len ) 
              & ", should not exceed " & Fmt . Int ( Text16Short . MaxLength ) 
              & "." 
            ) 
        ELSIF TC . contents [ TC . len ] # VAL ( 0 , WIDECHAR )  
        THEN 
          RAISE BadInvariant 
            ( "Text16Short, wide char at " & Fmt . Int ( TC . len ) 
              & " should be null, but is VAL ( " 
              & Fmt . Int ( ORD ( TC . contents [ TC . len ] ) ) 
              & ", WIDECHAR )." 
            ) 
        END (* IF *) 

      | Text8 . T ( TC ) 
      => IF TC . contents = NIL 
        THEN 
          RAISE BadInvariant ( "Text8, contents is NIL" ) 
        ELSE
          VAR Last := LAST ( TC . contents ^ ) 
        ; BEGIN 
            IF TC . contents [ Last ] # '\000' 
            THEN 
              RAISE BadInvariant 
                ( "Text8, char at " & Fmt . Int ( Last ) 
                  & " should be null, but is VAL ( " 
                  & Fmt . Int ( ORD ( TC . contents [ Last ] ) ) 
                  & ", CHAR )." 
                ) 
            END (* IF *) 
          END (* Block *) 
        END (* IF *) 

      | Text16 . T ( TC ) 
      => IF TC . contents = NIL 
        THEN 
          RAISE BadInvariant ( "Text16, contents is NIL" ) 
        ELSE
          VAR Last := LAST ( TC . contents ^ ) 
        ; BEGIN 
            IF TC . contents [ Last ] # VAL ( 0 , WIDECHAR ) 
            THEN 
              RAISE BadInvariant 
                ( "Text16, wide char at " & Fmt . Int ( Last ) 
                  & " should be null, but is VAL ( " 
                  & Fmt . Int ( ORD ( TC . contents [ Last ] ) ) 
                  & ", WIDECHAR )." 
                ) 
            END (* IF *) 
          END (* Block *) 
        END (* IF *) 

      | TextSub . TT ( TC ) 
      => IF TC . base = NIL 
        THEN 
          RAISE BadInvariant ( "TextSub, base is NIL" ) 
        ELSE
          VerifyText ( TC . base )
        ; VAR LBaseLength := Text . Length ( TC . base ) 
        ; BEGIN 
            IF TC . start >= LBaseLength 
            THEN 
              RAISE BadInvariant 
                ( "TextSub, start = " & Fmt . Int ( TC . start ) 
                  & ", which is off the end of base, whose length is " 
                  & Fmt . Int ( LBaseLength ) 
                  & "." 
                ) 
            ELSIF TC . start + TC . len > LBaseLength 
            THEN 
              RAISE BadInvariant 
                ( "TextSub, start = " & Fmt . Int ( TC . start ) 
                  & ", and len = " & Fmt . Int ( TC . len ) 
                  & ", which is off the end of base, whose length is " 
                  & Fmt . Int ( LBaseLength ) 
                  & "." 
                ) 
            END (* IF *) 
          END (* Block *) 
        END (* IF *) 

      | TextCat . T ( TC ) 
      => IF TC . a = NIL 
        THEN 
          RAISE BadInvariant ( "TextCat, a is NIL" ) 
        ELSIF TC . b = NIL 
        THEN 
          RAISE BadInvariant ( "TextCat, b is NIL" ) 
        ELSE
          VerifyText ( TC . a ) 
        ; VerifyText ( TC . b ) 
        ; VAR LALen := Text . Length ( TC . a ) 
        ; VAR LBLen := Text . Length ( TC . b ) 
        ; VAR LAWide := Wide ( TC . a )  
        ; VAR LBWide := Wide ( TC . b )  
        ; BEGIN 
            IF LALen # TC . a_len 
            THEN 
              RAISE BadInvariant 
                ( "TextCat, a_len = " & Fmt . Int ( TC . a_len ) 
                  & ", but Text . Length ( a ) = " 
                  & Fmt . Int ( LALen ) 
                  & "." 
                ) 
            ELSIF LBLen # TC . b_len 
            THEN 
              RAISE BadInvariant 
                ( "TextCat, b_len = " & Fmt . Int ( TC . b_len ) 
                  & ", but Text . Length ( b ) = " 
                  & Fmt . Int ( LBLen ) 
                  & "." 
                ) 
            ELSIF ( LAWide OR LBWide ) # TC . a_or_b_wide 
            THEN 
              RAISE BadInvariant 
                ( "TextCat, a_or_b_wide = " & Fmt . Bool ( TC . a_or_b_wide ) 
                  & ", but Wide ( a ) = " 
                  & Fmt . Bool ( LAWide ) 
                  & ", and Wide ( b ) = " 
                  & Fmt . Bool ( LBWide ) 
                  & "." 
                ) 
            END (* IF *) 
          END (* Block *) 
        END (* IF *) 
      END (* TYPECASE *) 
    END VerifyText 

; PROCEDURE Log2 ( X : INTEGER ) : LONGREAL 

  = VAR XR : LONGREAL
  ; VAR Result : LONGREAL 

  ; BEGIN 
      XR := FLOAT ( X , LONGREAL ) 
    ; Result 
        := Math . log10 ( XR ) / Math . log10 ( 2.0D0 ) 
    ; RETURN Result 
    END Log2 

; PROCEDURE CompImbalance ( MaxDepth : CARDINAL ; LeafCt : CARDINAL ) : REAL 

  = VAR MinPossibleDepth , Imbalance : LONGREAL  

  ; BEGIN
      MinPossibleDepth := Math . ceil ( Log2 ( LeafCt ) ) + 1.0D0    
    ; Imbalance := FLOAT ( MaxDepth , LONGREAL ) / MinPossibleDepth   
    ; RETURN FLOAT ( Imbalance , REAL ) 
    END CompImbalance 

; PROCEDURE MeasureText 
    ( T : TEXT 
    ; VAR MaxDepth : INTEGER  
    ; VAR LeafCt : INTEGER
    ; VAR Imbalance : REAL 
    ; VAR LengthDiff : INTEGER 
    ) 

  = PROCEDURE Recurse 
      ( RT : TEXT 
      ; VAR RMaxDepth : INTEGER 
      ; VAR RLeafCt : INTEGER 
      ; VAR RImbalance : REAL 
      ; VAR RLengthDiff : INTEGER 
      ) 

    = BEGIN 
        TYPECASE RT 

        OF NULL 
        => RMaxDepth := 0 
        ; RLeafCt := 0 
        ; RLengthDiff := 0
        ; RImbalance := 1.0 

        | TextSub . TT ( TS ) 
        => VAR MaxDepthDown : INTEGER 
        ; BEGIN (* Block *)
            Recurse 
              ( TS . base , MaxDepthDown , RLeafCt , RImbalance , RLengthDiff )
          ; RMaxDepth := MaxDepthDown + 1 
          END (* Block *)

        | TextCat . T ( TC ) 
        => VAR MaxDepthA , MaxDepthB : INTEGER 
        ; VAR LeafCtA , LeafCtB : INTEGER 
        ; VAR LengthDiffA , LengthDiffB : INTEGER 
        ; VAR ImbalanceA , ImbalanceB : REAL 
        ; VAR LengthA := Text . Length ( TC . a ) 
        ; VAR LengthB := Text . Length ( TC . b ) 
        ; BEGIN (* Block *)
            Recurse 
              ( TC . a , MaxDepthA , LeafCtA , ImbalanceA , LengthDiffA ) 
          ; Recurse 
              ( TC . b , MaxDepthB , LeafCtB , ImbalanceB , LengthDiffB ) 
          ; RMaxDepth := 1 + MAX ( MaxDepthA , MaxDepthB ) 
          ; RLeafCt := LeafCtA + LeafCtB 
          ; RImbalance := CompImbalance ( RMaxDepth , RLeafCt )  
          ; RLengthDiff 
              := LengthDiffA + LengthDiffB + ABS ( LengthA - LengthB )
          END (* Block *) 

        ELSE 
          RMaxDepth := 1 
        ; RLeafCt := 1 
        ; RLengthDiff := 0
        ; RImbalance := 1.0 
        END (* TYPECASE *) 
      END Recurse 

  ; BEGIN  
      Recurse ( T , MaxDepth , LeafCt , Imbalance , LengthDiff ) 
    END MeasureText 
; BEGIN 
  END TextUtils 
. 
