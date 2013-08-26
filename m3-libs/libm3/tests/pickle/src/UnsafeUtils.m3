UNSAFE MODULE UnsafeUtils 

; IMPORT TextLiteral 

; TYPE WWchpTyp = UNTRACED REF WIDECHAR 

; PROCEDURE PatchTextLit ( T : TextLiteral . T ) 
  (* Very sleazy. We are mutating a sormally immutable text literal.  Probably
     all occurences of this literal value in its compilation will be using the
     mutated copy.  
     If WIDECHAR is unicode-sized, patch any occurences
     of the Unicode substition code U+FFFD to U+10FFFF
  *)   

  = VAR LCt : INTEGER 

  ; BEGIN 
      IF ORD ( LAST ( WIDECHAR ) ) > 16_FFFF 
      THEN (* We are running on a Unicode-sized WIDECHAR system. *) 
        IF T . cnt < 0 
        THEN (* This is a wide text literal. *) 
          LCt := - T . cnt (* Count of WIDECHARS. *) 
        ; FOR RI := 0 TO LCt - 1 
          DO 
           WITH WWchp 
                = LOOPHOLE 
                    ( ADR ( T . buf [ RI * ADRSIZE ( WIDECHAR ) ] ) , WWchpTyp )
           DO 
             IF ORD ( WWchp ^ ) = 16_FFFD 
                               (* ^Unicode substitution code point. *)  
             THEN 
               WWchp^ := VAL ( 16_10FFFF , WIDECHAR )
                      (* ^This will get a runtime range error if 
                          LAST(WIDECHAR) < 16_10FFFF, but we won't get here 
                          in that case. *) 
(* We can't do this on AMD64_LINUX, because the text literals are stored in
   readonly memory. *) 
             END (* IF *)      
           END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      END (* IF *) 
    END PatchTextLit 

; BEGIN 
  END UnsafeUtils
. 

