MODULE SmallSet

; IMPORT Fmt 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal

; PROCEDURE Image ( Val : MainType ) : TEXT 
  = VAR Result : TEXT
  ; VAR First , Last : CARDINAL
  ; VAR DisplayedCt : INTEGER 
  ; BEGIN
      DisplayedCt := 0 
    ; Result := "{"
    ; First := FIRST ( Base ) 
    ; Last := LAST ( Base )
    ; IF Last >= First
      THEN
        IF First IN Val 
        THEN
          Result := Result & Fmt . Int ( First )
        ; INC ( DisplayedCt ) 
        END (*IF*) 
      ; FOR RE := First + 1 TO Last 
        DO IF RE IN Val  
          THEN
            IF DisplayedCt > 0 THEN Result := Result & "," END
          ; Result := Result & Fmt . Int ( RE ) 
          ; INC ( DisplayedCt ) 
          END
        END
      END 
    ; RETURN Result & "}" 
    END Image 
    
 ; BEGIN
   END SmallSet
 .
