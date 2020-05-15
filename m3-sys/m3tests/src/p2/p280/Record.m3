MODULE Record

; IMPORT Fmt 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal

; PROCEDURE Image ( Val : MainType ) : TEXT 
  = BEGIN
      RETURN
        "{" & Fmt . Char ( Val . C1 ) & "," & Fmt . Char ( Val . C2 ) & "}"  
    END Image 
    
 ; BEGIN
   END Record
 .
