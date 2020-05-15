MODULE Enum

; IMPORT Fmt 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal

; PROCEDURE Image ( Val : MainType ) : TEXT 
  = BEGIN
      RETURN Fmt . Int ( Val ) 
    END Image 
    
 ; BEGIN
   END Enum
 .
