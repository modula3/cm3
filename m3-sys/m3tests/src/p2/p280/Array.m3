MODULE Array

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
        "{" & Fmt . Int ( Val [ 0 ]  )
        & "," & Fmt . Int ( Val [ 1 ] )
        & "," & Fmt . Int ( Val [ 2 ] )
        & "}"  
    END Image 
    
 ; BEGIN
   END Array
 .
