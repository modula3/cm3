MODULE SmallArray

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
        "{" & Fmt . Char ( Val [ 0 ]  )
        & "," & Fmt . Char ( Val [ 1 ] )
        & "," & Fmt . Char ( Val [ 2 ] )
        & "}"  
    END Image 
    
 ; BEGIN
   END SmallArray
 .
