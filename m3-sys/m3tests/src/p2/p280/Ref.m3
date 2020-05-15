UNSAFE MODULE Ref 

; IMPORT Fmt 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal

; PROCEDURE RefImage ( Val : MainType ) : TEXT
  = BEGIN
      RETURN "16_" & Fmt . Int ( LOOPHOLE ( Val , INTEGER ) , base := 16 ) 
    END RefImage 

; PROCEDURE Image ( Val : MainType ) : TEXT 
  = BEGIN
      IF Val = NIL
      THEN RETURN "NIL"
      ELSE RETURN RefImage ( Val ) & " ^ = " & Fmt . Int ( Val ^ )
      END 
    END Image 
    
; BEGIN
    Val1 := NEW ( MainType )
  ; Val1 ^ := 1776 
  ; Val2 := NEW ( MainType )
  ; Val2 ^ := 1945  
  ; NondesigVal1 := Val1 
  END Ref 
.
