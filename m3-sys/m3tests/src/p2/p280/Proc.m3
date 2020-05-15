MODULE Proc 

; PROCEDURE Val1 ( <* UNUSED *> F : INTEGER ) : TEXT
  = BEGIN
      RETURN "Val1"
    END Val1 
    
; PROCEDURE Val2 ( <* UNUSED *> F : INTEGER ) : TEXT 
  = BEGIN
      RETURN "Val2"
    END Val2 
    
; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal

; PROCEDURE Image ( Val : MainType ) : TEXT 
  = BEGIN
      IF Val = NIL
      THEN RETURN "NIL"
      ELSE RETURN Val ( 17 )
      END 
    END Image 
    
; BEGIN
  END Proc 
.
