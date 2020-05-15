INTERFACE Enum
(* Including subranges. *)

; IMPORT Word 

(* Used in instantiation of Modes: *)
; CONST Label = "Enumeration"
; CONST Lo = 0
; CONST Hi = 16_3FFFFF
; TYPE MainType = [ Lo .. Hi ] 
; CONST BitCt = 23
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST PadBits = 4
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 
; CONST DoPacked = TRUE 
; CONST IsArray = FALSE

; VAR Val1 := 17
; CONST NondesigVal1 = 17 
; VAR Val2 := 9865

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

; END Enum
.
