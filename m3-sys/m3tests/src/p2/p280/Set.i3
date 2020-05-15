INTERFACE Set

; IMPORT Word 

(* Parameterizing values: *)

(* Used in instantiation of Modes: *)
; CONST Label = "Set"
; CONST BitsPerWord = BITSIZE ( Word . T ) 
; TYPE Base = [ 0 .. 3 * BitsPerWord - 1 ]
; TYPE MainType = SET OF Base
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST DoPacked = TRUE 
; CONST IsArray = FALSE

; VAR Val1 := MainType { 3 , BitsPerWord + 5 , BitCt - 1 }
; CONST NondesigVal1 = MainType { 3 , BitsPerWord + 5 , BitCt - 1 }
; VAR Val2 := MainType { 7 , BitsPerWord + 9 , BitCt - 1 }

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

(* Internal: *)
; CONST BitCt = BITSIZE ( MainType ) 
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8

; CONST PadBits = BitsPerWord 
; CONST PadLast = Word . RightShift ( - 1 , BitsPerWord - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 

; END Set
.
