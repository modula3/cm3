INTERFACE Array

; IMPORT Word 

(* Used in instantiation of Modes: *)
; CONST Label = "Array"
; TYPE EltType = INTEGER 
; TYPE MainType = ARRAY [ 0 .. 2 ] OF EltType 
; CONST BitCt = BITSIZE ( MainType ) 
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST BitsPerWord = BITSIZE ( Word . T ) 
; CONST PadBits = BitsPerWord 
; CONST PadLast = Word . RightShift ( - 1 , BitsPerWord - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 
; CONST DoPacked = TRUE 
; CONST IsArray = TRUE 

; VAR Val1 := MainType { 9 , 11 , 13 }
; CONST NondesigVal1 = MainType { 9 , 11 , 13 }
; VAR Val2 := MainType { 15 , 17 , 19 }

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

; END Array
.
