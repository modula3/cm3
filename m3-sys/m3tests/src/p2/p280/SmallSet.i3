INTERFACE SmallSet

; IMPORT Word 

(* Parameterizing values: *)

(* Used in instantiation of Modes: *)
; CONST Label = "Small Set"
; TYPE MainType = SET OF Base
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST DoPacked = TRUE 
; CONST IsArray = FALSE

; VAR Val1 := MainType { 0 , 5 , BitCt - 1 }
; CONST NondesigVal1 = MainType { 0 , 5 , BitCt - 1 }
; VAR Val2 := MainType { 0 , 7 , BitCt - 1 }

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

(* Internal: *)
; CONST BitCt = 23
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8
; TYPE Base = [ 0 .. BitCt - 1 ]

; CONST PadBits = 4
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 

; END SmallSet
.
