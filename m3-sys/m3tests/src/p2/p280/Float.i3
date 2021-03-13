INTERFACE Float 

; IMPORT Word 

(* Parameterizing values: *)

(* Used in instantiation of Modes: *)
; CONST Label = "Float"
; TYPE MainType = REAL 
; CONST BitCt = BITSIZE ( MainType )
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BitCt FOR MainType
; CONST DoPacked = FALSE  
; CONST IsArray = FALSE

; VAR Val1 := 1.277654E2
; CONST NondesigVal1 = 1.277654E2
; VAR Val2 := 7.20987645E-3

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

(* Internal: *)

; CONST PadBits = BITSIZE ( MainType )
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 

; END Float
.
