INTERFACE Ref 

; IMPORT Word 

(* Parameterizing values: *)

(* Used in instantiation of Modes: *)
; CONST Label = "Ref"
; TYPE Referent = INTEGER
; TYPE MainType = REF Referent
; CONST BitCt = BITSIZE ( MainType )
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BitCt FOR MainType
; CONST DoPacked = FALSE  
; CONST IsArray = FALSE

; VAR Val1 , Val2 , NondesigVal1 : MainType 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

(* Internal: *)

; CONST PadBits = BITSIZE ( MainType )
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 

; END Ref
.
