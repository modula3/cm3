INTERFACE Proc 

; IMPORT Word 

(* Parameterizing values: *)

(* Used in instantiation of Modes: *)
; CONST Label = "Procedure"
; TYPE ProcType = PROCEDURE ( F : INTEGER ) : TEXT 
; TYPE MainType = ProcType  
; CONST BitCt = BITSIZE ( MainType )
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BitCt FOR MainType
; CONST DoPacked = FALSE  
; CONST IsArray = FALSE

; PROCEDURE Val1 ( F : INTEGER ) : TEXT 
; PROCEDURE Val2 ( F : INTEGER ) : TEXT 
; CONST NondesigVal1 = Val1 

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

(* Internal: *)

; CONST PadBits = BITSIZE ( MainType )
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 

; END Proc 
.
