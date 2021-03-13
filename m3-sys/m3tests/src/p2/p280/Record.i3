INTERFACE Record
(* Including subranges. *)

; IMPORT Word 

(* Used in instantiation of Modes: *)
; CONST Label = "Record"
; TYPE MainType = RECORD C1 , C2 : CHAR END 
; CONST BitCt = 23
; CONST BytesBitCt = ( ( BitCt + 1 ) DIV 8 ) * 8
; TYPE ShortType = BITS BitCt FOR MainType  
; TYPE BytesType = BITS BytesBitCt FOR MainType
; CONST PadBits = 3
; CONST PadLast = Word . RightShift ( - 1 , BITSIZE ( Word . T ) - PadBits + 1 )
; CONST PadFirst = - PadLast - 1 
; TYPE PadType = BITS PadBits FOR [ PadFirst .. PadLast ] 
; CONST DoPacked = TRUE 
; CONST IsArray = FALSE

; VAR Val1 := MainType { '3' , 'H' }
; CONST NondesigVal1 = MainType { '3' , 'H' }
; VAR Val2 := MainType { '%' , '_' }

; PROCEDURE Equal ( Left , Right : MainType ) : BOOLEAN 

; PROCEDURE Image ( Val : MainType ) : TEXT 

; END Record
.
